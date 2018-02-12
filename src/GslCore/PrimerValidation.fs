module primerValidation

open commonTypes
open Amyris.Bio.utils
open Amyris.Bio.biolib
open Amyris.Dna

let checkAnnotation (p:Primer) errorDesc =
    for a in p.annotation do
        if a.il < 0 then
            failwithf "primer annotation il (%d) < 0 p=%s %s"
                a.il (p.Primer.str) errorDesc
        if a.ir >= p.tail.Length + p.body.Length then
            failwithf "primer annotation ir (%d) off end len=%d %s"
                a.ir  (p.tail.Length+p.body.Length) errorDesc
        if a.il > a.ir && p.tail.Length > 0 then 
            failwithf "primer annotation il(%d) > ir(%d) %s" a.il a.ir errorDesc
        match a.iType with
            | ANNEAL when a.ir-a.il+1 < 12 ->
                failwithf "annealing region of a primer is less than 12 bases il=%d ir=%d %s"
                    a.il a.ir errorDesc
            | AMP when a.ir-a.il+1 < 12 ->
                failwithf "amplification region of a primer is less than 12 bases il=%d ir=%d %s"
                    a.il a.ir errorDesc
            | _ -> () // fine

let checkPrimers (primers : DivergedPrimerPair list list) =
    for pList in primers do
        for primer in pList do
            match primer with
            | DPP(dpp) ->
                checkAnnotation dpp.fwd dpp.name
                checkAnnotation dpp.rev dpp.name
                match dpp.fwd.Interval DNAIntervalType.ANNEAL, dpp.rev.Interval DNAIntervalType.ANNEAL with
                | Some(f), Some(r) ->
                    // Annealing primers fwd and reverse
                    if f.ir-f.il <> r.ir-r.il then
                        failwithf "primer annotation issue annealing regions different lengths il1=%d ir1=%d il2=%d ir2=%d"
                            f.il f.ir r.il r.ir
                    let s1 = dpp.fwd.Primer.[f.il..f.ir]
                    let s2 = dpp.rev.Primer.[r.il..r.ir]
                    let s2' = s2.RevComp()

                    if s1 <> s2' then
                        failwithf
                            "primer annotation anneal region fails antiparallel test\nfwd  =%O\nrev  =%O\nrcrev=%O\nname=%s\n"
                            s1 s2 s2' dpp.name
                | None, Some(x) ->
                    failwithf "primer annotation single anneal region rev %d-%d %O"
                        x.il x.ir dpp.rev.Primer
                | Some(x), None ->
                    failwithf "primer annotation single anneal region fwd %d-%d %O"
                        x.il x.ir dpp.fwd.Primer
                | None, None -> () // fine
            | GAP -> ()

let checkPrimersVAssembly (pa:(DivergedPrimerPair list*DnaAssembly) list) =
    for pList, assembly in pa do

        let assemblySeq = assembly.Sequence()

        for primer in pList do
            match primer with
            | DPP(dpp) ->
                // Ensure assembly contains primer
                let fwd = dpp.fwd.Primer
                if not (assemblySeq.Contains(fwd)) then
                    failwithf
                        "fwd primer validation failure.  Primer %O\ntail=%O\nhead=%O\n does not occur in assembly %s\n%s"
                        fwd
                        dpp.fwd.tail
                        dpp.fwd.body
                        assembly.name
                        (assemblySeq.arr |> format60)

                let rev = dpp.rev.Primer.RevComp()
                if not (assemblySeq.Contains(rev)) then
                    failwithf
                        "rev primer validation failure.  Primer %O\ntail=%O\nbody=%O\n does not occur in assembly %s\n%s"
                        rev
                        dpp.rev.tail
                        dpp.rev.body
                        assembly.name
                        (assemblySeq.arr |> format60)
                ()
            | GAP -> ()
        
        let lastN N (c: Dna) = c.[c.Length-1-N |> max 0..c.Length-1]

        /// More stringent check that some reasonable primer tail binds to the template DNA sequences
        let templateSeq =
            assembly.dnaParts
            |> List.map (fun slice -> 
                match slice.template with 
                | None when slice.sliceType = SliceType.LINKER -> [|'n';'n'|]
                | None -> [|'N';'N'|]
                | Some(x) ->
                    Array.concat [ [|'N'|] ; x.arr ; [|'N'|] ]) 
            |> Array.concat
            |> fun s -> Dna(s, false, AllowAmbiguousBases)

        let templateSeqRC = templateSeq.RevComp()

        for primer in pList do
            match primer with
            | DPP(dpp) ->
                let fwd = dpp.fwd.body |> lastN 10 
                let rev = dpp.rev.body |> lastN 10 |> fun d -> d.RevComp() 

                let ff = templateSeq.Contains fwd
                let fr = templateSeqRC.Contains fwd
                let rf = templateSeq.Contains rev
                let rr = templateSeqRC.Contains rev

                // Ensure assembly contains primer
                if not (ff || fr) then
                    failwithf
                        "fwd XXX primer validation failure.  Primerlast10 %O\ntail=%O\nbody=%O\n does not occur fwd or rc in template %s\n>template\n%s\n>assembly\n%s" 
                        fwd 
                        dpp.fwd.tail 
                        dpp.fwd.body 
                        assembly.name 
                        (templateSeq.arr |> format60)
                        (assemblySeq.arr |> format60)
                if not (rf || rr) then
                    failwithf
                        "rev primer validation failure.  Primer %O\ntail=%O\nbody=%O\n does not occur fwd or rc in template %s\n>template\n%s\n>assembly\n%s" 
                        rev 
                        dpp.rev.tail
                        dpp.rev.body
                        assembly.name
                        (templateSeq.arr |> format60)
                        (assemblySeq.arr |> format60)
            | GAP -> ()
