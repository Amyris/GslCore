/// IO routine for emitting simple primer details
module primerDump
open System.IO
open commonTypes
open Amyris.Bio.utils
open Amyris.Bio.biolib
open System
open Amyris.Dna
open utils

/// Dump out all the primers/primerparts to define the construct
let simplePrimerDump (file:string) (primers:DivergedPrimerPair list list) (assemblies:DnaAssembly list) =
    // User wants primers now
    if file <> "-" then printfn "Writing primers to %s" file
    let outF = if file = "-" then None else Some(new StreamWriter(file))
    // use outF = new StreamWriter(file)

    let w (s:string) = match outF with
                            | None -> stdout.WriteLine(s)
                            | Some(x) -> x.WriteLine(s)

    let dumpOne i (primerList:DivergedPrimerPair list,assembly:DnaAssembly) =
        //name the primer based on the part in binds to
        let primerInfo isFwd (primer:Primer) =
            //find the AMP part of the primer that binds to the template.
            let ampBody = 
                match primer.Interval DNAIntervalType.AMP with 
                    | Some(i) -> primer.Primer.[i.il..i.ir]
                    | None -> primer.body
            //search using either the ampBody or reverse complement of it depending on direction of primer
            let searchBody =  if isFwd then ampBody else ampBody.RevComp()
            let containsPrimer (part:DNASlice) = 
                part.dna.Contains searchBody
            let bindingPart = List.tryFind containsPrimer assembly.dnaParts
            let name =
                match bindingPart with
                | Some value -> 
                    if value.sliceName <> "" then
                        value.sliceName
                    else if value.description <> "" then
                        value.description
                    else (ambId value.id)
                | None -> ""
            if primer.Primer.Length > 0 then 
                let cols = seq {
                    yield (sprintf "\"%s_%s\"" 
                            name
                            (if isFwd then "fwd" else "rev"))
                    yield (if isFwd then "fwd" else "rev")
                    yield (primer.Primer.str)
                    yield (primer.tail.str)
                    yield (primer.body.str)
                    //calculate tm for given primer parts
                    let tm (a:char array) = Amyris.Bio.primercore.temp assembly.designParams.pp a a.Length |> fun t -> sprintf "%3.1f" (t*1.0/(1.0<Amyris.Bio.primercore.C>))
                    //emit individual primer parts and tm
                    yield ( match primer.Interval DNAIntervalType.ANNEAL with
                            | Some(i) -> primer.Primer.[i.il..i.ir].str
                            | None -> "")
                    yield ( match primer.Interval DNAIntervalType.SANDWICH with
                            | Some(i) -> primer.Primer.[i.il..i.ir].str
                            | None -> "")
                    yield ( match primer.Interval DNAIntervalType.AMP with
                            | Some(i) -> primer.Primer.[i.il..i.ir].str
                            | None -> "")
                    yield ( match primer.Interval DNAIntervalType.ANNEAL with
                            | Some(i) -> primer.Primer.[i.il..i.ir].arr |> tm
                            | None -> "")
                    yield ( match primer.Interval DNAIntervalType.AMP with
                            | Some(i) -> primer.Primer.[i.il..i.ir].arr |> tm
                            | None -> "")
                }
                String.Join("\t",cols) |> w

        for dpp in primerList do
            match dpp with
                | GAP 
                | SANDWICHGAP -> () // Don't need to emit these
                | DPP(dpp) -> 
                    primerInfo true dpp.fwd
                    primerInfo false dpp.rev

    w(String.Join("\t",
                                seq {
                                        yield "name"
                                        yield "direction"
                                        yield "sequence"
                                        yield "tail"
                                        yield "body"
                                        yield "anneal"
                                        yield "sandwich"
                                        yield "amp"
                                        yield "annealTm"
                                        yield "ampTm"
                                }
    ))

    List.zip primers assemblies |> List.iteri (dumpOne)
    match outF with
        | Some(outF) -> outF.Close()
        | None -> ()


