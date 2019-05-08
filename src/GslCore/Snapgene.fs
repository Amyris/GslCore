module snapgene

open System.IO
open System
open commonTypes
open constants
open pragmaTypes
open Amyris.Bio.utils
open utils
open Amyris.Dna
open Genbank
        
let topologyToString : Topology -> string =
    function
    | Linear -> "linear"
    | Circular -> "circular"
        
/// Emit Snapgene (genbank) format
///  outDir : string   tag: string  prefix for files  assemblies : List of AssemblyOut
let dumpSnapgene (outDir:string) (tag:string) (assemblies : DnaAssembly list) (primers:DivergedPrimerPair list list option) =
    // pair up the primer list (if they exist) with the matching assembly
    let assemWithPrimers =  
        match primers with
        | Some p -> p
        | None -> List.init assemblies.Length (fun _ -> [])
        |> List.zip assemblies

    for (a,primers) in assemWithPrimers do

        let path = sprintf "%s.%d.dna" tag 
                    (match a.id with 
                        | None -> failwithf "unassigned assembly id in %s" a.name
                        | Some(i) -> i 
                    )
                    |> opj outDir
        printf "Writing snapgene output to dir=%s tag=%s path=%s\n" outDir tag path


        use outF = new StreamWriter(path)
        let w (s:string) = outF.Write(s)
        
        // "Exported" // snapgene fails to color anything if it's named anything else locusName 
        let locusName = sprintf "Exported GSL %s" tag
        let totLength = a.dnaParts |> List.map (fun p -> p.dna.Length) |> Seq.sum
        let now = DateTime.Now
        let topology = a.topology |> topologyToString
        (* // constraints on header format per SnapGene direct correspandance

        1) LOCUS must contain "Exported".
        2) Last reference TITLE must be "Direct Submission".
        3) Last reference JOURNAL must contain "SnapGene".
            (In the past we did require it contain "Exported from SnapGene" or "Exported from SnapGene Viewer" 
            but we recently relaxed that requirement. The change is in version 4.1.)
        *)

        sprintf "LOCUS       %-22s %d bp ds-DNA     %s   SYN %2d-%s-%d
DEFINITION  .
ACCESSION   .
VERSION     .
KEYWORDS    .
SOURCE      synthetic DNA construct
  ORGANISM  synthetic DNA construct
COMMENT
REFERENCE   1  (bases 1 to %d)
  AUTHORS   .
  TITLE     Direct Submission
  JOURNAL   Exported %s %2d, %d from GSL SnapGene generator
            http://pubs.acs.org/doi/abs/10.1021/acssynbio.5b00194
            http://www.snapgene.com
FEATURES             Location/Qualifiers
     source          1..%d
                     /organism=\"synthetic DNA construct\"
                     /mol_type=\"other DNA\"
                     /note=\"color: #ffffff\"
"           locusName 
            totLength  // header line locus length
            topology
            now.Day (mon.[now.Month-1]) now.Year  // header line date
            totLength  // length for REFERENCE line
            (mon.[now.Month-1]) now.Day now.Year  // Journal export date
            totLength // source length line
        |> w

        for p in a.dnaParts do
            let colorFwd = 
                match p.sliceType with 
                | REGULAR -> 
                    match p.breed with
                    | Breed.B_UPSTREAM -> "#009933"
                    | Breed.B_DOWNSTREAM -> "#009933"
                    | Breed.B_PROMOTER -> "#3399FF"
                    | Breed.B_FUSABLEORF -> "#FF0000"
                    | Breed.B_GS -> "#FF3300"
                    | Breed.B_GST -> "#FF6600"
                    | Breed.B_INLINE -> "#99CCFF"
                    | Breed.B_TERMINATOR -> "#000066"
                    | Breed.B_VIRTUAL -> "#FF0066"
                    | Breed.B_LINKER -> "#996633"
                    | Breed.B_MARKER -> "#336600"
                    | Breed.B_X -> "#000000"
                | LINKER -> "#FF0000"
                | MARKER -> "yellow" 
                | INLINEST -> "green" 
                | FUSIONST -> "red"
            let colorRev = colorFwd // reserve possibility of different colors for different orientations but for now the same
            let range = sprintf (if p.destFwd then "%A..%A" else "complement(%A..%A)") (zero2One p.destFr) (zero2One p.destTo)
            let label = 
                if p.sliceName <> "" then 
                    p.sliceName 
                 else if p.description <> "" then 
                    p.description 
                 else (ambId p.id)
            sprintf "     misc_feature    %s
                     /label=\"%s\"
                     /note=\"%s\"
                     /note=\"color: %s; direction: %s\"\n"
                        range 
                        label
                        label
                        (if p.destFwd then colorFwd else colorRev)
                        (if p.destFwd then "RIGHT" else "LEFT")
            |> w

        // Primer emission
        let makeRange l r = sprintf "%A..%A" (l+1) (r+1) // +1 for human friendly coords
        let complement s = sprintf "complement(%s)" s

        let emitPrimer isFwd (primer:Primer) =
            let searchDna = if isFwd then primer.Primer else primer.Primer.RevComp()
            let ampBody = 
                match primer.Interval DNAIntervalType.AMP with 
                    | Some(i) -> primer.Primer.[i.il..i.ir]
                    | None -> primer.body
            //search using either the ampBody or reverse complement of it depending on direction of primer
            let searchBody =  if isFwd then ampBody else ampBody.RevComp()
            // this isn't an ideal way to place primers but simpler than trying to infer coordinates during emission
            // will break if there are multiple binding sites but that might be a good thing to alert user
            let left = a.Sequence().IndicesOf(searchDna) |> Seq.head 
            let right = left + primer.Primer.Length-1
            // naming primers using part it binds to
            let containsPrimer (part:DNASlice) = 
                part.dna.Contains searchBody
            let bindingPart = List.tryFind containsPrimer a.dnaParts
            let name =
                match bindingPart with
                | Some value -> 
                    if value.sliceName <> "" then
                        value.sliceName
                    else if value.description <> "" then
                        value.description
                    else (ambId value.id)
                | None -> ""
            
            sprintf "     primer_bind     %s 
             /note=\"%s_%s\"
             /note=\"color: #a020f0; sequence: %s; direction: %s\"\n" 
                (makeRange left right |> if isFwd then (id) else complement)  
                name
                (if isFwd then "fwd" else "rev")
                primer.Primer.str
                (if isFwd then "RIGHT" else "LEFT")
            |> w

        for pp in primers do 
            match pp with
            | GAP -> () // nothing to emit
            | DPP(dpp) ->
                if dpp.fwd.Primer.Length > 0 then emitPrimer true dpp.fwd
                if dpp.rev.Primer.Length > 0 then emitPrimer false dpp.rev

        "ORIGIN\n" |> w
        
        a.Sequence()
        |> formatGB
        |> w
