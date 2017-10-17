module snapgene

open System.IO
open System
open commonTypes
open constants
open Amyris.Bio.utils
open utils
open Amyris.Dna
open Genbank
        
/// Emit Snapgene (genbank) format
///  outDir : string   tag: string  prefix for files  assemblies : List of AssemblyOut
let dumpSnapgene (outDir:string) (tag:string) (assemblies : DnaAssembly list) (primers:DivergedPrimerPair list list option) =
    // pair up the primer list (if they exist) with the matching assembly
    let assemWithPrimers = ( 
                                match primers with
                                | Some p -> p
                                | None -> List.init assemblies.Length (fun _ -> [])
                            ) 
                            |> List.zip assemblies

    for (a,primers) in assemWithPrimers do

        let path = sprintf "%s.%d.dna" tag (match a.id with None -> failwith "ERROR: unassigned assembly id" | Some(i) -> i )
                        |> opj outDir
        printf "Writing snapgene output to dir=%s tag=%s path=%s\n" outDir tag path


        use outF = new StreamWriter(path)
        let w (s:string) = outF.Write(s)
        
        let locusName = sprintf "%s_snapgene_output" tag
        let totLength = a.dnaParts |> List.map (fun p -> p.dna.Length) |> Seq.sum
        let now = DateTime.Now
        sprintf "LOCUS       %-22s %d bp ds-DNA     linear   SYN %2d-%s-%d
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
  JOURNAL   Exported %s %2d, %d from SnapGene Viewer 4.0.5
            http://www.snapgene.com
FEATURES             Location/Qualifiers
     source          1..%d
                     /organism=\"synthetic DNA construct\"
                     /mol_type=\"other DNA\"
                     /note=\"color: #ffffff\"
"           "Exported" // snapgene fails to color anything if it's named anything else locusName 
            totLength 
            now.Day (mon.[now.Month-1]) now.Year 
            totLength 
            (mon.[now.Month-1]) now.Day now.Year 
            totLength
        |> w

        for p in a.dnaParts do
            let colorFwd = match p.sliceType with 
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
            //let colorRev = match p.sliceType with | REGULAR -> "#0000F0" | LINKER -> "#D00000" 
            //                                      | MARKER -> "yellow" | INLINEST -> "green" | FUSIONST ->"red"
            let colorRev = colorFwd
            let range = sprintf (if p.destFwd then "%A..%A" else "complement(%A..%A)") (zero2One p.destFr) (zero2One p.destTo)
            let label = 
                        (if p.sliceName <> "" then 
                            p.sliceName 
                         else if p.description <> "" then 
                            p.description 
                         else (ambId p.id))  
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

        for pp in primers do 
            match pp with
            | GAP -> () // nothing to emit
            | DPP(dpp) ->
                if dpp.fwd.Primer.Length > 0 then
                    let left = a.Sequence().IndicesOf(dpp.fwd.Primer) |> Seq.head
                    let right = left + dpp.fwd.Primer.Length-1
                    sprintf "     primer_bind     %A..%A
                     /note=\"%s_Fwd\"
                     /note=\"color: #a020f0; direction: RIGHT\"\n" (left+1) (right+1)  dpp.name
                    |> w

                if dpp.rev.Primer.Length > 0 then
                    let left = a.Sequence().IndicesOf(dpp.rev.Primer.RevComp()) |> Seq.head
                    let right = left + dpp.rev.Primer.Length-1
                    sprintf "     primer_bind     complement(%A..%A)
                     /note=\"%s_rev\"
                     /note=\"color: #a020f0; direction: LEFT\"\n" (left+1) (right+1) dpp.name
                    |> w

        sprintf "ORIGIN\n" |> w
        
        a.Sequence().arr
        |> formatGB
        |> w