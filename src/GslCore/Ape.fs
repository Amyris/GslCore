module ape

open System.IO
open System
open commonTypes
open constants
open Amyris.Bio.utils
open utils
open Amyris.Dna
open Genbank
open pragmaTypes
        
let topologyToString : Topology -> string =
    function
    | Linear -> "linear"
    | Circular -> "circular" 
        
/// Emit APE (genbank) format
///  outDir : string   tag: string  prefix for files  assemblies : List of AssemblyOut
let dumpAPE (outDir:string) (tag:string) (assemblies : DnaAssembly list) =
    for a in assemblies do
        let path = sprintf "%s.%d.ape" tag (match a.id with None -> failwith "ERROR: unassigned assembly id" | Some(i) -> i )
                        |> opj outDir
        printf "Writing ape output to dir=%s tag=%s path=%s\n" outDir tag path
        use outF = new StreamWriter(path)
        let w (s:string) = outF.Write(s)
        
        let locusName = sprintf "%s_ape_output" tag
        let totLength = a.dnaParts |> List.map (fun p -> p.dna.Length) |> Seq.sum
        let now = DateTime.Now
        let topology = a.topology |> topologyToString
        sprintf "LOCUS                 %15s%5d bp ds-DNA   %s       %2d-%s-%d
DEFINITION  .
ACCESSION
VERSION
SOURCE      .
  ORGANISM  .
COMMENT
COMMENT     ApEinfo:methylated:1
FEATURES             Location/Qualifiers
"           locusName totLength topology now.Day (mon.[now.Month-1]) now.Year |> w
        for p in a.dnaParts do
            let colorFwd = match p.sliceType with | REGULAR -> "#0000FF" | LINKER -> "#FF0000"
                                                  | MARKER -> "yellow" | INLINEST -> "green" | FUSIONST -> "red"
            let colorRev = match p.sliceType with | REGULAR -> "#0000F0" | LINKER -> "#D00000" 
                                                  | MARKER -> "yellow" | INLINEST -> "green" | FUSIONST ->"red"
            let range = sprintf (if p.destFwd then "%A..%A" else "complement(%A..%A)") (zero2One p.destFr) (zero2One p.destTo)
            sprintf "     misc_feature    %s
                     /label=%s
                     /ApEinfo_fwdcolor=%s
                     /ApEinfo_revcolor=%s\n" range 
                        (if p.sliceName <> "" then p.sliceName else if p.description <> "" then p.description else (ambId p.id))  colorFwd colorRev |> w

        sprintf "ORIGIN\n" |> w
        
        a.Sequence()
        |> formatGB
        |> w

      
            
