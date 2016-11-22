module ape

open System.IO
open System.Text
open System
open commonTypes
open constants
open Amyris.Bio.utils
open utils
        
/// Format a dna sequence in genbank human readable form
let formatGB (dna : char array) =
    let rows = seq {0..60..dna.Length} |> Seq.map (fun i -> i,dna.[i..(min (i+59) (dna.Length-1))]) |> Array.ofSeq
    let rowsSplit (i:int,row:char array) =
        seq {0..10..row.Length-1} |> Seq.map (fun i -> new String(row.[i..(min (i+9) (row.Length-1))]))
            |> fun x -> i,(String.Join (" ",Array.ofSeq x))
    let tb = new StringBuilder()
    for i,row in (rows |> Array.map (rowsSplit)) do
        tb.AppendLine(sprintf "%9d %s" (i+1) row) |> ignore
    tb.Append("//\n") |> ignore
    tb.ToString()
    
/// Emit APE (genbank) format
///  outDir : string   tag: string  prefix for files  assemblies : List of AssemblyOut
let dumpAPE (outDir:string) (tag:string) (assemblies : AssemblyOut list) =
    let mon = [| "JAN" ; "FEB" ; "MAR" ; "APR" ; "MAY" ; "JUN" ; "JUL" ; "AUG" ; "SEP" ; "OCT"; "NOV" ; "DEC" |]
    for a in assemblies do
        let path = sprintf "%s.%d.ape" tag (match a.id with None -> failwith "ERROR: unassigned assembly id" | Some(i) -> i )
                        |> opj outDir
        printf "Writing ape output to dir=%s tag=%s path=%s\n" outDir tag path
        use outF = new StreamWriter(path)
        let w (s:string) = outF.Write(s)
        
        let locusName = sprintf "%s_ape_output" tag
        let totLength = a.dnaParts |> List.map (fun p -> p.dna.Length) |> Seq.sum
        let now = DateTime.Now
        sprintf "LOCUS                 %15s%5d bp ds-DNA   linear       %2d-%s-%d
DEFINITION  .
ACCESSION
VERSION
SOURCE      .
  ORGANISM  .
COMMENT
COMMENT     ApEinfo:methylated:1
FEATURES             Location/Qualifiers
"           locusName totLength now.Day (mon.[now.Month-1]) now.Year |> w
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
        
        let dnaJoined = a.dnaParts |> List.map (fun p -> p.dna) |> Array.concat
        formatGB dnaJoined |> w

      
            
