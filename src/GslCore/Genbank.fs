module Genbank
/// common routines for emitting Genbank format
open System.Text
open System

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
    

let mon = [| "JAN" ; "FEB" ; "MAR" ; "APR" ; "MAY" ; "JUN" ; "JUL" ; "AUG" ; "SEP" ; "OCT"; "NOV" ; "DEC" |]
