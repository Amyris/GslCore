/// Functions needed across several modules which require domain types.
module shared

open System
open AstTypes
open commonTypes
open constants
open Amyris.Bio.utils

/// Recalculate the offsets of pieces in a list of pieces after new pieces are added in    
let recalcOffset (pieces: DNASlice list) =
    let lengths =
        pieces |> List.map (fun p -> p.dna.Length*1<ZeroOffset>)
    let _, offsets' =
        lengths |> List.fold (fun (o,r) l -> (o+l,o::r)) (0*1<ZeroOffset>,[])
    let offsets = List.rev offsets'

    List.zip pieces offsets
    |> List.map (fun (p,o) ->
        {p with destFr = o; destTo = o+(p.dna.Length-1)*1<ZeroOffset> } )

let extractLinker (s:string ) =
    if s.StartsWith("Linker_") then s.[7..]
    else failwithf "ERROR: unable to parse linker name '%s'" s

let checkLinker (l:Linker) =
    if not (legalLinkers.Contains(l.l1)) then
        failwithf "ERROR: linker %s not a legal linker" l.l1
    if not (legalLinkers.Contains(l.l2)) then
        failwithf "ERROR: linker %s not a legal linker" l.l2


