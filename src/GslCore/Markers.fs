module markers
open Amyris.Bio.utils
open System.IO

/// Logic for reading in marker sets that support the ### virtual part

(*
hphAX   25672   2of2    5       9
hphAX   25671   1of2    5       9
hphAX   13992   2of2    2       9
*)

type HalfMarker =
    {name: string;
     rid: int;
     linkerFr: string;
     linkerTo: string;
     part1or2: int}

let loadMarkers (libPath:string) =
    let markerPath = opj libPath "markers.txt"
    if not (File.Exists(markerPath)) then
        Set.empty
    else
        (eachLineIn markerPath)
        |> Seq.map (tabSplit)
        |> Seq.map
            (function 
                | [|name; id; "1of2"; frId; toId|] ->
                    {name = name;
                     rid = int id; 
                     linkerFr = frId;
                     linkerTo = toId; 
                     part1or2 = 1}
                | [|name; id; "2of2"; frId; toId |] ->
                    {name = name;
                     rid = int id; 
                     linkerFr = frId;
                     linkerTo = toId; 
                     part1or2 = 2}
                | x -> failwithf "ERROR: unmatched line in %s:  %A\n" markerPath x)
        |> Set.ofSeq

let legalMarkers path =
    loadMarkers path |> Set.map (fun halfMarker -> halfMarker.name)

