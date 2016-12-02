/// Code for flat file output format
module dumpFlat
open System
open System.IO

open Amyris.Bio.utils
open utils
open shared
open commonTypes

/// Text representation of the assemblies to stdout
/// dumpFlat file format
let dumpFlat (outFile:string) (assembliesIn : DnaAssembly list) =
    if outFile <> "-" then printfn "Writing flat format to %s" outFile
    let outF = if outFile = "-" then None else Some(new StreamWriter(outFile))

    /// Assign ids to any linkers present
    let assignLinkPartNumbers (al:DnaAssembly list) =
        // get linker list
        let unlabeled =
            seq { // find unlabeled parts
                for a in al do
                    for d in a.dnaParts do
                        match d.id with
                        | Some(_) -> ()
                        | None -> yield d.description
            } |> Set.ofSeq

        let map = Seq.zip unlabeled {100000..99999+unlabeled.Count} |> Map.ofSeq

        al |> List.map (fun a ->
            {a with dnaParts = a.dnaParts |> List.map (fun d ->
                match d.id with
                | Some(_) -> d
                | None -> {d with id = Some(map.[d.description])})
            })

    let assemblies = assignLinkPartNumbers assembliesIn
    let w (s:string) =
        match outF with
        | None -> stdout.WriteLine(s)
        | Some(x) -> x.WriteLine(s)

    for a in assemblies do
        let aId = ambId a.id
        sprintf "##### Assembly %s #######" aId |> w
        sprintf "A# %s" aId |> w
        sprintf "NA %s" a.name |> w
        match a.uri with Some(u) -> sprintf "NU %s" u |> w | None -> ()
        sprintf "NP %d" (a.dnaParts.Length) |> w
        sprintf "AS %s" (a.Sequence().str) |> w
        sprintf "" |> w
        
        for p in a.dnaParts do
            sprintf "P# %s" (ambId p.id) |> w
            if p.sliceName <> "" then sprintf "SN %s" p.sliceName |> w
            match p.uri with Some(u) -> sprintf "NU %s" u |> w | None -> ()
            sprintf "DE %s" p.description |> w
            sprintf "ST %s" (formatST p.sliceType) |> w
            sprintf "PA %s" aId |> w
            sprintf "DS %s" p.dnaSource |> w
            sprintf "CH %s" p.sourceChr |> w
            sprintf "LT %A" p.sourceFr |> w
            sprintf "RT %A" p.sourceTo |> w
            sprintf "LE %d" p.dna.Length |> w
            sprintf "FW %s" (if p.sourceFwd then "+" else "-") |> w
            sprintf "SQ\n%s\n//\n" (format60 p.dna.arr) |> w
        sprintf "" |> w
        
    // Now emit a summary of the assembly parts
    sprintf "######### Assembly summary ##############" |> w
    for a in assemblies do
        let s = sprintf "AI %s -> " (ambId a.id)
        match outF with | None -> stdout.Write(s) | Some(x) -> x.Write(s)

        sprintf "%s"
           (a.dnaParts
            |> List.map (fun p -> ambId p.id)
            |> Array.ofList
            |> (fun x -> String.Join(",",x)))
        |> w

    match outF with
    | None -> ()
    | Some(f) -> f.Close()
