module uri
open System
// Big TODO: dry out the duplication of these constants between Thumper and GSLc

let amyrisUriBase = "http://amyris.com/GBoM"

let uriPathDelimiter = "/"
let uriTermDelimiter = "/"

type Uri = string

// TODO: make this a specific instance of a generic Result type.
type UriResult =
    | Ok of Uri
    | Err of string

// TODO: make this generic
/// Helper function to raise an exception on a bad UriResult
let unwrap result =
    match result with
    | Ok(x) -> x
    | Err(e) -> failwith e

let forbiddenChars = [| uriPathDelimiter; uriTermDelimiter |]

/// Check a string for forbidden characters, return Some(s, badChars) if any are found.
let checkForbiddenChars (s:string) =
    let badChars = seq {for c in forbiddenChars do if s.Contains(c) then yield c} |> List.ofSeq
    match badChars with
    | [] -> None
    | x -> Some(s, x)

let checkTermsForIssues (ts:string list) =
    let issues = List.choose checkForbiddenChars ts
    if not issues.IsEmpty then
        Some("Found bad chars TODO informative error message.")
    else None


/// Construct a local URI from a list of namespaces and an instance term.
let buildUri (namespaces:string list) (term:string) =
    // TODO: type constraint on stringifyable term?
    match checkTermsForIssues (term::namespaces) with
    | Some(e) -> Err(e)
    | None ->
        let ub = System.Text.StringBuilder()
        ub.Append(amyrisUriBase) |> ignore
        for ns in namespaces do
            ub.Append(uriPathDelimiter + ns) |> ignore
        ub.Append(uriTermDelimiter) |> ignore
        ub.Append(term) |> ignore
        Ok(ub.ToString())

/// Construct a URI namespace extension.
let addNamespaces (baseNamespace:string) (namespaces:string list) =
    match checkTermsForIssues namespaces with
    | Some(e) -> Err(e)
    | None ->
        let ub = System.Text.StringBuilder()
        ub.Append(baseNamespace) |> ignore
        for ns in namespaces do
            ub.Append(uriPathDelimiter + ns) |> ignore
        Ok(ub.ToString())

/// Add a term entry into a namespace.
let addTermToNamespace (baseNamespace:string) (term:string) =
    match checkTermsForIssues [term] with
    | Some(e) -> Err(e)
    | None -> Ok(baseNamespace + uriTermDelimiter + term)

// TODO: possibly move these definitions into the appropriate module
let linkerBase = unwrap (addNamespaces amyrisUriBase ["Component"; "Linker"])

/// Construct a RYSE linker URI from a link code.
/// Since this is entirely programmatic we expect it should never fail at
/// runtime; thus, raises an exception on error.
let linkerUri linkCode = unwrap (addTermToNamespace linkerBase linkCode)

let gslcTempUriBase = unwrap (addNamespaces amyrisUriBase ["GSLC"; "TEMP"])

// heap-allocated counter
let globalUriCounter = ref 0

/// Construct a locally-unique temporary URI.
let createTempUri () =
    let value = !globalUriCounter
    globalUriCounter := value + 1
    unwrap (addTermToNamespace gslcTempUriBase (sprintf "%d" value))


