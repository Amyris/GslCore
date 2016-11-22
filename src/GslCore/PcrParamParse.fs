/// Parsing for PCR parameters.
module PcrParamParse

open System
open Amyris.Bio.primercore
open System.Text.RegularExpressions
open Amyris.ErrorHandling
open constants

/// Handle parsing of PCR parameters

// e.g.   ERROR: unknown pragma #pcrparams mon=50mM div=m5mM dNTP=0uM template=0uM primer=0.25uM

let re = new Regex("([^= ]*)=(\d*\.?\d*)(\w*)")

// ParseRegex parses a regular expression and returns a list of the strings that match each group in
// the regular expression.
// List.tail is called to eliminate the first element in the list, which is the full matched expression,
// since only the matches for each group are wanted.
let (|ParsePcrParam|_|) str =
    let m = re.Match(str)
    if m.Success then
        let vals = List.tail [ for x in m.Groups -> x.Value ]
        Some (vals)
    else None

type PcrUnit = | UM | MM | NM

let (|ParsedAsFloat|_|) str =
    match Double.TryParse(str) with
    | true, v -> Some v
    | _ -> None

let (|ParsedAsUnit|_|) str =
    match str with
    | "um" -> Some UM
    | "mm" -> Some MM
    | "nm" -> Some NM
    | _ -> None

type PcrParamTag = | Mon | Div | Dntp | Template | Primer

let (|KnownTag|UnknownTag|) str =
    match str with
    | "mon" -> KnownTag Mon
    | "div" -> KnownTag Div
    | "dntp" -> KnownTag Dntp
    | "template" -> KnownTag Template
    | "primer" -> KnownTag Primer
    | _ -> UnknownTag str



let matchErrorMsg = "#pcrparams should match tag=valueunit pattern (no spaces).
tags: [mon, div, dntp, template, primer]; value: a decimal number; unit: [uM, mM, nM]
e.g. #pcrparams mon=50mM div=1.5mM dNTP=200uM template=0.01uM primer=0.25uM"

/// Try to parse a single argument.
let parseArg (a: string) =
    match a.ToLower() with
    | ParsePcrParam [tag; ParsedAsFloat v; ParsedAsUnit u] ->
        let unitVal =
            match u with
            | UM -> v*1.0<uM> |> Amyris.Bio.primercore.uM2M
            | MM -> v*1.0<mM> |> Amyris.Bio.primercore.mM2M
            | NM -> v*1.0<nM> |> Amyris.Bio.primercore.nM2M
        match tag with
        | KnownTag t -> ok (t, unitVal)
        | UnknownTag t -> fail (sprintf
            "unknown pcr parameter '%s', should be one of mon, div, dntp, template or primer" t)
    | _ -> fail (sprintf "Invalid argument: '%s'. %s" a matchErrorMsg)

/// Try to parse a single argument, and use it to up
let updatePP pp (tag, unitVal) =
    match tag with
    | Mon -> {pp with monovalentConc = unitVal}
    | Div -> {pp with divalentConc = unitVal}
    | Dntp -> {pp with dNTPConc = unitVal}
    | Template -> {pp with templateConc = unitVal}
    | Primer -> {pp with primerConc = unitVal}

let parseArgUpdatePP (a: string) pp =
    parseArg a
    |> lift (updatePP pp)
