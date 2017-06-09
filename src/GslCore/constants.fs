/// Numeric and string constants, as well as some broadly-needed domain types.
module constants
open Amyris.Bio.primercore

let ryseLinkerTargetDefault = 60.0<C> // melting temp for linker overhang melting temp
let seamlessTargetDefault = 68.0<C> // higher temp for seamless junctions
let seamlessOverlapTargetDefault = 68.0<C> // Temperature for internal overlap region of a seamless junction
let overlapMinLenDefault = 24 
let approxMargin = 50
let primerMaxDefault = 60
let primerMinDefault = 20
let maxNameLen = 600 // see also thumper restriction in thumper.fs
let maxThumperOligo = 80
let flanklenDefault = 500

/// Classic allele swap codon replacement min frequency
let minHBCodonUsage = 0.05                                  

let strToTempC (s:string): float<C> =
    (float s ) * 1.0<C>

let defaultRefGenome = "cenpk"
let defaultUser = "platt"                                                

let aaLegal = "ACDEFGHIKLMNPQRSTVWY*" |> Set.ofSeq

/// List of approved linker abbreviations.
let legalLinkers =
    [ '0' .. '9' ] @ [ 'A'..'E']
    |> List.map (fun c -> sprintf "%c" c) |> Set.ofSeq

[<Measure>] type OneOffset
[<Measure>] type ZeroOffset

type GeneEnd = FivePrime | ThreePrime
type RelPos = { x : int<OneOffset> ; relTo : GeneEnd }

/// Drop the units from a ZeroOffset int.
let z2i (z:int<ZeroOffset>) = z/1<ZeroOffset>

/// Convert a ZeroOffset int into a OneOffset int.
let zero2One (z:int<ZeroOffset>) =
    let unitless = z2i z
    if unitless >= 0 then unitless+1 else unitless
    * 1<OneOffset>

/// Convert a OneOffset int into a ZeroOffset int.
let one2Zero (z:int<OneOffset>) =
    match z/1<OneOffset> with
    | 0 -> 0
    | x when x > 0 -> (x-1)
    | x -> x
    *1<ZeroOffset>

[<Measure>]type PluginScore

/// Domain wrapper type to indicate that a string represents literal GSL source.
type GslSourceCode = GslSourceCode of string
    with
    member x.String =
        let (GslSourceCode(s)) = x
        s
    override x.ToString() = x.String
