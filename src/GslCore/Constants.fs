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
/// Terminator length default value (can be overriden in genome def)
let termLenDefault = 500

/// Terminator length default value when part of an 'm' mRNA part (can be overriden in genome def)
let termLenMRNADefault = 200

/// Promoter length default value (can be overriden in genome def)
let promLenDefault = 500

/// Classic allele swap codon replacement min frequency
let minHBCodonUsage = 0.05                                  

let strToTempC (s:string): float<C> =
    (float s ) * 1.0<C>

// FIXME: this should be injected as a configuration parameter rather than set globally.
let mutable defaultRefGenome = "cenpk"

let aaLegal = "ACDEFGHIKLMNPQRSTVWY*" |> Set.ofSeq

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
