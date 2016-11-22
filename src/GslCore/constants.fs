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
let maxThumperOligo = 60
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


/// Convert a OneOffset relative position
/// to a zero based coordinate system.  Follows
/// convention that -1 is the base before the 'A' in ATG,
/// and +1 is the 'A' for the 5' end.  For the 3' end, 
/// -1 is the last base of the stop codon, and +1 is the first
/// base of the terminator
let one2ZeroOffset (f:RelPos) =
    let o = f.x
    match f.relTo with
        | FivePrime ->
            if o >=1<OneOffset> then (o-1<OneOffset>) * 1<ZeroOffset/OneOffset>
                else o * 1<ZeroOffset/OneOffset>
        | ThreePrime ->
            if o >=1<OneOffset> then o * 1<ZeroOffset/OneOffset>
                else (o+1<OneOffset>) * 1<ZeroOffset/OneOffset>

let zero2One (z:int<ZeroOffset>) =((z / 1<ZeroOffset>)  + (if z<0<ZeroOffset> then 0 else 1) ) * 1<OneOffset>
let z2i (z:int<ZeroOffset>) = z/1<ZeroOffset>

[<Measure>]type PluginScore

/// Domain wrapper type to indicate that a string represents literal GSL source.
type GslSourceCode = GslSourceCode of string
    with
    member x.String =
        let (GslSourceCode(s)) = x
        s
    override x.ToString() = x.String
