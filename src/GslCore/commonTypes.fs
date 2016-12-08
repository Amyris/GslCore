module commonTypes

open System
open pragmaTypes
open constants
open ThumperProxyTypes
open DesignParams
open RefGenome
open uri
open LegacyParseTypes
open Amyris.Dna

type SequenceLibrary = Map<string, Dna>

/// Instructions gleaned from command line
type ParsedOptions =
   {quiet: bool;
    refStrain: string;
    libDir: string;
    iter: bool;
    onlyPhase1: bool;
    doParallel: bool;
    verbose: bool;
    noPrimers: bool;
    lexOnly: bool;
    refList : bool;
    refDump : string option;
    listPlugins: bool;
    }

type DNAIntervalType = ANNEAL | RYSELINKER | AMP | SANDWICH
type DNAInterval = {il:int; ir:int; iType:DNAIntervalType}

type Breed =
    | B_PROMOTER
    | B_TERMINATOR
    | B_MARKER
    | B_FUSABLEORF
    | B_UPSTREAM
    | B_DOWNSTREAM
    | B_GST
    | G_M
    | G_STOP
    | B_GS
    | B_INLINE
    | B_X
    | B_VIRTUAL
    | B_LINKER

/// Used in the grammar of GSL to pick a standard part of a gene (p, t, o etc)
type StandardSlice =
    | GENE
    | PROMOTER
    | TERMINATOR
    | ORF
    | FUSABLEORF
    | UPSTREAM
    | DOWNSTREAM
    | MRNA (* ORF + term *)

let sliceTypeChars = ['p', 'u', 't', 'd', 'o', 'f', 'g', 'm']
let charToSliceType c =
    match Char.ToLower c with
    | 'p' -> Some(PROMOTER)
    | 'u' -> Some(UPSTREAM)
    | 't' -> Some(TERMINATOR)
    | 'd' -> Some(DOWNSTREAM)
    | 'o' -> Some(ORF)
    | 'f' -> Some(FUSABLEORF)
    | 'g' -> Some(GENE)
    | 'm' -> Some(MRNA)
    | _ -> None

type SliceType = REGULAR | MARKER | LINKER | INLINEST | FUSIONST
let formatST (s:SliceType) =
    match s with
    | REGULAR -> "REG"
    | MARKER -> "MARKER"
    | LINKER -> "LINKER"
    | INLINEST -> "INLINE"
    | FUSIONST ->"FUSION"

/// Due to slices or other considerations, Orfs may not exactly align with the codon sequence.
/// Indicate which position in the first codon is represented by the first base in the orf.
type OrfOffset = | Zero | One | Two

/// Slice annotation for indicating the presence of an ORF in a slice.
type OrfAnnotation =
    /// The leftmost base pair of this ORF.
   {left: int<ZeroOffset>;
    /// The rightmost base pair of this ORF, inclusive.
    right: int<ZeroOffset>
    /// Is the first base of this ORF offset into a codon?
    /// This field should be interpreted in the context of strand,
    /// as it applies to the leftmost base in a fwd Orf vs. the rightmost base in a rev Orf.
    frameOffset: OrfOffset;
    /// Is this ORF on the fwd or reverse direction relative to this slice?
    fwd: bool;
}

/// Extensible type to add useful annotations to slices.
type SliceAnnotation =
    | Orf of OrfAnnotation


/// Represents one piece of DNA for assembly, capturing its origins and relevant details
type DNASlice =
   {id: int option;
    extId: string option;
    dna: Dna;
    sourceChr: string;
    sourceFr: int<ZeroOffset>;
    sourceTo: int<ZeroOffset>;
    sourceFwd: bool;
    sourceFrApprox: bool;
    sourceToApprox: bool;
    destFr: int<ZeroOffset>;
    destTo: int<ZeroOffset>;
    destFwd: bool;
    /// is this slice created by PCR
    amplified: bool;
    template: Dna option;
    sliceName: string;
    uri: Uri option;
    description: string;
    sliceType: SliceType;
    pragmas: PragmaCollection;
    dnaSource: string;
    breed: Breed;
    /// Keep track of the part this slice was materialized from.
    materializedFrom: PPP option;
    annotations: SliceAnnotation list}

type DnaAssembly =
   {id: int option;
    dnaParts: DNASlice list;
    name: string;
    uri: Uri option;
    linkerHint: string;
    pragmas: PragmaCollection;
    designParams: DesignParams;
    docStrings: string list;
    materializedFrom: Assembly}
    with
    member x.Sequence() =
        x.dnaParts
        |> Seq.map (fun p -> p.dna)
        |> DnaOps.concat

/// Model a primer which diverges and has body/tail parts.
/// The body part anneals to the intended amplification target and the tail
/// hangs out and anneals for stitching purposes
type Primer =
    {tail: Dna;
     body: Dna;
     annotation: DNAInterval list}
    with
    member x.Primer
        with get() = DnaOps.append x.tail x.body

    member x.lenLE(maxOligo) =
        x.tail.Length + x.body.Length<=maxOligo

    /// Try to find an interval of type iType, returns Some/None
    member x.Interval(iType:DNAIntervalType) =
        x.annotation |> List.tryFind (fun i -> i.iType = iType)
end

/// Divergend pair of Primers
type PrimerPair = {fwd:Primer; rev:Primer; name:string}

type DivergedPrimerPair =
    | DPP of PrimerPair
    | GAP

type RYSELinker = {name:string; dna: Dna}


