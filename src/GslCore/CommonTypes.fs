module commonTypes

open System
open pragmaTypes
open constants
open DesignParams
open uri
open LegacyParseTypes
open Amyris.Dna
open Amyris.ErrorHandling
open AstTypes

type SequenceLibrary = Map<string, Dna>

/// Instructions gleaned from command line
type ParsedOptions =
   {quiet: bool;
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
    doHelpPragmas : bool
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

/// Given a slice offset from the start of a gene's ORF, determine what offset the first allele
/// in the resulting sequence will have.
let orfOffsetFromAlleleOffset (offset: int<ZeroOffset>) =
    match (offset/1<ZeroOffset>) % 3 with
    | 0 -> Zero
    | 1 | -2 -> One
    | 2 | -1 -> Two
    | _ -> Zero // this case is unreachable

/// Slice annotation for indicating the presence of an ORF in a slice.
type OrfAnnotation =
    /// The leftmost base pair of this ORF.
   {left: int<ZeroOffset>;
    /// The rightmost base pair of this ORF, inclusive.
    right: int<ZeroOffset>;
    /// Is the first base of this ORF offset into a codon?
    /// This field should be interpreted in the context of direction,
    /// as it applies to the leftmost base in a fwd Orf vs. the rightmost base in a rev Orf.
    frameOffset: OrfOffset;
    /// Is this ORF on the fwd or reverse direction relative to this slice?
    fwd: bool}
    with
    /// Return a sequence of the starting indices of every complete codon described by this annotation.
    /// If the Orf is fwd, these will be in increasing order; if rev, decreasing order.
    member x.CompleteCodonIndices() =
        let left, right = z2i x.left, z2i x.right
        // the number of bases we need to move inwards from the edge to find the start of the first codon
        let alleleOffset =
            match x.frameOffset with
            | Zero -> 0
            | One -> 2
            | Two -> 1
        // compute the number of complete codons based on the size of the ORF and the offset
        let nCodons =
            let nBases = right - left + 1 - alleleOffset
            nBases / 3

        let codonOffsets = seq {0..3..(nCodons-1)*3}
        if x.fwd then
            let firstCodon = left + alleleOffset
            codonOffsets
            |> Seq.map (fun offset ->
                (firstCodon + offset)*1<ZeroOffset>)
        else
            let firstCodon = right - alleleOffset
            codonOffsets
            |> Seq.map (fun offset ->
                (firstCodon - offset)*1<ZeroOffset>)

/// Create an ORF annotation from a slice on gene-relative coordiantes.
let orfAnnotationFromSlice (slice: Slice) (orfLen: int) fwd context =
    let sliceStart, sliceEnd =
        getBoundsFromSlice slice orfLen context
        |> returnOrFail
        |> (fun (l, r) -> one2Zero l, one2Zero r)

    // compute the actual length of the slice
    let sliceLen = z2i(sliceEnd - sliceStart) + 1

    // based on the gene-relative slice coordinates, determine the frame offset of this ORF.
    let frameOffset = orfOffsetFromAlleleOffset sliceStart

    // if left is less than 0, then the ORF starts at a positive offset
    // these coordinates are now relative to the materialized DNA slice
    let orfStart = -1 * sliceStart // slice coordinates are relative to ORF start at 5' end
    let orfEnd = orfStart + orfLen*1<ZeroOffset> - 1<ZeroOffset>

    // if the part is reversed, the start and end need to be flipped to be relative to
    // the opposite ends
    let left, right =
        if fwd then orfStart, orfEnd
        else (orfLen*1<ZeroOffset>) - orfEnd - 1<ZeroOffset>, (orfLen*1<ZeroOffset>) - orfStart - 1<ZeroOffset>

    // the slice may be smaller than the ORF in either direction, so make sure left and right
    // are constrained to the interval defined by the length of the slice
    let constrain index =
        index
        |> max 0<ZeroOffset>
        |> min (sliceLen*1<ZeroOffset> - 1<ZeroOffset>)

    {left = constrain left; right = constrain right; frameOffset = frameOffset; fwd = fwd}

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

type DnaAssembly =
   { id : int option
     dnaParts : DNASlice list
     name : string
     uri : Uri option
     linkerHint : string
     pragmas : PragmaCollection
     designParams : DesignParams
     docStrings : string list
     materializedFrom : Assembly
     topology : Topology }
with
    member x.Sequence() =
        x.dnaParts
        |> Seq.map (fun p -> p.dna)
        |> DnaOps.concat

    interface ISourcePosition with
        member x.OptionalSourcePosition = x.materializedFrom.sourcePosition

/// DNASlice default for a fusion slidetype
let fusionSliceConstant = {
    id = None;
    extId = None;
    sliceName = "fusion";
    uri = None; // TODO: uri for fusion parts?
    dna = Dna("");
    sourceChr = "";
    sourceFr = 0<ZeroOffset>;
    sourceTo = 0<ZeroOffset>;
    sourceFwd = true;
    template = None;
    amplified = false;
    sourceFrApprox = false;
    sourceToApprox =false;
    destFr = 0<ZeroOffset>;
    destTo = 0<ZeroOffset>;
    destFwd= true;
    description ="::";
    dnaSource = "";
    sliceType = FUSIONST;
    pragmas = EmptyPragmas;
    breed = B_VIRTUAL;
    materializedFrom = None; // TODO: should we mark this as associated with this ppp?
    annotations = [];
}

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


