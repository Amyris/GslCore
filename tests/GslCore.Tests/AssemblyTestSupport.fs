/// Shared operations for compiling and extracting material from assemblies
module AssemblyTestSupport

open AstTypes
open AstAssertions
open AstExpansion
open constants
open Amyris.ErrorHandling
open commonTypes
open pragmaTypes
open Amyris.Dna

let rec extractAssemblies (n:AstNode) : AstNode list =
    [
        match n with
        | Block b -> 
            let result = b.x |> List.collect extractAssemblies
            yield! result
        | Splice s -> 
            let result = s |> List.ofArray |> List.collect extractAssemblies
            yield! result
        | Part p -> 
            match p.x.basePart with
            | Assembly a as x -> yield x
            | _ -> ()
        | Assembly a as x -> yield x
        | _ -> ()
    ]


/// compile one GSL source example and extract assemblies
let compileOne source =
    source 
    |> GslSourceCode
    |> compile (phase1 Set.empty) 
    |> returnOrFail
    |> fun x -> extractAssemblies x.wrappedNode

/// Simple slice creator with just the parameters
/// needed for testing procAssembly
let makeSimpleSlice dna 
                    sliceName
                    sliceType
                    pragmas
                    isFromApprox 
                    isToApprox 
                    isAmplified 
                    breed
                    =

   {id = None
    extId = None
    dna= dna
    sourceChr = "1"
    sourceFr = 0<ZeroOffset>
    sourceTo = (dna.Length-1)*1<ZeroOffset>
    sourceFwd = true
    sourceFrApprox = isFromApprox
    sourceToApprox = isToApprox
    destFr= 0<ZeroOffset>;
    destTo= (dna.Length-1)*1<ZeroOffset>
    destFwd=true
    /// is this slice created by PCR
    amplified = isAmplified
    template = None // might want to add
    sliceName = sliceName
    uri = None
    description = sliceName
    sliceType = sliceType
    pragmas = pragmas
    dnaSource = "unknown"
    breed = breed
    /// Keep track of the part this slice was materialized from.
    materializedFrom = None
    annotations = []
   }
let uFoo = makeSimpleSlice 
            (Dna "TACTGACTGAGTCTGACTGACGTTAGCTGACTGACTGCATGACGTACGTACTGAGTCAGTCGTACTGACTGACTGCATGACTGACTGCATGCATGATGCGTATCGAGCGGCGCTGCTGTGGTCGTATATCTGGTCGTATGTGCGTACGTGTAGTCATGCGTACTG")
            "uFoo"
            SliceType.REGULAR
            EmptyPragmas
            true
            false
            true
            Breed.B_UPSTREAM
let dFoo = 
    makeSimpleSlice 
        (Dna "TTTGGTATGCTGTTATCGTGTTGGGCGGTCTATTGAGTTTTGCGTGTCGTAGTCGTGCGGCGCGTATTGTGCGTGTCGGCGCGATGCGTGTGTTGAGTCGTGTGGGATTGGTGTGTGTCGTCGCGACTGATCATGTATCAGTCGAGCGATGGTGTGTCAGTGTTGTGAGTCG")
        "dFoo"
        SliceType.REGULAR
        EmptyPragmas
        true // from approx
        false // to approx
        true // amplified
        Breed.B_DOWNSTREAM

/// open reading frame slice
let oBar = 
    makeSimpleSlice 
        (Dna "ATGTCTCAGAACGTTTACATTGTATCGACTGCCAGAACCCCAATTGGTTCATTCCAGGGTTCTCTATCCTCCAAGACAGCAGTGGAATTGGGTGCTGTTGCTTTAAAAGGCGCCTTGGCTAAGGTTCCAGAATTGGATGCATCCAAGGAT")
        "oBar"
        SliceType.REGULAR
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_FUSABLEORF

let marker = 
    makeSimpleSlice
        (Dna "TGTACTGACGTAGTCGTACACGTAGTCGTATCGATGTGCGACGTACTGAGCGTAGTCTGATGCGTATGCTCGTAGTAGTCGTACGTACGTGTCGTCGTGTGTGTAGTCGTGTACGAGCGTACGATCGATCAGTCTGACGTAGTGTAGTCGTAGTGTCGTAGTACGTA")
        "###"
        SliceType.MARKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_MARKER
/// really short inline (14) which will be implemented with primers
let shortInline = 
    makeSimpleSlice
        (Dna "CACATGTGGAGATT")
        "shortInline1"
        SliceType.INLINEST
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_MARKER

let rabitStart = {definition = {name = "rabitstart"; argShape = Zero; scope = PartOnly;
                     desc = "Designate part as the start of a RYSE rabit.";
                     invertsTo = Some("rabitend"); validate = noValidate};
                  args = []
                 }
let rabitEnd = {definition = {name = "rabitend"; argShape = Zero; scope = PartOnly;
                     desc = "Designate part as the end of a RYSE rabit.";
                     invertsTo = Some("rabitstart"); validate = noValidate};
                  args = []
                 }
let amp = {definition = {name = "amp"; argShape = Zero; scope = PartOnly;
                     desc = "make part through amplification";
                     invertsTo = Some("amp"); validate = noValidate};
                  args = []
                 }

let fusePragma = {definition = {name = "fuse"; argShape = Zero; scope = PartOnly;
                     desc = "join part with next part without linker";
                     invertsTo = Some("fuse"); validate = noValidate};
                  args = []
                 }
let inlinePragma = {definition = {name = "inline"; argShape = Zero; scope = PartOnly;
                     desc = "inline pragma";
                     invertsTo = Some("inline"); validate = noValidate};
                  args = []
                 }

/// 102 bp inline
let longInline = 
    makeSimpleSlice 
        (Dna "ATGTCTCAGAACGTTTACATTGTATCGACTGCCAGAACCCCAATTGGTTCATTCCAGGGTTCTCTATCCTCCAAGACAGCAGTGGAATTGGGTGCTGTTATG")
        "longinline"
        SliceType.INLINEST
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_INLINE

/// 75 bp inline
let mediumInline = 
    makeSimpleSlice 
        (Dna "TTTGACGTGTAGTCGTGCGCGGTCGCGCGCGTCTATTTTTGTCGTCGTACGTACGTACGGCTAGCGTACGTACGT")
        "mediuminline"
        SliceType.INLINEST
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_INLINE

/// small 48 bp inline
let smallInline = 
    makeSimpleSlice 
        (Dna "TAGCTATATAGGTAGCTAGACTATCTTTATCTTACTACTTCTCTTTAT")
        "smallinline"
        SliceType.INLINEST
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_INLINE

let smallInlineAmp = {smallInline with pragmas = smallInline.pragmas.Add(amp) ; sliceName = smallInline.sliceName+"#amp"}
let smallInlineFuse = {smallInline with pragmas = smallInline.pragmas.Add(fusePragma) ; sliceName = smallInline.sliceName+"#fuse"}
let mediumInlineAmp = {mediumInline with pragmas = mediumInline.pragmas.Add(amp) ; sliceName = mediumInline.sliceName+"#amp"}
let mediumInlineFuse = {mediumInline with pragmas = mediumInline.pragmas.Add(fusePragma) ; sliceName = mediumInline.sliceName+"#fuse"}

let longInlineAmp = {longInline with pragmas = longInline.pragmas.Add(amp) ; sliceName = longInline.sliceName+"#amp"}
let longInlineAmpFuse = {longInlineAmp with pragmas = longInlineAmp.pragmas.Add(fusePragma) ; sliceName = longInlineAmp.sliceName+"#fuse"}
let longInlineInline = {longInline with pragmas = longInline.pragmas.Add(inlinePragma)}
let shortInlineWithRabitStart = { shortInline with pragmas = shortInline.pragmas.Add(rabitStart)}
let shortInlineWithRabitEnd = { shortInline with pragmas = shortInline.pragmas.Add(rabitEnd)}

let linkerAlice = 
    makeSimpleSlice
        (Dna "GATCGATTAGATCGATAGGCTACG")
        "linkerAlice"
        SliceType.LINKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_LINKER
(*
/// 102 bp inline
let longInline = 
    makeSimpleSlice 
        (Dna "ATGTCTCAGAACGTTTACATTGTATCGACTGCCAGAACCCCAATTGGTTCATTCCAGGGTTCTCTATCCTCCAAGACAGCAGTGGAATTGGGTGCTGTTATG")
        "oBar"
        SliceType.INLINEST
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_INLINE


let shortInlineWithRabitStart = 
    { shortInline with 
        pragmas = shortInline.pragmas.Add(rabitStart)
        description = shortInline.description+"#rabitstart"
    }
let shortInlineWithRabitEnd = 
    { shortInline with 
        pragmas = shortInline.pragmas.Add(rabitEnd);
        description = shortInline.description+"#rabitend"
    }
let linkerAlice = 
    makeSimpleSlice
        (Dna "GATCGATTAGATCGATAGGCTACG")
        "linkerAlice"
        SliceType.LINKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_LINKER

        *)
let linkerBob = 
    makeSimpleSlice
        (Dna "TTTGGTTTGTAGCGGGGCTTTAGA")
        "linkerBob"
        SliceType.LINKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_LINKER
let linkerCharlie = 
    makeSimpleSlice
        (Dna "ATGATGGGATCGGGATCGGGGGCAGACTTTG")
        "linkerCharlie"
        SliceType.LINKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_LINKER

let linkerDoug = 
    makeSimpleSlice
        (Dna "GATCGATTAGCTTAGATCGTGATCGGTCG")
        "linkerDoug"
        SliceType.LINKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_LINKER

let placeholder = 
    makeSimpleSlice
        (Dna "")
        "placeholder"
        SliceType.LINKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_VIRTUAL

let fuse = 
    makeSimpleSlice
        (Dna "")
        "fusion"
        SliceType.FUSIONST
        EmptyPragmas
        false // from approx
        false // to approx
        false // amplified
        Breed.B_X
