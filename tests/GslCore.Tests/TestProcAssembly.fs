namespace GslCore.Tests
open System
open NUnit.Framework
open Amyris.ErrorHandling
open gslcProcess
open LegacyParseTypes
open commonTypes
open LexAndParse
open pragmaTypes
open Amyris.Dna
open constants
[<TestFixture>]
type TestProcAssembly() = 

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

    let longInlineAmp = {longInline with pragmas = longInline.pragmas.Add(amp) ; sliceName = longInline.sliceName+"#amp"}
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
    let _doProcAssembly testName (slices:DNASlice list) designParams =
        PrimerCreation.procAssembly
            true // debug
            designParams
            testName
            [] // prev
            []  // slice out
            []  // (primersOut:DivergedPrimerPair list)
            slices

    let doProcAssembly testName (slices:DNASlice list) = 
        _doProcAssembly testName slices DesignParams.initialDesignParams

    let doProcAssemblyLongOligos testName (slices:DNASlice list) = 
        _doProcAssembly testName slices 
            { DesignParams.initialDesignParams with 
              pp = { DesignParams.initialDesignParams.pp with maxLength = 100 }
            }

    /// Check that high level layout of diverged primer pair types meets expectation
    let checkPattern expected (result:DivergedPrimerPair list) =
        let pattern = 
            result |> List.map (fun dpp ->
                                    match dpp with
                                    | DPP _ -> "D"
                                    | GAP _ -> "G"
                                    | SANDWICHGAP _ -> "S"
                               )
                   |> List.reduce(+)
        Assert.AreEqual(expected,pattern)

    let dumpSlices (slices:DNASlice list) =
        printf "%s" (String.Join(";",[for s in slices -> s.sliceName]))

    /// Check that the final DNA sequence contains all the input elements
    let checkSequence (expected:DNASlice list) (actual:DNASlice list) =
        if expected.Length <> actual.Length then
            printfn "========================================="
            printfn "checkSequence about to fail\nexplen=%d actualLen=%d" expected.Length actual.Length
            printfn "checkSequence: expected"
            dumpSlices expected
            printfn "\ncheckSequence: actual"
            dumpSlices actual
            printfn ""

        for e,a in List.zip expected actual do
            match e.sourceFrApprox,e.sourceToApprox with
            | false,false -> 
                Assert.AreEqual(e.dna.str,a.dna,sprintf "expect %s to equal %s" e.sliceName a.sliceName)
            | true,false -> 
                Assert.IsTrue(e.dna.EndsWith(a.dna),sprintf "expect %s to end with %s" e.sliceName a.sliceName)
            | false,true -> 
                Assert.IsTrue(e.dna.StartsWith(a.dna),sprintf "expect %s to start with %s" e.sliceName a.sliceName)
            | _ -> 
                Assert.IsTrue(e.dna.Contains(a.dna),sprintf "expect %s to contain %s" e.sliceName a.sliceName)
    [<Test>]
    /// Equivalent of gFOO^
    member __.koNoLinkers() =
        let parts = [ uFoo ; marker ; dFoo]
        let dpps,slices = doProcAssembly "koNoLinkers" parts
        checkPattern "GDGDG" dpps
        printfn "slice order expected: %s" (String.Join(";",[ for s in [ uFoo ; fuse ; marker ; fuse ; dFoo] -> s.sliceName]))
        printfn "slice order returned: %s" (String.Join(";",[ for s in slices -> s.sliceName]))
        checkSequence [ uFoo ; fuse ; marker ; fuse ; dFoo] slices

    [<Test>]
    /// Equivalent of uFoo; /inline/ ; dFoo
    member __.koInlineNoLinkers() =
        let dpps,slices = doProcAssembly "koInlineNoLinkers" [ uFoo ; shortInline ; dFoo]
        checkPattern "GDG" dpps
        checkSequence [ uFoo ;  shortInline ; dFoo] slices

    [<Test>]
    member __.koLinkers() =
        let dpps,slices = 
            doProcAssembly 
                "koLinkers" 
                [ linkerAlice; uFoo ; linkerBob ;  marker ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDGDGD" dpps
        checkSequence [ linkerAlice; uFoo ; linkerBob ;  marker ; linkerCharlie ; dFoo ; linkerDoug] slices

    [<Test>]
    member __.testRabitStartAdjacentToMarker() =
        let dpps,slices = 
            doProcAssembly 
                "testRabitStartAdjacentToMarker" 
                [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart ; marker ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDSGDGD" dpps
        checkSequence [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart;marker ; linkerCharlie ; dFoo ; linkerDoug] slices
    [<Test>]
    member __.testRabitStartAdjacentToRegularSlice() =
        let dpps,slices = 
            doProcAssembly 
                "testRabitStartAdjacentToRegularSlice" 
                [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart ; oBar ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDSGDGD" dpps
        checkSequence [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart;oBar ; linkerCharlie ; dFoo ; linkerDoug] slices



    // // by default this needs a longer primer than allowed.  Fails ungracefully but once we
    // // have better error handling in place, this is a useful test
    // [<Test>]
    // member __.testInlineDirective() =
    //     let dpps,slices = 
    //         doProcAssembly 
    //             "testInlineDirective" 
    //             [ linkerAlice; oBar ; longInline; oBar ; linkerDoug]
    //     checkPattern "DGDGD" dpps
    //     checkSequence [ linkerAlice; oBar ; longInline;  oBar; dFoo ; linkerDoug] slices

    [<Test>]
    member __.testLongInline() =
        //   longInline gets built here by long primers
        //  <------------------------------------o
        //                o------------------------------------>
        //                |------- inline -------|
        let dpps,slices = 
            doProcAssemblyLongOligos // NB long oligos
                "testLongInline" 
                [ linkerAlice; oBar ; longInline; oBar ; linkerDoug]
        checkPattern "DGDGD" dpps
        checkSequence [ linkerAlice; oBar ; longInline; oBar; linkerDoug] slices
    
    [<Test>]
    member __.testInlineWithInline() =
        let dpps,slices = 
            doProcAssemblyLongOligos // NB long oligos
                "testInlineWithInline" 
                [ linkerAlice; oBar ; longInlineInline; oBar ; linkerDoug]
        checkPattern "DGDGD" dpps
        checkSequence [ linkerAlice; oBar ; longInlineInline; oBar; linkerDoug] slices

    [<Test>]
    member __.testInlineWithAmp() =
        // In this case, the long inline sequence should be made by amplification
        //           o--------->             o-------->
        // ----------------+------ inline --------+--------------
        //           <----------o            <--------o

        let dpps,slices = 
            doProcAssemblyLongOligos // NB long oligos
                "testInlineWithAmp" 
                [ linkerAlice; oBar ; longInlineAmp; oBar ; linkerDoug]
        checkPattern "DGDGDGD" dpps
        checkSequence [ linkerAlice; oBar ; fuse ; longInlineAmp; fuse ; oBar; linkerDoug] slices
   (* 
    [<Test>]
    member __.testRabitEnd() =
        let dpps,slices = 
            doProcAssembly 
                "testRabitEnd" 
                [ linkerAlice; uFoo ; shortInlineWithRabitEnd ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGSDGD" dpps
        checkSequence [ linkerAlice; uFoo ; shortInlineWithRabitEnd ; linkerCharlie ; dFoo ; linkerDoug] slices

    [<Test>]
    member __.testRabitEndPostMarker() =
        let dpps,slices = 
            doProcAssembly 
                "testRabitEndPostMarker" 
                [ linkerAlice; uFoo ; linkerBob ;  marker ; shortInlineWithRabitEnd ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDGSDGD" dpps
        checkSequence [ linkerAlice; uFoo ; linkerBob ;  marker; shortInlineWithRabitEnd ; oBar ; linkerCharlie ; dFoo ; linkerDoug] slices

*)