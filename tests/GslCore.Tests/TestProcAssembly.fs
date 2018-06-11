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

    let verbose = false  // enable for detailed (very detailed) output from procAssembly

    do
        // initialize pragmas
        pragmaTypes.finalizePragmas []

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
        // simulate what compiler will do cleaning up inline seqs
        // that are too short to be treated as inlines
        let cleanedSlices = cleanLongSlicesInPartsList pragmaTypes.EmptyPragmas slices
        PrimerCreation.procAssembly
            verbose // debug
            designParams
            testName
            [] // prev
            []  // slice out
            []  // (primersOut:DivergedPrimerPair list)
            cleanedSlices

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

    /// perform one test and check output pattern and sequences
    let runOne name slicesIn pattern seqs =
        let dpps,slices = 
            doProcAssemblyLongOligos // NB long oligos
                name
                slicesIn
        checkPattern pattern dpps
        checkSequence seqs slices
            
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
        //   longInline gets built here by amplification, so execpt
        //  auto fuse on each end
        //  <------------------------------------o
        //                o------------------------------------>
        //                |------- inline -------|
        runOne
            "testLongInline" 
            [ linkerAlice; oBar ; longInline; oBar ; linkerDoug]
            "DGDGDGD"
            [ linkerAlice; oBar ; fuse; longInline; fuse; oBar; linkerDoug]
    
    [<Test>]
    member __.testInlineWithInline() =
        runOne
            "testInlineWithInline" 
            [ linkerAlice; oBar ; longInlineInline; oBar ; linkerDoug]
            "DGDGD"
            [ linkerAlice; oBar ; longInlineInline; oBar; linkerDoug]

    [<Test>]
    member __.testInlineWithAmp() =
        // In this case, the long inline sequence should be made by amplification
        //           o--------->             o-------->
        // ----------------+------ inline --------+--------------
        //           <----------o            <--------o

        runOne
            "testInlineWithAmp" 
            [ linkerAlice; oBar ; longInlineAmp; oBar ; linkerDoug]
            "DGDGDGD"
            [ linkerAlice; oBar ; fuse ; longInlineAmp; fuse ; oBar; linkerDoug]

    [<Test>]
    member __.testDoubleLongInlineWithAmp() =
        // In this case, both long inline sequences should be made by amplification
        //           --------->               -------->           <-----------
        // ----------------+------ inline --------+----- inline -----+-------
        //           ----------              <--------         --------->
        runOne
            "testDoubleLongInlineWithAmp" 
            [ linkerAlice; oBar ; longInlineAmp; longInlineAmp ; oBar ; linkerDoug]
            "DGDGDGDGD"
            [ linkerAlice; oBar ; fuse ; longInlineAmp; fuse; longInlineAmp; fuse ; oBar; linkerDoug]

    [<Test>]
    member __.testDoubleMediumInlineWithAmp() =
        // same as long case above but medium (under automatic threshold) but with amp so 
        // should fuse and amplify them
        runOne
            "testDoubleMediumInlineWithAmp" 
            [ linkerAlice; oBar ; mediumInlineAmp; mediumInlineAmp ; oBar ; linkerDoug]
            "DGDGDGDGD"
            [ linkerAlice; oBar ; fuse ; mediumInlineAmp; fuse; mediumInlineAmp; fuse ; oBar; linkerDoug]

    [<Test>]
    member __.testDoubleMediumLong1InlineWithAmp() =
        // mixed long and short inline sequences tagged for amplification
        runOne
            "testDoubleMediumInlineWithAmp" 
            [ linkerAlice; oBar ; mediumInlineAmp; longInlineAmp ; oBar ; linkerDoug]
            "DGDGDGDGD"
            [ linkerAlice; oBar ; fuse ; mediumInlineAmp; fuse; longInlineAmp; fuse ; oBar; linkerDoug]

    [<Test>]
    member __.testDoubleMediumLong2InlineWithAmp() =
        // mixed long and short inline sequences tagged for amplification other way around
        runOne
            "testDoubleMediumInlineWithAmp" 
            [ linkerAlice; oBar ; longInlineAmp ; mediumInlineAmp; oBar ; linkerDoug]
            "DGDGDGDGD"
            [ linkerAlice; oBar ; fuse ; longInlineAmp ; fuse; mediumInlineAmp; fuse ; oBar; linkerDoug]

    [<Test>]
    member __.testDoubleSmallNoAmpLong1InlineLinkerFlanked() =
        // mixed long and short inline sequences with medium sequence NOT tagged for amp
        // but linkers adjacent to both slices
        // FAILING
        runOne
            "testDoubleSmallNiAmpLong1InlineLinkerFlanked" 
            [ linkerAlice; smallInlineFuse; longInlineAmp ; linkerDoug]
            "DGDGD"
            [ linkerAlice; smallInlineFuse; fuse; longInlineAmp; linkerDoug]

    [<Test>]
    member __.testDoubleMediumNoAmpLong1InlineWithAmp() =
        // mixed long and short inline sequences with medium sequence NOT tagged for amp
        // FAILING
        runOne
            "testDoubleMediumNoAmpInlineWithAmp" 
            [ linkerAlice; oBar ; linkerBob ; mediumInlineFuse; longInlineAmpFuse ; oBar ; linkerDoug]
            "DGDGDGDGD"
            [ linkerAlice; oBar ; linkerBob ; mediumInlineFuse; fuse; longInlineAmp; fuse ; oBar; linkerDoug]

    [<Test>]
    member __.testRabitEnd() =
        runOne
            "testRabitEnd" 
            [ linkerAlice; uFoo ; shortInlineWithRabitEnd ; linkerCharlie ; dFoo ; linkerDoug]
            "DGSDGD"
            [ linkerAlice; uFoo ; shortInlineWithRabitEnd ; linkerCharlie ; dFoo ; linkerDoug]

    [<Test>]
    member __.testRabitEndPostMarker() =
        runOne
            "testRabitEndPostMarker" 
            [ linkerAlice; uFoo ; linkerBob ;  marker ; shortInlineWithRabitEnd ; linkerCharlie ; dFoo ; linkerDoug]
            "DGDGSDGD"
            [ linkerAlice; uFoo ; linkerBob ;  marker; shortInlineWithRabitEnd ; linkerCharlie ; dFoo ; linkerDoug]

    [<Test>]
    member __.testRabitEndPostMarkerExtraSlice() =
        runOne
            "testRabitEndPostMarkerExtraSlice" 
            [ linkerAlice; uFoo ; linkerBob ;  marker ; shortInlineWithRabitEnd ; linkerCharlie ; oBar ; dFoo ; linkerDoug]
            "DGDGSDGDGD"
            [ linkerAlice; uFoo ; linkerBob ;  marker; shortInlineWithRabitEnd ; linkerCharlie ; oBar; fuse ; dFoo ; linkerDoug]