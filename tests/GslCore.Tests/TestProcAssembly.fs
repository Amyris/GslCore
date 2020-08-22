namespace GslCore.Tests
open System
open NUnit.Framework
open gslcProcess
open commonTypes
open AssemblyTestSupport

module SharedSliceTesting =
    let dumpSlices (slices:DNASlice list) =
        printf "%s" (String.Join(";",[for s in slices -> if s.sliceName <> "" then s.sliceName else s.description]))

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

[<TestFixture>]
type TestProcAssembly() =

    // enable for detailed (very detailed) output from procAssembly. Useful for debugging test cases
    let verbose = true

    do
        // initialize pragmas
        pragmaTypes.finalizePragmas []

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

    /// Check that the final DNA sequence contains all the input elements
    /// perform one test and check output pattern and sequences
    let runOne name slicesIn pattern seqs =
        let dpps,slices =
            doProcAssemblyLongOligos // NB long oligos
                name
                slicesIn
        checkPattern pattern dpps
        SharedSliceTesting.checkSequence seqs slices

    [<Test>]
    /// Equivalent of gFOO^
    member __.koNoLinkers() =
        let parts = [ uFoo ; marker ; dFoo]
        let dpps,slices = doProcAssembly "koNoLinkers" parts
        checkPattern "GDGDG" dpps
        printfn "slice order expected: %s" (String.Join(";",[ for s in [ uFoo ; fuse ; marker ; fuse ; dFoo] -> s.sliceName]))
        printfn "slice order returned: %s" (String.Join(";",[ for s in slices -> s.sliceName]))
        SharedSliceTesting.checkSequence [ uFoo ; fuse ; marker ; fuse ; dFoo] slices

    [<Test>]
    /// Equivalent of uFoo; /inline/ ; dFoo
    member __.koInlineNoLinkers() =
        let dpps,slices = doProcAssembly "koInlineNoLinkers" [ uFoo ; shortInline ; dFoo]
        checkPattern "GDG" dpps
        SharedSliceTesting.checkSequence [ uFoo ;  shortInline ; dFoo] slices

    [<Test>]
    member __.koLinkers() =
        let dpps,slices =
            doProcAssembly
                "koLinkers"
                [ linkerAlice; uFoo ; linkerBob ;  marker ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDGDGD" dpps
        SharedSliceTesting.checkSequence [ linkerAlice; uFoo ; linkerBob ;  marker ; linkerCharlie ; dFoo ; linkerDoug] slices

    [<Test>]
    member __.testRabitStartAdjacentToMarker() =
        let dpps,slices =
            doProcAssembly
                "testRabitStartAdjacentToMarker"
                [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart ; marker ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDSGDGD" dpps
        SharedSliceTesting.checkSequence [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart;marker ; linkerCharlie ; dFoo ; linkerDoug] slices
    [<Test>]
    member __.testRabitStartAdjacentToRegularSlice() =
        let dpps,slices =
            doProcAssembly
                "testRabitStartAdjacentToRegularSlice"
                [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart ; oBar ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDSGDGD" dpps
        SharedSliceTesting.checkSequence [ linkerAlice; uFoo ; linkerBob ;  shortInlineWithRabitStart;oBar ; linkerCharlie ; dFoo ; linkerDoug] slices



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

    // Disabled - failing test - compiler can't handle medium inline sequences like this
    // gGAL3[-750S:-83S] {#fuse} ; gGAL3[-71S:-1S]
    //[<Test>]
    //member __.testDoubleMediumLong2InlineNoAmp() =
    //    // mixed long and short inline sequences tagged for amplification other way around
    //    runOne
    //        "testDoubleMediumInlineNoAmp"
    //        [ linkerAlice; mediumInline ; fuse ; longInlineAmp ; linkerDoug]
    //        "DGDGD"
    //        [ linkerAlice; mediumInline ; fuse; longInlineAmp ; linkerDoug]

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

    [<Test>]
    member __.testSimpleKO() =
        runOne
            "testSimpleKO"
            [ linkerAlice; uFoo ; dFoo ; linkerCharlie]
            "DGDGD"
            [ linkerAlice; uFoo ; fuse ;  dFoo; linkerCharlie]
    [<Test>]
    member __.testFuseThenSlice() =
        runOne
            "testFuseThenSlice"
            [ linkerAlice; uFooFuse ; dFoo ; linkerCharlie]
            "DGDGD"
            [ linkerAlice; uFoo ; fuse ;  dFoo; linkerCharlie]

    [<Test>]
    member __.testTerminalSlice() =
        runOne
            "testTerminalSlice"
            [ linkerAlice; uFoo ; dFoo ; shortInlineWithRabitEnd ; linkerCharlie]
            "DGDGSD"
            [ linkerAlice; uFoo ; fuse ;  dFoo; shortInlineWithRabitEnd ; linkerCharlie]

    [<Test>]
    member __.``internalInlineMarkhell2Case``()  =
        runOne "internalInlineMarkhell2Case"
            [ linkerAlice ; uFoo ; linkerBob ;pBaz ; oBar; linkerCharlie ; shortInlineWithRabitStart; oBar2 ; shortInlineWithRabitEnd; linkerDoug; tShaz ;linkerEmma ]
            "DGDGDGDSGSDGD"
            [ linkerAlice ; uFoo ; linkerBob ;pBaz ; fuse ; oBar; linkerCharlie ; shortInlineWithRabitStart; oBar2 ; shortInlineWithRabitEnd; linkerDoug; tShaz ;linkerEmma ]

