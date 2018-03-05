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
        description = "Description:"+sliceName
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
    let doProcAssembly testName (slices:DNASlice list) =
        PrimerCreation.procAssembly
            false
            DesignParams.initialDesignParams // defaults
            testName
            [] // prev
            []  // slice out
            []  // (primersOut:DivergedPrimerPair list)
            slices

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
    let checkSequence (expected:DNASlice list) (actual:DNASlice list) =
        if expected.Length <> actual.Length then
            printfn "checkSequence about to fail\nexplen=%d actualLen=%d" expected.Length actual.Length
            printfn "checkSequence: actual=%A" actual

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
    let dumpSlices (slices:DNASlice list) =
        printf "%s" (String.Join(";",[for s in slices -> s.sliceName]))

    [<Test>]
    /// Equivalent of gFOO^
    member __.koNoLinkers() =
        let parts = [ uFoo ; marker ; dFoo]
        let dpps,slices = doProcAssembly "SimpleKO" parts
        checkPattern "GDGDG" dpps
        printfn "slice order expected: %s" (String.Join(";",[ for s in [ uFoo ; fuse ; marker ; fuse ; dFoo] -> s.sliceName]))
        printfn "slice order returned: %s" (String.Join(";",[ for s in slices -> s.sliceName]))
        checkSequence [ uFoo ; fuse ; marker ; fuse ; dFoo] slices

    [<Test>]
    /// Equivalent of uFoo; /inline/ ; dFoo
    member __.koInlineNoLinkers() =
        let dpps,slices = doProcAssembly "SimpleKOInline" [ uFoo ; shortInline ; dFoo]
        checkPattern "GDG" dpps
        checkSequence [ uFoo ;  shortInline ; dFoo] slices

    [<Test>]
    member __.koLinkers() =
        let dpps,slices = 
            doProcAssembly 
                "SimpleKOInline" 
                [ linkerAlice; uFoo ; linkerBob ;  marker ; linkerCharlie ; dFoo ; linkerDoug]
        checkPattern "DGDGDGD" dpps
        dumpSlices slices
        checkSequence [ linkerAlice; uFoo ; linkerBob ;  marker ; linkerCharlie ; dFoo ; linkerDoug] slices