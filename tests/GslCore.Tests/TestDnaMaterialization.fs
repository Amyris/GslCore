namespace GslCore.Tests
open System.IO
open NUnit.Framework
open Amyris.Bio.biolib
open LegacyParseTypes
open commonTypes
open pragmaTypes
open Amyris.ErrorHandling
open constants
open DesignParams
open RefGenome
open AssemblyTestSupport

/// One part design request and test expectation w.r.t reference
type private PartTest = { ppp:PPP; gp:GenePartWithLinker ; revPart:bool ; revReference:bool}

/// Tests for DNA part retrieval from genome
[<TestFixture>]
type DnaMaterialization() = 
    /// location of test gslc_lib fixtures
    let testLibDir1 = @"../../../../TestGslcLib"
    let testLibDir2 = @"../../../../../TestGslcLib"

    let testLibDir =
        if System.IO.Directory.Exists testLibDir1 then testLibDir1
            else testLibDir2

    do
        pragmaTypes.finalizePragmas []
    let refGenomePragma = 
        match buildPragma "refgenome" ["TestGenome2"] with
        | Result.Ok (p,_) -> p 
        | _ -> failwithf "Failure to build refgenome TestGenome2"

    let pc = PragmaCollection(["refgenome",refGenomePragma] |> Map.ofList)

    /// We don't need much from an assembly so ok to leave it mostly empty
    let emptyAssembly = 
        {   parts = []
            name = None
            uri = None
            linkerHint = ""
            pragmas = pc
            designParams = initialDesignParams
            capabilities = Set.empty
            docStrings = []
            sourcePosition = []
        }

    /// this test gene is on the fwd / W strand
    let tADH1 = {gene = "tADH1"; mods = []; where = []}
    /// this test gene is on the rev / C strand
    let tABC1 = {gene = "tABC1"; mods = []; where = []}

    // general retrieval parameters that are gene agnostic
    let gd = new RefGenome.GenomeDef(testLibDir,"TestGenome2")
    let verbose = false
    let rgs:GenomeDefs = [("TestGenome2",gd)] |> Map.ofList
    let library:SequenceLibrary = Map.empty
    let a:Assembly = emptyAssembly
    let dnaSource = "TestGenome2"


    // get the raw DNA we are going to compare retrieval against. First chromosome
    let testGenome2Chrom1 = 
        Path.Combine(testLibDir,"TestGenome2","TestGenome2.fsa")
        |> fastaStream 
        |> Seq.head 
        |> snd

    let materializeOne gp ppp =
        DnaCreation.expandGenePart 
            verbose
            rgs
            library
            a
            dnaSource
            ppp
            gp

    /// get dna beetween sourceFr and sourceTo
    let liftDnaFromChrom (dna:DNASlice) =
        assert(dna.sourceFr <= dna.sourceTo)
        let left = dna.sourceFr
        let right = dna.sourceTo

        testGenome2Chrom1.[left/1<ZeroOffset>..right/1<ZeroOffset>]

    /// retrieve one part and check the coordinates match the DNA returned independently
    let checkOne (test:PartTest)= 
        let dna = materializeOne test.gp test.ppp
        let fromDna = liftDnaFromChrom dna
        Assert.AreEqual(fromDna,(if test.revReference then dna.dna.RevComp() else dna.dna).str)

    let checkOneWithProcAssembly testName (template:DNASlice list) (test:PartTest) =
        let dna = materializeOne test.gp test.ppp

        // drop test part into template where marker is located
        let parts = 
            template 
            |> List.map (fun part -> 
                            if part = placeholder then 
                                dna // swap in test part
                            else 
                                part
                        )
        
        let placeholderIndex = template |> List.findIndex (fun part -> part = placeholder)

        let _primers,slices = 
            PrimerCreation.procAssembly
                    false
                    initialDesignParams
                    testName
                    []
                    []
                    []
                    parts

        let dna = slices |> List.item placeholderIndex
        let fromDna = liftDnaFromChrom dna
        Assert.AreEqual(fromDna,(if test.revReference then dna.dna.RevComp() else dna.dna).str)

    let makeTest gp revPart revReference =
        let gpWithLinker = {part = gp; linker = None}
        { 
            gp = gpWithLinker
            ppp = { part = GENEPART gpWithLinker; pr = PragmaCollection(Map.empty); fwd = not revPart}
            revPart = revPart
            revReference = revReference
        }

    let tABC1Prefix = "AAGAAGTTGCATGCGCCTATTATTACTTCAATAGATGGCAAATGGAAAAAG"
    let tADH1Prefix = "GCGAATTTCTTATGATTTATGATTTTTATTATTAAATAAGTTATAAAAAA"

    // makeTest part revPart revReference
    let test1 = makeTest tADH1 false false
    let test2 = makeTest tABC1 false true
    let test3 = makeTest tADH1 true  true
    let test4 = makeTest tABC1 true false

    let testContext1 = [placeholder ; linkerAlice ; dFoo]
    let testContext2 = [linkerAlice ; placeholder ; dFoo]

    [<SetUp>]
    member __.setupPragmas() =
        do
        // initialize pragmas
        pragmaTypes.finalizePragmas []

    [<Test>]
    /// Check basic sequence
    member __.ChecktADH1StartsWith() =
        let dna = materializeOne test1.gp test1.ppp
        Assert.AreEqual(tADH1Prefix,dna.dna.[0..tADH1Prefix.Length-1].str)

    [<Test>]
    /// Check basic sequence
    member __.CheckRevtADH1EndsWith() =
        let dna = materializeOne test3.gp test3.ppp
        Assert.IsTrue(dna.dna.RevComp().str.StartsWith(tADH1Prefix))

    [<Test>]
    /// Check basic sequence
    member __.ChecktABC1StartsWith() =
        let dna = materializeOne test2.gp test2.ppp
        Assert.AreEqual(tABC1Prefix,dna.dna.[0..tABC1Prefix.Length-1].str)

    [<Test>]
    /// Check basic sequence
    member __.CheckRevtABC1EndsWith() =
        let dna = materializeOne test4.gp test4.ppp
        Assert.IsTrue(dna.dna.RevComp().str.StartsWith(tABC1Prefix))

    [<Test>]
    /// fwd gene in genome used in fwd orientation
    member __.CheckFromCoordsFwdGeneFwd() =
        checkOne test1

    [<Test>]
    /// rev gene in genome used in fwd orientation
    member __.CheckFromCoordsRevGeneFwd() =
        checkOne test2

    [<Test>]
    /// fwd gene in genome used in rev orientation
    member __.CheckFromCoordsFwdGeneRev() =
        checkOne test3

    [<Test>]
    /// rev gene in genome used in rev orientation
    member __.CheckFromCoordsRevGeneRev() =
        checkOne test4

    [<Test>]
    member __.CheckFromCoordsProcAssembly1() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly1" testContext1 test1

    [<Test>]
    member __.CheckFromCoordsProcAssembly2() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly2" testContext1 test2

    [<Test>]
    member __.CheckFromCoordsProcAssembly3() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly3" testContext1 test3

    [<Test>]
    member __.CheckFromCoordsProcAssembly4() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly4" testContext1 test4

    [<Test>]
    member __.CheckFromCoordsProcAssembly1B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly1B" testContext2 test1

    [<Test>]
    member __.CheckFromCoordsProcAssembly2B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly2B" testContext2 test2

    [<Test>]
    member __.CheckFromCoordsProcAssembly3B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly3B" testContext2 test3

    [<Test>]
    member __.CheckFromCoordsProcAssembly4B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly4B" testContext2 test4
