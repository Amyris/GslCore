namespace GslCore.Tests
open System
open System.IO
open NUnit.Framework
open Amyris.Bio.biolib
open gslcProcess
open LegacyParseTypes
open commonTypes
open AstTypes
open pragmaTypes
open Amyris.Dna
open Amyris.ErrorHandling
open constants
open DesignParams
open RefGenome
open AssemblyTestSupport




/// Tests for DNA part retrieval from genome
[<TestFixture>]
type DnaMaterialization() = 
    /// location of test gslc_lib fixtures
    let testLibDir1 = @"../../../../TestGslcLib"
    let testLibDir2 = @"../../../../../TestGslcLib"

    let testLibDir =
        if System.IO.Directory.Exists testLibDir1 then testLibDir1
            else testLibDir2

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

    /// retrieve one part and check the coordinates match the DNA returned independently
    let checkOne gp ppp flip =
        let dna =
            DnaCreation.expandGenePart 
                verbose
                rgs
                library
                a
                dnaSource
                ppp
                gp

        let left = min dna.sourceFr dna.sourceTo
        let right = max dna.sourceFr dna.sourceTo

        let fromDna = testGenome2Chrom1.[left/1<ZeroOffset>..right/1<ZeroOffset>]
        Assert.AreEqual(fromDna,(if flip then dna.dna.RevComp() else dna.dna).str)

    [<SetUp>]
    member __.setupPragmas() =
        do
        // initialize pragmas
        pragmaTypes.finalizePragmas []

    [<Test>]
    /// fwd gene in genome used in fwd orientation
    member __.CheckFromCoordsFwdGeneFwd() =

        let gp:GenePartWithLinker = {part = tADH1; linker = None}
        let ppp:PPP = { part = GENEPART gp; pr = PragmaCollection(Map.empty); fwd = true}

        checkOne gp ppp false

    [<Test>]
    /// rev gene in genome used in fwd orientation
    member __.CheckFromCoordsRevGeneFwd() =

        let gp:GenePartWithLinker = {part = tABC1; linker = None}
        let ppp:PPP = { part = GENEPART gp; pr = PragmaCollection(Map.empty); fwd = true}

        checkOne gp ppp true

    [<Test>]
    /// fwd gene in genome used in rev orientation
    member __.CheckFromCoordsFwdGeneRev() =

        let gp:GenePartWithLinker = {part = tADH1; linker = None}
        let ppp:PPP = { part = GENEPART gp; pr = PragmaCollection(Map.empty); fwd = false} // reverse usage

        checkOne gp ppp true

    [<Test>]
    /// rev gene in genome used in rev orientation
    member __.CheckFromCoordsRevGeneRev() =

        let gp:GenePartWithLinker = {part = tABC1; linker = None}
        let ppp:PPP = { part = GENEPART gp; pr = PragmaCollection(Map.empty); fwd = false} // reverse usage

        checkOne gp ppp false


    [<Test>]
    member __.CheckFromCoordsProcAssembly() =
        let assembly =
            compileOne "#refgenome TestGenome2
tADH1"
            |> List.head

        match assembly with
        | Assembly nodes ->
            printfn "XXX: %A" nodes
            ()
        | _ -> failwithf "expected assembly node"
