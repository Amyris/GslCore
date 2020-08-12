namespace GslCore.Tests
open System
open LegacyParseTypes
open NUnit.Framework
open gslcProcess
open commonTypes
open AssemblyTestSupport
open pragmaTypes
open Amyris.ErrorHandling

[<TestFixture>]
type TestMapRyseLinkers() =

    let verbose = false  // enable for detailed (very detailed) output from mapRyseLinkers

    do
        // initialize pragmas
        pragmaTypes.finalizePragmas []

    let makePragma name values =
        match buildPragma name values with
        | Ok (p,[]) ->
            let map = [ p.name,p] |> Map.ofList |> PragmaCollection
            map
        | _ -> failwith "building pragma"
    /// perform one test and check output pattern and sequences
    let runOne (name:string) isMegastitch (linkersIn:(DNASlice list*DNASlice list)) slicesIn expected =
        /// Boring default options
        let opts : ParsedOptions = { quiet = false
                                     refStrain = "cenpk"
                                     libDir = "whatever"
                                     iter = true
                                     onlyPhase1 = false
                                     doParallel = false
                                     verbose = verbose
                                     noPrimers = false
                                     lexOnly = false
                                     refList = false
                                     refDump = None
                                     listPlugins = false
                                     doHelpPragmas = false }

        let leftLinkers,rightLinkers = linkersIn
        let linkers = [for l in leftLinkers@rightLinkers -> l.sliceName , { RYSELinker.name = l.sliceName ; dna = l.dna }] |> Map.ofList


        /// Wrap up in a generic assembly
        let assemblyIn : DnaAssembly = { id = None
                                         dnaParts = slicesIn
                                         name = name
                                         uri = None
                                         linkerHint =
                                             String.Join(",",[for l in leftLinkers -> l.sliceName]) + "|" +
                                             String.Join(",",[for l in rightLinkers -> l.sliceName])
                                         pragmas = if isMegastitch then EmptyPragmas else makePragma "platform" ["stitch"]
                                         designParams = DesignParams.initialDesignParams
                                         docStrings = []
                                         materializedFrom = {
                                             Assembly.name = None
                                             parts = []
                                             uri = None
                                             linkerHint = ""
                                             pragmas = EmptyPragmas
                                             designParams = DesignParams.initialDesignParams
                                             capabilities = Set.empty
                                             docStrings = []
                                             sourcePosition = []
                                         }
                                         tags = Set.empty
                                         topology = Linear}

        let updatedAssembly = ryse.mapRyseLinkers opts Map.empty linkers assemblyIn
        // Check expected and actual slice output
        SharedSliceTesting.checkSequence expected updatedAssembly.dnaParts


    [<Test>]
    member __.SinglePart() =
        runOne "SinglePart"
                false // is stitch
                ([linkerAlice ; linkerBob],[]) // A and B part linkers
                [uFoo]
                [linkerAlice ; uFoo ; linkerBob]

    [<Test>]
    member __.TwoParts() =
        runOne "TwoParts"
                false // is stitch
                ([linkerAlice ; linkerBob ; linkerCharlie],[]) // A and B part linkers
                [uFoo ; dFoo]
                [linkerAlice ; uFoo ; linkerBob ; dFoo ; linkerCharlie]

    [<Test>]
    member __.TwoPartsPlusShortInline() =
        runOne "TwoPartsShortInline"
                false // is stitch
                ([linkerAlice ; linkerBob],[]) // A and B part linkers
                [uFoo ; shortInline; dFoo]
                [linkerAlice ; uFoo ; shortInline ; dFoo ; linkerBob]

    [<Test>]
    /// BROKEN - this is a test case for https://gitlab.com/demetrixbio/DemGslc/-/issues/48
    member __.TerminalSlice() =
        
        runOne "TwoPartsShortInline"
                false // is stitch
                ([linkerAlice ; linkerBob],[]) // A and B part linkers
                [ uFoo ; fuse; dFoo ; shortInlineWithRabitEnd]
                [linkerAlice ; uFoo ; dFoo ; shortInline ; linkerBob]

