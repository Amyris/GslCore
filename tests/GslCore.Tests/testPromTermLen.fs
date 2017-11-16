module testPromTermLen
/// Test #promoterlen #terminatorlen work

open System
open NUnit.Framework
open commandConfig
open LegacyParseTypes
open constants
open commonTypes
open gslc
open Amyris.Dna
open pragmaTypes
open System
open NUnit.Framework
open Amyris.ErrorHandling
open AstTypes
open AstFixtures
open AstAssertions
open AstAlgorithms
open AstExpansion
open AstProcess
open AstErrorHandling
open BasicL2ExpansionProvider
open constants
open PluginTypes
open LexAndParse

let phase1WithL2Validation = 
        phase1 Set.empty
        >=> (validate validateNoAssemblyInL2Promoter)

let phase2WithData (ga:GlobalAssets) =
        phase2 false (Some(10)) false false Set.empty [] ga.rgs ga.codonProvider

let compiler (ga:GlobalAssets) gslText =
    lexAndParse false gslText
    >>= phase1 Set.empty // legal capas
    >>= prepPhase2 ga.rgs ga.seqLibrary
    >>= phase2WithData ga
    >>= convertAndGatherAssemblies 

let compiler2 =
    configureGslc [] [||]
    >?> checkInputFileList
    >?> runCompiler
    >?> handleCompileResult
    >?> doDnaMaterialization

let compilePhase1NoCapas = GslSourceCode >> (compile (phase1 Set.empty))

[<TestFixture>]
type TestPromTermLen() = 
    let legalCmdLineArgs = { builtins = Map.empty ; builtinsWithProc = Map.empty ; fromPlugins = Map.empty}
    let s = ProcessCmdLineArgs.configure true legalCmdLineArgs [] []
    [<Test>]
    member x.TestDetectAssemblyInL2Promoter() =
        let errorText = "Unsupported use of an Assembly."
        let x = """#promoterlen 750\
                        tADH1""" 
                    |> compilePhase1NoCapas
                    |> phase2 true (Some 10) false false Set.empty []  s.ga.rgs (s.ga.codonProvider)

        printfn "%A" x
        ()