namespace gslc.Tests
open System
open NUnit.Framework
open constants
open Amyris.ErrorHandling
open AstTypes
open AstErrorHandling
open AstAlgorithms
open AstExpansion
open AstAssertions
open LegacyParseTypes

[<TestFixture>]
type TestBootstrapping() = 

    /// Very dumb expansion rule that just reprints the asseembly.
    let reprintAssembly a =
        let s = prettyPrintAssembly a
        printfn "Reprinted assembly: %s" s.String
        s

    /// Expansion rule that always raises an exception.
    let expansionFail _ = failwith "Expansion failed with an exception."

    let bootstrapToTree node =
        match node with Splice(nodes) -> AstTreeHead(Block(nodeWrap (List.ofArray nodes)))

    let bootstrapPhase1NoCapas = bootstrapPhase1 Set.empty

    let compilePhase1NoCapas = GslSourceCode >> (compile (phase1 Set.empty))

    /// Test that a bootstrap operation round-trips successfully.
    let testAssembly source =
        let source = GslSourceCode source
        source
        |> bootstrapPhase1NoCapas None
        |> lift bootstrapToTree
        |> failIfBad (Some(source))
        |> returnOrFail
        |> assertDecompilesTo source.String


    let testPhase1 node =
        let bootstrapOperation = bootstrapExpandLegacyAssembly Error reprintAssembly bootstrapPhase1NoCapas
        executeBootstrap bootstrapOperation Serial node

    let testCaptureException node =
        let bootstrapOperation = bootstrapExpandLegacyAssembly Error expansionFail bootstrapPhase1NoCapas
        executeBootstrap bootstrapOperation Serial node

    [<SetUp>]
    member x.SetUp() = initGlobals()


    [<Test>]
    member x.TestBootstrapAssembly() =
        testAssembly "gFOO ; ### ; ~ ; gFOO[1S:30E] {#name foo}"

    [<Test>]
    member x.SmokeTestFullPhase1Bootstrap() =
        let source = "gFOO ; ### ; ~ ; gFOO[1S:30E] {#name foo}"
        let compiledTree = 
            compilePhase1NoCapas source
            |> failIfBad (Some(GslSourceCode source))
            |> returnOrFail
        compiledTree
        |> testPhase1
        |> failIfBad None
        |> returnOrFail
        |> assertDecompilesTo source

        printfn "Source in: %s" source

    [<Test>]
    member x.TestCaptureExpansionFailure() =
        let source = "gFOO" // doesn't matter what's in here for this test
        compilePhase1NoCapas source
        |> failIfBad (Some(GslSourceCode source))
        |> returnOrFail
        |> testCaptureException
        |> assertFail (Error) (Some "Expansion failed with an exception.")
        |> ignore

