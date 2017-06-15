namespace gslc.Tests
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
open constants

[<TestFixture>]
type TestL2Expansion() = 

    let phase1WithL2Validation = 
        phase1 Set.empty
        >=> validateNoAssemblyInL2Promoter
    
    [<Test>]
    member x.TestDetectAssemblyInL2Promoter() =
        let errorText = "Unsupported use of an Assembly."
        let source = GslSourceCode("(!gERG10 ; !pFBA1 ; pSLN1)>gADH1")

        compile phase1WithL2Validation source 
        |> assertFail L2ExpansionError (Some errorText)


//        let tree = lexparse source |> returnOrFail
//        compile failOnAssemblyInL2Promoter source
//        |> ignore
//        assertFail
//            L2ExpansionError (Some errorText)
//            failOnAssemblyInL2Promoter tree
//        |> ignore