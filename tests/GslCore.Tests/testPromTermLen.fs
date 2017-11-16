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

let testLibDir = @"../../../../TestGslcLib"

[<TestFixture>]
type TestPromTermLen() = 
    let checkOneGenome name promLen termLen =
        let gd = new RefGenome.GenomeDef(testLibDir,name)

        let part = DnaCreation.translateGenePrefix gd TERMINATOR
        Assert.AreEqual( termLen, part.right.x-part.left.x+1<OneOffset>) // +1 since ends are inclusive

        let part = DnaCreation.translateGenePrefix gd PROMOTER
        Assert.AreEqual( promLen, part.right.x-part.left.x+1<OneOffset>) // +1 since ends are inclusive
        
    [<Test>]
    member x.TestPragmasExist() =
        let checkPragmaExists name =
            Assert.DoesNotThrow (fun () -> returnOrFail (buildPragma name ["250"]) |> ignore) |> ignore

        checkPragmaExists "promlen"
        checkPragmaExists "termlen"

    [<Test>]
    member x.CheckDefaultTerminatorLen() =
        checkOneGenome "TestGenome" constants.promLenDefault termLenDefault

    [<Test>]
    member x.CheckCustomTerminatorLen() =
        checkOneGenome "TestGenome2" 750 250