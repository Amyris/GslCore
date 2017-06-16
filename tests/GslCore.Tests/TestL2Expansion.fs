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
        >=> (validate validateNoAssemblyInL2Promoter)

//    let phase1AndL2 =
//        >=> phase1 [] // empty list as we shouldn't need any capas for this test
//        >=> expandLevel2 [] l2Providers rgs // you'll need to gather your L2 provider from the module where its defined, as well as the appropriate reference genomes
    
    [<Test>]
    member x.TestDetectAssemblyInL2Promoter() =
        let errorText = "Unsupported use of an Assembly."
        let source = """
(!gERG10 ; !pFBA1 ; pSLN1)>gADH1
let myAssembly = (pSLN1; mERG10 ; pTDH3)
&myAssembly>gADH1
gHO^ ; &myAssembly>gACS1""" |> GslSourceCode
        compile phase1WithL2Validation source 
        |> assertFailMany 
            [L2ExpansionError;L2ExpansionError;L2ExpansionError] 
            [(Some errorText);(Some errorText);(Some errorText)]
        |> ignore


    [<Test>]
    member x.TestInlineL2PromoterSwap() =
        let errorText = "found '>', expected one of ['EOF']."
        let source = GslSourceCode("!gERG10 ; !pFBA1 ; pSLN1>gADH1")
        compile phase1WithL2Validation source 
        |> assertFail ParserError (Some errorText)
        |> ignore


    [<Test>]
    member x.TestNativeL2PromoterSwap() =
        let source = """
pTDH3>gADH1
let myPromoter = pFBA1
&myPromoter>gSLN1
gHO^ ; pGAL1>mERG10""" |> GslSourceCode
        compile phase1WithL2Validation source 
        |> returnOrFail 
        |> ignore


    [<Test>]
    member x.TestRabitL2PromoterSwap() =
        let source = """
@R41811>gADH1
let myPromoter = @R56707
&myPromoter>gSLN1
gHO^ ; @R52888>mERG10""" |> GslSourceCode
        compile phase1WithL2Validation source 
        |> returnOrFail 
        |> ignore


    [<Test>]
    member x.TestCustomSeqL2PromoterSwap() =
        let source = """
let myPromoter = /$GTACVMPLQVGSASKYWALKERMYYQACLPH/
&myPromoter>gSLN1
gHO^ ; &myPromoter>mERG10""" |> GslSourceCode
        compile phase1WithL2Validation source 
        |> returnOrFail 
        |> ignore
    
    
        

