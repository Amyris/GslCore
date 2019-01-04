namespace gslc.Tests
open System
open NUnit.Framework
open Amyris.ErrorHandling
open AstTypes
open AstFixtures
open AstAssertions
open AstAlgorithms
open AstProcess
open AstErrorHandling
open constants

[<TestFixture>]
type TestParsing() = 

    [<Test>]
    member x.TestParseEmpty() =
        assertRoundtrip "" []

    [<Test>]
    member x.TestParseLet() =
        assertRoundtrip "let foo = 1\n" [fooEqual1]

    /// tests of integer expressions
    [<Test>]
    member x.TestIntegerExps() =
        testExpectedReprinting
            "let foo = 1 + 1\n"
            "let foo = (1 + 1)\n" // reprinting binary expressions always unambiguously parenthesizes

        testExpectedReprinting
            "let foo = 1 + 1 + 1\n"
            "let foo = ((1 + 1) + 1)\n"

        testExpectedReprinting
            "let foo = 1 + 1 * 1\n"
            "let foo = (1 + (1 * 1))\n"

        testExpectedReprinting
            "let foo = 1 / 1 * 1\n"
            "let foo = ((1 / 1) * 1)\n"

    [<Test>]
    member x.TestIntegerExpsWithVariables() =
        testExpectedReprinting
            "let foo = 1\nlet bar = &foo + 2\n"
            "let foo = 1\nlet bar = (&foo + 2)\n"

    [<Test>]
    member x.TestParseSimplePart() =
        assertRoundtrip "gFOO" [assemble [fooGenePart]]

    [<Test>]
    member x.TestParsePartWithHyphen() =
        assertRoundtrip "gFOO-BAR" [assemble [createGenePart "gFOO-BAR"]]

    [<Test>]
    member x.TestParsePartWithComma() =
        assertRoundtrip "gFOO,BAR" [assemble [createGenePart "gFOO,BAR"]]

    [<Test>]
    member x.TestParsePartWithMod() =
        assertRoundtrip "gFOO[~20:~200]" [assemble [fooGeneWithSlice]]

    [<Test>]
    member x.TestParsePartWithPragma() =
        assertRoundtrip "gFOO {#name foo}" [assemble [fooGeneWithPragma]]

    [<Test>]
    member x.TestParsePragma() =
        assertRoundtrip "#name foo" [namePragmaFoo]

    [<Test>]
    member x.TestSeveralParts() =
        assertRoundtrip
            "gFOO ; gFOO[~20:~200] ; gFOO {#name foo}"
            [assemble [fooGenePart; fooGeneWithSlice; fooGeneWithPragma]]

    [<Test>]
    member x.TestNoTrailingSemicolons() =
        let source = "gFOO ;"
        source
        |> GslSourceCode
        |> compile (validate checkParseError)
        |> assertFail ParserError (Some("syntax error"))
        |> ignore

    [<Test>]
    member x.TestVariableUse() =
        assertRoundtrip "&foo" [assemble [partVariable "foo"]]

    [<Test>]
    member x.TestVariableRepertoire() =
        // if we just said 1.0 it wouldn't string round-trip as %f gives a lot of digits and ToString just prints 1
        let text = """
let int = 1
let float = 1.000000
let string = "hello"
let part = gFOO
let assembly = gFOO ; gFOO[~20:~200]
"""
        let correctBindings =
            [ variableize "int" (wrapInt 1);
              variableize "float" (wrapFloat 1.0);
              variableize "string" (wrapString "hello");
              variableize "part" fooGenePart;
              variableize "assembly" (assemble [fooGenePart; fooGeneWithSlice]) ]
        assertRoundtrip text correctBindings

    [<Test>]
    member x.TestFunctionDeclaration() =
        let text = """
let foo(bar) =
    &bar
end
"""
        let funDef = functionalize "foo" ["bar"] (bootstrapParseOnly "&bar")

        assertRoundtrip text [funDef]



    [<Test>]
    member x.TestFunctionCall() =
        let arg = typedValue IntType (wrapInt 1)
        let fCall = FunctionCall(nodeWrap {name="foo"; args=[arg]})
        assertRoundtrip "foo(1)" [fCall]

    [<Test>]
    member x.TestFunctionCallManyArgs() =
        let source = "foo(1, 1.000000, \"hello\", gFOO, (gFOO))"
        let args = [
            typedValue IntType (wrapInt 1);
            typedValue FloatType (wrapFloat 1.0);
            typedValue StringType (wrapString "hello");
            typedValue PartType (fooGenePart);
            typedValue PartType (assemble [fooGenePart]);
            ]
    
        let fCall = FunctionCall(nodeWrap {name="foo"; args=args})
        assertRoundtrip source [fCall]

    [<Test>]
    member x.TestAllBaseParts() =
        let partSource = [
            "@fooPart";
            "###";
            "/GATCGTCGA/";
            "&fooVar";
            "/$UUU/";
            "/$*/";
            "~";
            "gFOO";
            "&fooVar";
            ]

        let text = String.Join(" ; ", partSource)
        let parts = [
            basePartWrap (PartId(nodeWrap "fooPart"));
            basePartWrap (Marker(nodeWrap ()));
            basePartWrap (InlineDna(nodeWrap "GATCGTCGA"));
            partVariable "fooVar";
            basePartWrap (InlineProtein(nodeWrap "UUU"));
            basePartWrap (InlineProtein(nodeWrap "*"));
            basePartWrap (HetBlock(nodeWrap ()));
            fooGenePart;
            partVariable "fooVar";
            ]
        assertRoundtrip text [assemble parts]

    [<Test>]
    member x.TestSubassembly() =
        let source = "(@fooPart ; gFOO) ; &fooVar"
        let subAssem = assemble [basePartWrap (PartId(nodeWrap "fooPart")); fooGenePart]
        assertRoundtrip source [assemble [subAssem; partVariable "fooVar"]]

    [<Test>]
    member x.TestSubblocks() =
        let source = """
let foo = gFOO
do
    let bar = 2
    &foo[&bar:20]
    do
        let baz = 3
        pBAZ
    end
end
"""
        // just smoke test that this round-trips for now
        sourceCompareTest (promote id) source source

    [<Test>]
    member x.TestDocstrings() =
        let source = """
/// I'm a docstring for the following assembly.
gFOO
do
    /// I'm a docstring for an assembly in an inner scope.
    pBAR
end"""
        sourceCompareTest (promote id) source source

    // Attempts at starting some L2 parsing tests
    [<Test>]
    member x.TestL2ImplicitPromoterSwap() =
        let source = "pTDH3>gADH1"
        sourceCompareTest (promote id) source source

    [<Test>]
    member x.TestL2ExplicitPromoterSwap() =
        let source = "gHO^ ; pTDH3>gADH1"
        sourceCompareTest (promote id) source source

    [<Test>]
    member x.TestL2ExplicitMultiplePromoterSwap() =
        let source = "gHO^ ; pTDH3>gADH1 ; pSLN1>gADH6"
        sourceCompareTest (promote id) source source

    [<Test>]
    member x.TestL2Knockout() =
        let source = "gHO^"
        sourceCompareTest (promote id) source source

    [<Test>]
    member x.TestL2ImplicitPromoterSwapRabit() =
        let source = "@R41811>gADH1"
        sourceCompareTest (promote id) source source

//    [<Test>]
//    member x.TestL2ImplicitPromoterSwapAssembly() =
//        let source = "(!gERG10 ; !pFBA1 ; pSLN1)>gADH1"
//        sourceCompareTest (promote id) source source

    [<Test>]
    member x.TestL2ImplicitPromoterSwapVariable() =
        let source = """
let prom = /GTGGTGACTATAGCTATGCTAGTGCTCGCTAAATAGCCTGA/
&prom>gADH1
"""
        sourceCompareTest (promote id) source source

    
