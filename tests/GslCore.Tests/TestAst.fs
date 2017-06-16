namespace gslc.Tests
open System
open NUnit.Framework
open Amyris.ErrorHandling
open AstTypes
open AstErrorHandling
open AstProcess
open AstLinting
open AstFixtures
open AstExpansion
open AstAssertions
open AstAlgorithms
open constants

[<TestFixture>]
type TestLinting() = 

    [<Test>]
    member x.TestDetectOldVariableSyntax() =
        "@foo"
        |> GslSourceCode
        |> compile linters
        |> assertWarn Warning (Some("The syntax for using a variable has changed"))
        |> ignore

    [<Test>]
    member x.TestDetectPushPop() =
        "#push\n#pop"
        |> GslSourceCode
        |> compile linters
        |> assertFailMany
            [PragmaError; PragmaError]
            [Some("#push and #pop have been removed"); Some("#push and #pop have been removed")]
        |> ignore

   

[<TestFixture>]
type TestValidation() = 

    let assertValidationFail msgType msgSnippet op tree =
        (validate op tree)
        |> assertFail msgType msgSnippet

    [<Test>]
    member x.TestDetectParseError() =
        let errorText = "test failure"
        let err = createParseError errorText None
        let tree = treeify [err]
        let failure = assertValidationFail ParserError (Some errorText) checkParseError tree
        Assert.AreEqual(err, failure.node)

    [<Test>]
    member x.NoModsAllowed() =
        let source = GslSourceCode("###[2:20]")
        let tree = lexparse source |> returnOrFail
        assertValidationFail
            PartError
            (Some "Can only apply part mods to Gene or PartId, not Marker")
            checkMods tree
        |> ignore

    [<Test>]
    member x.NoModsOnAssemblies() =
        let source = GslSourceCode("(pFOO; gFOO)[2:20]")
        let tree = lexparse source |> returnOrFail
        assertValidationFail
            PartError
            (Some "Can only apply part mods to Gene or PartId, not Assembly")
            checkMods tree
        |> ignore

[<TestFixture>]
type TestTransformation() =


    let variableTest = sourceCompareTest resolveVariables

    let mathReductionTest =
        sourceCompareTest (resolveVariables >=> reduceMathExpressions)

    let functionInliningTest =
        sourceCompareTest
            (resolveVariables
             >=> inlineFunctionCalls
             >=> stripFunctions)

    let flattenAssemblyTest =
        sourceCompareTest (buildPragmas Set.empty >=> flattenAssemblies)

    let flattenPartTest =
        sourceCompareTest (resolveVariables >=> flattenAssemblies)

    let variableResolutionPipeline =
        checkRecursiveCalls
        >=> resolveVariables
        >=> inlineFunctionCalls
        >=> stripFunctions
        >=> resolveVariablesStrict

    let fullVariableResolutionTest = sourceCompareTest variableResolutionPipeline


    [<SetUp>]
    member x.SetUp() = initGlobals()

    [<Test>]
    member x.TestVariableResolutionBasic() = 
        variableTest "let foo = gFOO\n&foo" "let foo = gFOO\ngFOO"

    [<Test>]
    member x.TestVariableResolutionOneLevel() =
        let source = """
let foo = gFOO
let bar(p) =
    &p
    &foo
end"""
        let expectedResolution = """
let foo = gFOO
let bar(p) =
    &p
    gFOO
end"""
        variableTest source expectedResolution

    [<Test>]
    member x.TestBlockScopedVariables() =
        let source = """
let foo(bar) =
    let baz = 1
    &bar
end
&bar
&baz
"""
        GslSourceCode(source)
        |> compile resolveVariables
        |> assertFailMany [UnresolvedVariable; UnresolvedVariable] [Some("bar"); Some("baz")]
        |> ignore

    [<Test>]
    member x.TestIntExprResolution() =
        let source = "let foo = 1\nlet bar = &foo + 2\n"
        let expected = "let foo = 1\nlet bar = (1 + 2)\n"
        variableTest source expected

        // test deep nesting
        let source = """
let foo = 1
let bar = &foo + 2
let baz = &foo + &bar
let qux(a, b) =
    let local0 = &b
    let local1 = &a + &foo
    let local2 = &local1 + &bar + &baz + &local1 + &local0
end
"""
        let expected = """
let foo = 1
let bar = (1 + 2)
let baz = (1 + (1 + 2))
let qux(a, b) =
    let local0 = &b
    let local1 = (&a + 1)
    let local2 = (((((&a + 1) + (1 + 2)) + (1 + (1 + 2))) + (&a + 1)) + &local0)
end
"""
        // note that &local0 cannot resolve to &b at this phase, because the use has a a type via
        // context but eventually resolves to the untyped &b function local variable, so it should
        // not expand at this phase.
        variableTest source expected

    [<Test>]
    member x.TestMathReduction() =
        let source = "let foo = 1 + 1\n"
        let expected = "let foo = 2"
        mathReductionTest source expected

    [<Test>]
    member x.TestAlwaysFailingRegressionTest() =
        let source = """
let x = -12
let y = 1+1
let z = (4*10)+7-5
gHO[&x:&z]
gHO[&y:~&z]"""
        let expected = """
let x = -12
let y = 2
let z = 42
gHO[-12:42]
gHO[2:~42]"""
        mathReductionTest source expected

    [<Test>]
    member x.TestFunctionInlining() =
        let source = """
let foo(bar) =
    let baz = 1 + &bar
    let innerFunc(x) =
        let innerVar = "wow!"
        let innerVar2 = &x
        pFOO ; gBAR[&innerVar2:20] {#name &innerVar}
    end
    innerFunc(&baz)
end
foo(2)
"""
        let expected = """
do
    let bar = 2
    let baz = (1 + &bar)
    do
        let x = (1 + 2)
        let innerVar = "wow!"
        let innerVar2 = &x
        pFOO ; gBAR[&innerVar2:20] {#name wow!}
    end
end
"""
        functionInliningTest source expected

    [<Test>]
    member x.TestFunctionCallNested() =
        let source = """
let foo(bar, baz) =
    &bar ; &baz
end
let fooWithFixedArg(qux) =
    foo(&qux, gFOO)
end
fooWithFixedArg(pFOO)
"""
        let expected = """
do
    let qux = pFOO
    do
        let bar = pFOO
        let baz = gFOO
        pFOO ; gFOO
    end
end
"""
        fullVariableResolutionTest source expected

    [<Test>]
    member x.TestFunctionCallAliasing() =
        let source = """
let f1(x, y, z) =
    &x ; &y ; &z
end
let f2(x, y) =
    f1(gFOO, &x, &y)
end
f2(gBAR, gBAZ)
"""
        let expected = """
do
    let x = gBAR
    let y = gBAZ
    do
        let x = gFOO
        let y = gBAR
        let z = gBAZ
        gFOO ; gBAR ; gBAZ
    end
end
"""
        fullVariableResolutionTest source expected

    [<Test>]
    /// Test that variable aliases (let foo = &bar) resolve correctly.
    member x.TestVariableAliasResolution() =
        let source = """
let foo = gFOO
let bar = 1
let fooAlias = &foo
let barAlias = &bar
let testFunc(a, b) =
    let baz = &a
    pFOO ; &b
end
testFunc(&barAlias, &fooAlias)
"""
        let expected = """
let foo = gFOO
let bar = 1
let fooAlias = gFOO
let barAlias = 1
do
    let a = 1
    let b = gFOO
    let baz = 1
    pFOO ; gFOO
end
"""
        fullVariableResolutionTest source expected

    [<Test>]
    member x.TestRecursiveFunctionCall() =
        let source = """
let foo(x) =
    let bar(y) =
        foo(&y)
    end
    bar(&x)
end
foo(1)"""
        GslSourceCode(source)
        |> compile variableResolutionPipeline
        |> assertFail RecursiveFunctionCall None
        |> ignore

    [<Test>]
    /// Test that we correctly catch type errors.
    member x.TestTypeChecking() =
        let source = """
let fooPart = gFOO
let fooInt = 1
let testFunc(int, part) =
    let baz = 1 + &int
    pFOO ; &part
end
testFunc(&fooPart, &fooInt)
"""
        GslSourceCode(source)
        |> compile variableResolutionPipeline
        |> assertFailMany
            [TypeError; TypeError]
            [Some("The variable int has been inferred to have the type Part");
             Some("The variable part has been inferred to have the type Int")]
        |> ignore

    [<Test>]
    member x.TestFunctionCallArgCount() =
        let source = """
let foo(a, b) =
    &a ; &b
end
foo(gFOO)
foo(gFOO, pFOO, dFOO)
"""
        GslSourceCode(source)
        |> compile variableResolutionPipeline
        |> assertFailMany
            [TypeError; TypeError]
            [Some("Function 'foo' expects 2 arguments but received 1.");
             Some("Function 'foo' expects 2 arguments but received 3.")]
        |> ignore

    [<Test>]
    member x.TestFlattenAssemblies() =
        let source = "gFOO ; (pBAR ; (gBAR ; dBAR) {#name inner}) {#seed 123} ; gBAZ"
        let expected = "gFOO ; pBAR {#seed 123} ; gBAR {#name inner #seed 123} ; dBAR {#name inner #seed 123} ; gBAZ"

        flattenAssemblyTest source expected

    [<Test>]
    member x.TestFlattenReverseAssemblies() =
        let source = "gFOO ; !(pBAR {#fuse} ; !gBAR ; gBAZ) ; gQUX"
        let expected = "gFOO ; !gBAZ ; gBAR {#fuse} ; !pBAR ; gQUX"

        flattenAssemblyTest source expected

    [<Test>]
    member x.TestFlattenReverseWithInvertPragmas() =
        let source = "gFOO ; !(pBAR {#rabitstart} ; !gBAR {#rabitend} ; gBAZ) ; gQUX"
        let expected = "gFOO ; !gBAZ ; gBAR {#rabitstart} ; !pBAR {#rabitend} ; gQUX"

        flattenAssemblyTest source expected

    [<Test>]
    member x.TestFlattenSinglePartVariable() =
        let source = "let foo = @R123\n!&foo"
        let expected = "let foo = @R123\n!@R123"

        flattenPartTest source expected
