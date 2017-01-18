namespace testGslc
open System
open System.IO
open NUnit.Framework
open Amyris.ErrorHandling
open LegacyParseTypes
open pragmaTypes
open LexAndParse
open commandConfig
open Amyris.Bio.primercore
open DesignParams
open AstTypes
open AstFixtures
open AstProcess
open AstAlgorithms
open AstAssertions
open AstErrorHandling
open constants

[<TestFixture>]
type TestPragmas() =

    [<SetUp>]
    member x.SetUp() =
        finalizePragmas []

    [<Test>]
    member x.TestBadPragmasLocal() =
        let badName = "constantinople"
        let badOption = "timbuktu"

        let goodName = "warn"
        let goodOption = "test"
        Assert.Throws (fun () -> returnOrFail (buildPragma badName [badOption]) |> ignore) |> ignore
        Assert.DoesNotThrow (fun () -> buildPragma goodName [goodOption] |> ignore)


    [<Test>]
    /// Smoke test parsing PCR parameters.
    member x.TestPcrParamsParsing() =
        let doParse ifGood ifBad args =
            match revisePP defaultParams args with
            | Ok(r, _) -> ifGood r
            | Bad(msgs) -> ifBad msgs

        let shouldPass = doParse ignore (fun errs -> Assert.Fail(String.concat ", " errs))
        let shouldFail = doParse (fun _ -> Assert.Fail("Parsing didn't fail.")) ignore

        shouldPass ["mon=50mM"; "primer=5.mM"]
        shouldFail ["incrediblybadargument"]
        shouldFail ["mon=2.0uM"; "anotherbadargument"]

[<TestFixture>]
type TestPragmasAST() =

    let pragmaBuildPipeline =
        resolveVariables
        >=> inlineFunctionCalls
        >=> stripFunctions
        >=> resolveVariablesStrict
        >=> stripVariables
        >=> reduceMathExpressions
        >=> (buildPragmas (["capa1"; "capa2"] |> Set.ofList))

    let compilePragmas = compile pragmaBuildPipeline

    let checkPragmaIsBuilt node =
        match node with
        | Pragma(p) -> good
        | ParsePragma(p) -> errorf Error "Pragma '%s' was not built." p.x.name node
        | _ -> good

    let pragmaBuildTest source =
        let source = GslSourceCode source
        source
        |> compilePragmas
        >>= validate checkPragmaIsBuilt
        |> failIfBad (Some(source))
        |> ignore

    let stuffPragmasPipeline =
        pragmaBuildPipeline
        >=> flattenAssemblies
        >=> stuffPragmasIntoAssemblies

    [<SetUp>]
    member x.SetUp() = initGlobals()
       
    [<Test>]
    member x.TestBasicPragmaBuild() =
        let source = """
#name test
let myName = "foo"
#name &myName
let bar(baz) =
    #name &myName
end
"""
        pragmaBuildTest source

    [<Test>]
    member x.TestFuncArgsInPragmas() =
        let source = """
let myName = "foo"
let myNumber = 123
let bar(baz) =
    #name &myName
    #name &baz
    #primermax &myNumber
end
bar("qux")
"""
        pragmaBuildTest source

    [<Test>]
    member x.TestUnknownPragma() =
        let source = "#verybadpragma"

        source
        |> GslSourceCode
        |> compile pragmaBuildPipeline
        |> assertFail PragmaError (Some("Unknown or invalid pragma: '#verybadpragma'"))
        |> ignore

    [<Test>]
    member x.TestBadPragmaScopes() =
        let source = "#fuse\ngFOO {#capa capa1} ; gBAR"

        source
        |> GslSourceCode
        |> compile pragmaBuildPipeline
        |> assertFailMany
            [PragmaError; PragmaError]
            [Some("#fuse is used at block-level"); Some("#capa is used at part-level")]
        |> ignore


    [<Test>]
    member x.TestDeprecatedPragma() =
        let source = "#stitch\n#stitch"

        source
        |> GslSourceCode
        |> compile pragmaBuildPipeline
        |> assertWarnMany
            [DeprecationWarning; DeprecationWarning]
            [Some("#stitch is deprecated"); Some("#stitch is deprecated")]
        // test the deprecation warning deduplication mechanism
        |> snd
        |> deduplicateMessages
        |> (fun msgs -> Ok((), msgs))
        |> assertWarn DeprecationWarning (Some("appear only once per file"))
        |> ignore


    [<Test>]
    member x.TestStuffPragmas() =
        // first assembly should have name, and no others
        // assembly inside the do block should have different seed
        let source = """
#name foobar
#seed 123
gFOO
#name shouldOnlyBeOnInner
do
    #seed 456
    gBAR
end
gBAZ"""
        
        // outer assemblies shouldn't reprint their pragma context as that isn't idiomatic GSL
        sourceCompareTest stuffPragmasPipeline source source

        let reducedAst = returnOrFail (compile stuffPragmasPipeline (source |> GslSourceCode))

        // we need to dive into the AST to check this
        let namePrag = Pragma(nodeWrap {definition=pragmaTypes.namePragmaDef; args=["foobar"]})
        let namePrag2 = Pragma(nodeWrap {definition=pragmaTypes.namePragmaDef; args=["shouldOnlyBeOnInner"]})
        let seedPrag = Pragma(nodeWrap {definition=pragmaTypes.getLegalPragmas().TryFind("seed").Value; args=["123"]})
        let seedPrag2 = Pragma(nodeWrap {definition=pragmaTypes.getLegalPragmas().TryFind("seed").Value; args=["456"]})
        let barGenePart = basePartWrap (createGenePart "gBAR")
        let bazGenePart = basePartWrap (createGenePart "gBAZ")
        let fooAssembly = assemble [fooGenePart]
        let barAssembly = assemble [barGenePart]
        let bazAssembly = assemble [bazGenePart]
        let fooAssemblyWithPragmas = addPragsToPart [namePrag; seedPrag] fooAssembly
        let barAssemblyWithPragmas = addPragsToPart [namePrag2; seedPrag2] barAssembly
        let bazAssemblyWithPragmas = addPragsToPart [seedPrag] bazAssembly
       
        let innerBlock = blockify [seedPrag2; barAssemblyWithPragmas]
        assertTreesEqual
            (treeify [namePrag; seedPrag; fooAssemblyWithPragmas; namePrag2; innerBlock; bazAssemblyWithPragmas]) reducedAst

    [<Test>]
    member x.TestNoCollisions() =
        // A pragma collision is almost impossible to create in source code, which is a good sign I guess.
        // Create an artificial AST to make sure the error is triggered.
        let source = "#name foobar\ngFOO; gBAR"
        let tree = compile stuffPragmasPipeline (GslSourceCode(source)) |> returnOrFail
        // now replace the outer name pragma and make sure the second pass triggers the collision error
        match tree.wrappedNode with
        | Block({x=[Pragma(npw); assem]; pos=p}) -> 
            let newNamePrag = Pragma({npw with x = {npw.x with args = ["differentName"]}})
            Block({x=[newNamePrag; assem]; pos=p})
        | _ -> failwith "Didn't unwrap correctly."
        |> AstTreeHead |> stuffPragmasPipeline
        |> assertFail
                PragmaError
                (Some("The pragma #name is set in this assembly as well as in the enclosing environment with conflicting values."))
        |> ignore
