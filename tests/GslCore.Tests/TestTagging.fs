namespace gslc.Tests
open System
open NUnit.Framework
open Amyris.ErrorHandling
open AstTypes
open AstAssertions
open AstExpansion
open constants

[<TestFixture>]
type TestTagging() = 

    /// Grab high level part wrappers around assemblies so we can see pragmas
    let rec extractParts (n:AstNode) : ParsePart list =
        [
            match n with
            | Block b -> 
                let result = b.x |> List.collect extractParts
                yield! result
            | Part p -> 
                yield p.x
            | Splice s -> 
                let result = s |> List.ofArray |> List.collect extractParts
                yield! result
            | _ -> ()
        ]

    /// compile one GSL source example and extract assemblies
    let compileOne source =
        source 
        |> GslSourceCode
        |> compile (phase1 Set.empty) 
        |> returnOrFail
        |> fun x -> extractParts x.wrappedNode

    /// example with no #tag
    let noTag = """#refgenome cenpk
#platform stitch

uADH1; dADH1
""" 

    /// one tag on a construct
    let simpleTag = """#refgenome cenpk
#platform stitch

#tag flavor:vanilla
uADH1; dADH1
""" 

    /// two tags on two different constructs
    let twoSerialTags = """#refgenome cenpk
#platform stitch

#tag flavor:vanilla
uADH1; dADH1
#tag temp:hot
uADH2; dADH2
""" 

    /// two tags on same line of tag
    let twoTandemTags = """#refgenome cenpk
#platform stitch

#tag flavor:vanilla temp:hot
uADH1; dADH1
""" 

    /// two tags on same construct on different lines
    let twoParallelTags = """#refgenome cenpk
#platform stitch

#tag flavor:vanilla
#tag temp:hot
uADH1; dADH1
""" 

    let runAndExtractTags code =
            code
            |> compileOne
            |> List.map (fun p ->
                            let tagPragmas =
                                p.pragmas 
                                |> List.choose (fun n ->
                                    match n with
                                    | Pragma(p) when p.x.name = "tag" ->
                                        Some p.x
                                    | _ -> 
                                        None
                                )
                            p,tagPragmas
                        )

    do
        // initialize pragmas
        pragmaTypes.finalizePragmas [TaggingPlugin.tagPragmaDef]

    [<Test>]
    member __.TestNoTag() =

        let results = noTag |> runAndExtractTags

        // should be one assembly
        Assert.AreEqual(1,results.Length)

        let _assembly,pragmas = results.Head
        // should be zero pragmas
        Assert.AreEqual(0,pragmas.Length)

    [<Test>]
    member __.TestSimpleTag() =

        let results = simpleTag |> runAndExtractTags

        // should be one assembly
        Assert.AreEqual(1,results.Length)

        let _assembly,pragmas = results.Head
        // should just be one pragma
        Assert.AreEqual(1,pragmas.Length)
        // should match the entered text
        Assert.IsTrue(pragmas.Head.hasVal "flavor:vanilla")

    [<Test>]
    member __.TwoSerialTags() =

        let results = twoSerialTags |> runAndExtractTags

        // should be two assemblies
        Assert.AreEqual(2,results.Length)

        match results with
        | [_assembly1,pragmas1 ; _assembly2,pragmas2] ->
            // should just be one pragma on each
            Assert.AreEqual(1,pragmas1.Length)
            Assert.AreEqual(1,pragmas2.Length)
            // should match the entered text
            Assert.IsTrue(pragmas1.Head.hasVal "flavor:vanilla")
            Assert.IsTrue(pragmas2.Head.hasVal "temp:hot")
        | x -> failwithf "bad config %A in TwoSerialTags" x

    [<Test>]
    member __.TwoTandemTags() =

        let results = twoTandemTags |> runAndExtractTags

        // should be one assemby
        Assert.AreEqual(1,results.Length)

        match results with
        | [_assembly1,pragmas1 ] ->
            // should be two pragmas on this assembly
            match pragmas1 with
            | [p] ->
                // should match the entered text - with both pragmas together
                printfn "YYY=%A" p
                Assert.AreEqual(["flavor:vanilla";"temp:hot"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in TwoParallelTags" x

    [<Test>]
    member __.TwoParallelTags() =
        // two tags created on different lines

        let results = twoParallelTags |> runAndExtractTags

        // should be one assemby
        Assert.AreEqual(1,results.Length)

        match results with
        | [_assembly1,pragmas1 ] ->
            // should be two pragmas on this assembly
            match pragmas1 with
            | [p] ->
                // should match the two pragmas but combined
                printfn "XXX=%A" p
                Assert.IsTrue(p.hasVal "flavor:vanilla temp:hot")

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in TwoParallelTags" x
