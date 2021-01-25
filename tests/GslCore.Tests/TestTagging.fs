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

    /// Three tags on same construct on different lines then single on next construct
    let mixedTags = """#refgenome cenpk
#platform stitch

#tag flavor:vanilla
#tag temp:hot
#tag condiment:ketchup
uADH1; dADH1

#tag id:1234
uADH2; dADH2
""" 

    /// one global tag
    let oneGlobalTag = """#refgenome cenpk
#platform stitch
#gtag flavor:vanilla

uADH1; dADH1
""" 

    /// one global tag, one scoped tag
    let twoTagsOneGlobalOneScoped = """#refgenome cenpk
#platform stitch
#gtag flavor:vanilla

#tag temp:hot
uADH1; dADH1
""" 

    /// one global tag, two scoped tag
    let threeTagsOneGlobalTwoScoped = """#refgenome cenpk
#platform stitch
#gtag flavor:vanilla

#tag temp:hot
uADH1; dADH1

#tag condiment:ketchup
uADH2; dADH2
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
                                    | Pragma(p) when p.x.name = "gtag" ->
                                        Some p.x
                                    | _ -> 
                                        None
                                )
                            p,tagPragmas
                        )

    do
        // initialize pragmas
        pragmaTypes.finalizePragmas [ TaggingPlugin.tagPragmaDef ; TaggingPlugin.gTagPragmaDef ]

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
            // should be one pragma on this assembly with two parts
            match pragmas1 with
            | [p] ->
                // should match the entered text - with both pragmas together
                Assert.AreEqual(["flavor:vanilla";"temp:hot"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in TwoParallelTags" x

    [<Test>]
    member __.MixedTags() =
        let results = mixedTags |> runAndExtractTags

        // should be two assemblies
        Assert.AreEqual(2,results.Length)

        match results with
        | [_assembly1,pragmas1 ; _assembly2,pragmas2] ->
            // should be one pragma on assembly1 with 3 parts
            match pragmas1 with
            | [p] ->
                // should match the entered text - with both pragmas together
                Assert.AreEqual(["flavor:vanilla";"temp:hot" ; "condiment:ketchup"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

            // should be one pragma on assembly2 with 1 part
            match pragmas2 with
            | [p] ->
                // should match the entered text - with both pragmas together
                Assert.AreEqual(["id:1234"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in TwoParallelTags" x

    [<Test>]
    member __.OneGlobalTag() =
        let results = oneGlobalTag |> runAndExtractTags

        // should be one assembly
        Assert.AreEqual(1, results.Length)

        let (_assembly, pragmas) = results.Head

        // should just be one pragma
        Assert.AreEqual(1, pragmas.Length)
        // should match the entered text
        Assert.IsTrue(pragmas.Head.hasVal "flavor:vanilla")

    [<Test>]
    member __.OneGlobalOneScopedTag() =
        let results = twoTagsOneGlobalOneScoped |> runAndExtractTags

        // should be one assembly
        Assert.AreEqual(1, results.Length)

        match results with
        | [ _assembly1, pragmas ] ->
            // should be two pragmas on this assembly
            match pragmas with
            | [p1 ; p2] ->
                // should have global tag and scoped tag
                Assert.AreEqual([ "flavor:vanilla" ], p1.args)
                Assert.AreEqual([ "temp:hot" ], p2.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in OneGlobalOneScopedTag" x

    [<Test>]
    member __.OneGlobalTwoScopedTag() =
        let results = threeTagsOneGlobalTwoScoped |> runAndExtractTags

        // should be two assemblies
        Assert.AreEqual(2, results.Length)

        match results with
        | [_assembly1, pragmas1 ; _assembly2, pragmas2] ->
            // should be two pragmas on assembly 1
            match pragmas1 with
            | [p1 ; p2] ->
                // should have global tag and scoped tag
                Assert.AreEqual([ "flavor:vanilla" ], p1.args)
                Assert.AreEqual([ "temp:hot" ], p2.args)
            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

            // should be two pragmas on assembly 2
            match pragmas2 with
            | [p1 ; p2] ->
                // should have global tag and scoped tag
                Assert.AreEqual([ "flavor:vanilla" ], p1.args)
                Assert.AreEqual([ "condiment:ketchup" ], p2.args)
            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in OneGlobalTwoScopedTag" x