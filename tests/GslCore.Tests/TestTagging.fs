namespace gslc.Tests

open System.Collections
open GslCore.Tests
open LegacyParseTypes
open NUnit.Framework
open Amyris.ErrorHandling
open AstTypes
open AstAssertions
open AstExpansion
open PluginTypes
open commonTypes
open constants
open BasicCodonProvider

open pragmaTypes
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
        finalizePragmas [ TaggingPlugin.tagPragmaDef ; TaggingPlugin.gTagPragmaDef ]

    [<Test>]
    member __.TestNoTag() =

        let results = noTag |> runAndExtractTags

        Assert.AreEqual(1,results.Length)

        let _assembly,pragmas = results.Head

        Assert.AreEqual(0,pragmas.Length)

    [<Test>]
    member __.TestSimpleTag() =

        let results = simpleTag |> runAndExtractTags

        Assert.AreEqual(1,results.Length)

        let _assembly,pragmas = results.Head
        
        Assert.AreEqual(1,pragmas.Length)

        Assert.IsTrue(pragmas.Head.hasVal "flavor:vanilla")

    [<Test>]
    member __.TwoSerialTags() =

        let results = twoSerialTags |> runAndExtractTags

        Assert.AreEqual(2,results.Length)

        match results with
        | [_assembly1,pragmas1 ; _assembly2,pragmas2] ->

            Assert.AreEqual(1,pragmas1.Length)
            Assert.AreEqual(1,pragmas2.Length)

            Assert.IsTrue(pragmas1.Head.hasVal "flavor:vanilla")
            Assert.IsTrue(pragmas2.Head.hasVal "temp:hot")
        | x -> failwithf "bad config %A in TwoSerialTags" x

    [<Test>]
    member __.TwoTandemTags() =

        let results = twoTandemTags |> runAndExtractTags

        Assert.AreEqual(1,results.Length)

        match results with
        | [_assembly1,pragmas1 ] ->
            match pragmas1 with
            | [p] ->
                Assert.AreEqual(["flavor:vanilla";"temp:hot"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in TwoParallelTags" x

    [<Test>]
    member __.TwoParallelTags() =
        // two tags created on different lines

        let results = twoParallelTags |> runAndExtractTags

        Assert.AreEqual(1,results.Length)

        match results with
        | [_assembly1,pragmas1 ] ->
            match pragmas1 with
            | [p] ->
                Assert.AreEqual(["flavor:vanilla";"temp:hot"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in TwoParallelTags" x

    [<Test>]
    member __.MixedTags() =
        let results = mixedTags |> runAndExtractTags

        Assert.AreEqual(2,results.Length)

        match results with
        | [_assembly1,pragmas1 ; _assembly2,pragmas2] ->
            match pragmas1 with
            | [p] ->
                Assert.AreEqual(["flavor:vanilla";"temp:hot" ; "condiment:ketchup"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

            match pragmas2 with
            | [p] ->
                Assert.AreEqual(["id:1234"],p.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in TwoParallelTags" x

    [<Test>]
    member __.OneGlobalTag() =
        let results = oneGlobalTag |> runAndExtractTags

        Assert.AreEqual(1, results.Length)

        let (_assembly, pragmas) = results.Head

        Assert.AreEqual(1, pragmas.Length)

        Assert.IsTrue(pragmas.Head.hasVal "flavor:vanilla")

    [<Test>]
    member __.OneGlobalOneScopedTag() =
        let results = twoTagsOneGlobalOneScoped |> runAndExtractTags

        Assert.AreEqual(1, results.Length)

        match results with
        | [ _assembly1, pragmas ] ->
            match pragmas with
            | [p1 ; p2] ->
                Assert.AreEqual([ "flavor:vanilla" ], p1.args)
                Assert.AreEqual([ "temp:hot" ], p2.args)

            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in OneGlobalOneScopedTag" x

    [<Test>]
    member __.OneGlobalTwoScopedTag() =
        let results = threeTagsOneGlobalTwoScoped |> runAndExtractTags

        Assert.AreEqual(2, results.Length)

        match results with
        | [_assembly1, pragmas1 ; _assembly2, pragmas2] ->
            match pragmas1 with
            | [p1 ; p2] ->
                Assert.AreEqual([ "flavor:vanilla" ], p1.args)
                Assert.AreEqual([ "temp:hot" ], p2.args)
            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

            match pragmas2 with
            | [p1 ; p2] ->
                Assert.AreEqual([ "flavor:vanilla" ], p1.args)
                Assert.AreEqual([ "condiment:ketchup" ], p2.args)
            | x -> failwithf "unexpected number of pragmas %d:\n%A" x.Length x

        | x -> failwithf "bad assembly pattern %A in OneGlobalTwoScopedTag" x
        
    static member dummyContext =
        { ATContext.ga =
            { GlobalAssets.rgs = Map.empty
              seqLibrary = Map.empty
              codonProvider =
                { BasicCodonProvider.parameters = None
                  cache = None } }
          opts = commandConfig.defaultOpts }
        
    static member dummyDnaAssembly =
        { DnaAssembly.id = None
          dnaParts = []
          name = "fooslice"
          uri = None
          linkerHint = "hint"
          pragmas = createPragmaCollection Seq.empty
          designParams = DesignParams.initialDesignParams
          docStrings = []
          materializedFrom = AssemblyTestBase.emptyAssembly
          tags = Set.empty
          topology = Topology.Linear }               

    static member FoldTestCases with get(): IEnumerable =
        seq {
            TestCaseData(
                {| CliTags = [ { AssemblyTag.nameSpace = "bar"; tag = "foo" } ]
                   PragmaTags = ([| |] : string [])
                   GlobalPragmaTags = ([| |]: string[])
                   AssemblyTags = ([]: AssemblyTag list) |})
                .Returns([ "bar:foo" ])
            TestCaseData(
                {| CliTags = ([]: AssemblyTag list)
                   PragmaTags = ([| |] : string [])
                   GlobalPragmaTags = ([| |]: string[])
                   AssemblyTags = [ { AssemblyTag.nameSpace = "bar"; tag = "foo" } ] |})
                .Returns([ "bar:foo" ])
            TestCaseData(
                {| CliTags = ([]: AssemblyTag list)
                   PragmaTags = ([| "bar:foo" |] : string [])
                   GlobalPragmaTags = ([| |]: string[])
                   AssemblyTags = ([]: AssemblyTag list) |})
                .Returns([ "bar:foo" ])
            TestCaseData(
                {| CliTags = [ { AssemblyTag.nameSpace = "bar"; tag = "foo" } ]
                   PragmaTags = ([| |] : string [])
                   GlobalPragmaTags = ([| "bar:foo" |]: string[])
                   AssemblyTags = ([]: AssemblyTag list) |})
                .Returns([ "bar:foo" ])
            TestCaseData(
                {| CliTags = [ { AssemblyTag.nameSpace = "bar"; tag = "foo" } ]
                   PragmaTags = [| "car:foo"  |]
                   GlobalPragmaTags = [| "dar:foo" |]
                   AssemblyTags = [ { AssemblyTag.nameSpace = "ear"; tag = "foo" } ] |})
                .Returns([ "bar:foo"; "car:foo"; "dar:foo"; "ear:foo" ])                    
                 
        } :> IEnumerable    
    
    [<TestCaseSource(typeof<TestTagging>, "FoldTestCases")>]
    member __.TestFolding(input: {| CliTags: AssemblyTag list; PragmaTags: string[]; GlobalPragmaTags: string[]; AssemblyTags: AssemblyTag list |}) =
        let cliTags = input.CliTags
        let pragmaCollection =
            createPragmaCollection
                [ for pragma in input.PragmaTags do
                      { Pragma.args = [ pragma ]
                        definition = TaggingPlugin.tagPragmaDef } 

                  for pragma in input.GlobalPragmaTags do
                      { Pragma.args = [ pragma ]
                        definition = TaggingPlugin.gTagPragmaDef } ]
                
        let context = TestTagging.dummyContext
        let assembly =
            { TestTagging.dummyDnaAssembly with
                pragmas = pragmaCollection
                tags = input.AssemblyTags |> Set.ofList }
        match TaggingPlugin.foldInTags cliTags context assembly with
        | Ok (result, _) ->
            result.tags |> Set.toList |> List.map (fun tag -> sprintf "%s:%s" tag.nameSpace tag.tag) |> List.sort
        | Bad _err ->
            failwith "Unexpected error"

    
