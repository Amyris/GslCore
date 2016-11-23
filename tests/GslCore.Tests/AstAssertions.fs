/// Test helper functions and assertions.
module AstAssertions
open System
open NUnit.Framework
open Amyris.ErrorHandling
open AstExpansion
open AstTypes
open AstErrorHandling
open AstProcess
open AstAlgorithms
open AstFixtures
open LexAndParse
open constants

/// Initialize any globals that need to be set for tests.
let initGlobals() = pragmaTypes.finalizePragmas []

/// Lex and parse, in verbose mode.
let lexparse = lexAndParse true


/// Assert that two trees are equal.  If they aren't, pretty print them.
/// Note that we never use source positions when comparing two AST nodes.
let assertTreesEqual (expected: AstTreeHead) (actual: AstTreeHead) =
    if expected <> actual then
        let dumpAst node =
            let items = traverse expected |> Seq.map (fun n -> sprintf "%+A" n) |> List.ofSeq
            String.Join(", ", items)

        let msg =
            sprintf
                "ASTs were not equal.\nExpected:\n%s\n\nActual:\n%s\nExpected nodes:\n%+A\nActual nodes:\n%+A"
                (decompile expected.wrappedNode)
                (decompile actual.wrappedNode)
                (expected)
                (actual)

        Assert.Fail(msg)

/// Assert that passed tree decompiles to the provided source literal.
/// Optionally trim leading and trailing whitespace.
let assertDecompilesTo (source: string) (tree: AstTreeHead) =
    let treeAsText = decompile tree.wrappedNode

    let cleanString (s: string) = s.Trim().Replace("\r\n", "\n")
    let expected, actual = cleanString source, cleanString treeAsText

    // search for where the two strings differ, if they do
    let diffPos =
        Seq.zip (Seq.cast<char> expected) (Seq.cast<char> actual)
        |> Seq.indexed
        |> Seq.tryPick (fun (i, (ec, ac)) -> if ec <> ac then Some(i) else None)
    match diffPos with
    | Some(p) ->
        let printSplitAtFirstDiff (s: string) =
            sprintf "%s(###diff site###)%s"
                s.[..p-1] s.[p..]
        let msg =
            (sprintf "Expected and actual source differs at character %d.\nExpected:\n%s\n\nActual:\n%s"
                p
                expected
                (printSplitAtFirstDiff actual))
        Assert.Fail(msg)
    | None ->
        // if they were the same when zipped, make sure they were the same length!
        if expected.Length <> actual.Length then
            Assert.Fail(
                sprintf
                    "Expected string with %d characters, got a string with %d instead.\nExpected:\n%s\n\nActual:\n%s"
                    expected.Length
                    actual.Length
                    expected
                    actual)

/// Assert that source compiles to an AST with the same top level block contents as the list of items passed.
/// Also assert that the tree decompiles correctly to the same literal source which was passed in.
let assertRoundtrip source astItems =
    let tree = lexparse (GslSourceCode(source)) |> returnOrFail
    assertTreesEqual (treeify astItems) tree
    assertDecompilesTo source tree

/// Parse and run op on parsed tree.
let compile op source =
    lexparse source
    >>= op

/// Fail if the incoming result is Bad.
/// Optionally pass in source code for message printing.
let failIfBad sourceCode r =
    let printMsg (m: AstMessage) =
        match sourceCode with
        | Some(s) -> m.Longform(false, s)
        | None -> m.Summary
    match r with
    | Bad(errs) -> 
        errs
        |> Seq.map printMsg
        |> String.concat "\n"
        |> Assert.Fail
    | _ -> ()
    r

///<summary>
/// Compare expected and reprinted source for a provided source sample.
/// The source is parsed, the tree is operated on by op, and the
/// resulting source is compared to the expected source.
///</summary>
let sourceCompareTest op sourceIn expectedSource =
    sourceIn
    |> GslSourceCode
    |> compile op 
    |> failIfBad (Some(sourceIn))
    |> returnOrFail
    |> assertDecompilesTo expectedSource

///<summary>
///Given GSL source code, parse it, reprint the AST, and compare
///to the expectation.
///</summary>
let testExpectedReprinting sourceIn expectedOut =
    sourceCompareTest (promote id) sourceIn expectedOut


let checkMessages
        (expectedTypes: AstMessageType list)
        (textSnippets: string option list)
        (msgs: AstMessage list) =

    Assert.AreEqual(
        expectedTypes.Length,
        textSnippets.Length,
        "Please provide the same number of expected types and text snippets.")

    if expectedTypes.Length <> msgs.Length then
        Assert.Fail(sprintf "Wrong number of errors.  Expected %d, got %d: %+A" expectedTypes.Length msgs.Length msgs)

    // optionally check text snippet inclusion
    let textSnips = textSnippets |> List.map (fun (s: string option) -> defaultArg s "")
    Seq.zip3 msgs expectedTypes textSnips
    |> Seq.iter (fun (msg, ft, ts) ->
        Assert.AreEqual(ft, msg.msgType)
        Assert.That(msg.msg.Contains(ts), sprintf "Didn't find '%s' in '%s'" ts msg.msg))
    msgs

/// Assert that a result is a success with many warnings, with matching types and message texts.
/// Returns the value and messages for further processing. Otherwise, fail.
let assertWarnMany failTypes textSnippets r =
    match r with
    | Ok(v, msgs) ->
        checkMessages failTypes textSnippets msgs |> ignore
        (v, msgs)
    | x ->
        Assert.Fail(sprintf "Validation test didn't succeed.  Result was %A" x)
        failwith "" // need this line to allow compiler to return msg value above

let assertWarn failType textSnippet r =
    let (v, msgs) = assertWarnMany [failType] [textSnippet] r
    (v, msgs.[0])

/// Assert that a result is a failure with many messages, with matching types and message texts.
/// Returns the failure messages for further processing. Otherwise, fail.
let assertFailMany failTypes textSnippets r =
    match r with
    | Bad msgs -> checkMessages failTypes textSnippets msgs
    | x ->
        Assert.Fail(sprintf "Validation test didn't fail.  Result was %A" x)
        failwith "" // need this line to allow compiler to return msg value above

/// Asset that a result is a failure with a single message, with matching type and message text.
/// Return the failure message for further processing. Otherwise, fail.
let assertFail failType textSnippet r =
    let msgs = assertFailMany [failType] [textSnippet] r
    msgs.[0]

