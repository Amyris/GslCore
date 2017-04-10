module AstErrorHandling
open AstTypes
open Amyris.ErrorHandling
open Microsoft.FSharp.Text.Parsing
open System.Diagnostics
open utils
open constants

// ================
// error handling support
// ================
   
/// Enumeration of every possible kind of error.  Most of these will just be flags, some might hold
/// extra data for convenience and to allow the outer message type to remain the standard.
type AstMessageType =
    | Context // a message which should be interpreted as additional context for a previous message
    | Warning
    | DeprecationWarning // we deduplicate these to avoid inundating the user
    | Error // totally generic error, ideally be more specific
    | ParserError // catchall for ParseError ast nodes for the time being.
    | PartError // catchall for errors related to part validation
    | UnresolvedVariable
    | UnresolvedFunction
    | RecursiveFunctionCall
    | TypeError
    | PragmaError // errors related to pragma construction or manipulation
    | InternalError of AstMessageType // errors that imply WE screwed up somewhere, not the user
    | RefGenomeError
    | ValueError // errors related to values being out of range and such
    // errors encountered when bootstrapping AST elements from source inside the compiler
    // the extra wrapped ast node is intended to be the parsed tree that failed to bootstrap correctly
    | BootstrapError of AstNode option
    // errors related to specific expansion phases that still raise exceptions
    | L2ExpansionError
    | MutationError
    | ProteinError
    | HetBlockError

// TODO: should improve message printing for bootstrapping by dumping source representation of the tree

/// Describe a warning or error encountered during Ast manipulations.
type AstMessage =
   {msg: string;
    sourcePosition: SourcePosition option;
    node: AstNode;
    msgType: AstMessageType;
    stackTrace: StackTrace option}
    with
    /// Pretty-print an AST message including context in source code.
    member msg.Longform(showStackTrace, sourceCode: GslSourceCode) =

        let msgTypeName = GetUnionCaseName msg.msgType
        // get the best position we can
        let pos =
            match msg.sourcePosition, msg.node.pos with
            | Some(p), _ | None, Some(p) -> Some(p)
            | _ -> None

        match pos with
        | None -> // can't do much without a position now, can we.
            sprintf "%s: %s" msgTypeName msg.msg
        | Some(p) -> // now we're cooking with gas
            // Accumulate lines in an error report.
            seq {
                yield (sprintf "%s: %s\n%s"
                    msgTypeName
                    (p.Format())
                    msg.msg)
                yield "================================================================="

                yield! p.SourceContext(sourceCode)
                if showStackTrace then yield msg.stackTrace.ToString()
            } |> String.concat "\n"

    /// Pretty-print a short summart of an AST message.
    member msg.Summary =
        let msgTypeName = GetUnionCaseName msg.msgType
        // get the best position we can
        match msg.sourcePosition, msg.node.pos with
        | Some(p), _ | None, Some(p) ->
            sprintf "%s: %s\n%s"
                msgTypeName
                (p.Format())
                msg.msg
        | _ -> // can't do much without a position now, can we.
            sprintf "%s: %s" msgTypeName msg.msg

    override msg.ToString() = msg.Summary

// =======================
// helper functions for creating warnings and errors
// =======================

/// Delegate position to a passed node.
let createMessage stackTrace msgType msg (node: AstNode) =
    {msg = msg;
     sourcePosition = node.pos;
     node = node;
     msgType = msgType;
     stackTrace = stackTrace}

/// Create a message with no stack trace, of Warning type.
let warningMessage = createMessage None Warning

/// Create a message that collects a stack trace, with unspecified type.
let errorMessage = createMessage (Some(StackTrace()))

// ------ creating error results ------

///Create a error result from a string and a node.
let error msgType msg node = Bad([errorMessage msgType msg node])

///Create a error result from a format string, single value, and node.
let errorf msgType msgfmt fmtVal node = error msgType (sprintf msgfmt fmtVal) node


let private optionalContextStr s =
    match s with
    | Some(s) -> sprintf " in %s" s
    | None -> ""

///Create an error representing a type mismatch resulting from a bugged GSL program.
let variableTypeMismatch varName declaredType expectedType (node: AstNode) =
    error
        TypeError
        (sprintf
            "The variable %s has been inferred to have the type %O, but is required to have the type %O in this context."
            varName declaredType expectedType)
        node

///<summary>
///Create an internal error representing a type mismatch.
///This is a common pattern when unpacking AST entities, and implies
///a bug in compiler logic rather than an error in parsed source code.
///</summary>
let internalTypeMismatch contextStr expectedType (actualNode: AstNode) =
    error
        (InternalError(TypeError))
        (sprintf
            "Expected a '%s'%s, but got a '%s'"
            expectedType
            (optionalContextStr contextStr)
            (actualNode.TypeName))
        actualNode

///Create an internal error if we encounter a pragma that hasn't been built.
let unbuiltPragmaError contextStr name node =
    error
        (InternalError(PragmaError))
        (sprintf
            "Found an unbuilt pragma%s: '%s'"
            (optionalContextStr contextStr)
            name)
        node

/// Convert an exception into an error message.
/// Provide an AST node for context.
let exceptionToError msgType (astNodeContext: AstNode) (exc: System.Exception) =
    let msg = exc.Message
    {msg = msg;
     sourcePosition = astNodeContext.pos;
     node = astNodeContext;
     msgType = msgType;
     stackTrace = Some(StackTrace(exc))}
    
type GslParseErrorContext =
   {stateStack:int list;
    parseState: IParseState; 
    reduceTokens: int list;
    currentToken: obj option; 
    reducibleProductions: int list list; 
    shiftableTokens: int list;
    message : string}

exception GslParseError of GslParseErrorContext

/// Customized handler for errors that occur during parsing.
/// Mostly here to eliminate the polymorphism on token type to
/// allow us to pass the parse error context up stack.
let handleParseError (context: ParseErrorContext<'tok>) =
    let newContext = 
       {stateStack = context.StateStack;
        parseState = context.ParseState;
        reduceTokens = context.ReduceTokens;
        currentToken = context.CurrentToken |> Option.map box;
        reducibleProductions = context.ReducibleProductions;
        shiftableTokens = context.ShiftTokens;
        message = context.Message}

    raise (GslParseError(newContext))

///<summary>
/// Perform some selective deduplication of warnings.
/// For now we just deduplicate DeprecationWarnings to only present them once.
///</summary>
let deduplicateMessages msgs =
    let depWarnings, others =
        msgs
        |> List.partition (fun msg -> match msg.msgType with | DeprecationWarning -> true | _ -> false)

    let dedupedDepWarnings =
        depWarnings
        |> List.distinctBy (fun dw -> dw.msg)
        |> List.map (fun dw -> {dw with msg = sprintf "%s\nThis message will appear only once per file." dw.msg})
    dedupedDepWarnings@others
