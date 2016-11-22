/// Entry points for lexing and parsing.
module LexAndParse
open constants
open GslParser
open GslLexer
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open utils
open Amyris.ErrorHandling
open AstTypes
open AstErrorHandling
open System.Diagnostics
open System

/// Create an instance of a GSL tokenizer, which internally stores
/// the actual tokenizer backend it is currently using.
let createGslTokenizer verbose =
    let currentTokenizer = ref Main
    let setTokenizer mode = currentTokenizer := mode
    let tokenize verbose (buf: LexBuffer<_>) =
        if verbose then
            // show progress tokenizing
            if buf.EndPos.Line % 1000 = 0 then
                printf "%d ... " buf.EndPos.Line
                Console.Out.Flush()
        let tokenizer =
            match !currentTokenizer with
            | Main -> main setTokenizer
            | PragmaLine -> pragmaLine setTokenizer
            | InlinePragma -> inlinePragmaParts setTokenizer
            | InlineRoughage -> roughage setTokenizer
        let t = tokenizer buf
        if verbose then
            printfn "%A" t
        t
    tokenize verbose

/// Just perform lexing.  For testing/debugging purposes.
let lexTest verbose inputText =
    let lexbuf = LexBuffer<_>.FromString inputText

    let tokenizer = createGslTokenizer verbose

    let rec nextToken() =
        let t = tokenizer lexbuf

        if t = EOF then ()
        else
            printf "%A\n" t
            nextToken()
    nextToken()

let private doLexParse verbose inBuffer =
    if verbose then printfn "Starting tree parse...\n"
    let tokenizer = createGslTokenizer verbose
    let t = GslParser.start tokenizer inBuffer
    if verbose then printfn "Parsed tree!"
    t

// =====================
// handling of parse errors
// =====================

/// Produce a string representation of a parser token ID.
let private tokenIdToString t = 
    match t with
    | TOKEN_LPAREN -> "("
    | TOKEN_RPAREN -> ")"
    | TOKEN_PLUS -> "+"
    | TOKEN_NEWLINE -> "newline"
    | TOKEN_EOF -> "EOF"
    | TOKEN_UMINUS -> "-"
    | TOKEN_DOUBLEQUOTE -> "\""
    | TOKEN_START_ROUGHAGE -> "<@"
    | TOKEN_END_ROUGHAGE -> "@>"
    | TOKEN_GREATERTHAN -> ">"
    | TOKEN_LESSTHAN -> "<"
    | TOKEN_DOT -> "."
    | TOKEN_LET -> "let"
    | TOKEN_CUT -> "cut"
    | TOKEN_END -> "end"
    | TOKEN_OPEN -> "open"
    | TOKEN_FOR -> "for"
    | TOKEN_IN -> "in"
    | TOKEN_DO -> "do"
    | TOKEN_COLON -> ":"
    | TOKEN_STAR -> "*"
    | TOKEN_SLASH -> "/"
    | TOKEN_AT -> "@"
    | TOKEN_LBRACE -> "{"
    | TOKEN_RBRACE -> "}"
    | TOKEN_EXCLM -> "!"
    | TOKEN_EQUALS -> "="
    | TOKEN_CARAT -> "^"
    | TOKEN_COMMA -> ","
    | TOKEN_HYPHEN -> "-"
    | TOKEN_OPENSQBRACKET -> "["
    | TOKEN_DOLLAR-> "$"
    | TOKEN_CLOSESQBRACKET-> "]"
    | TOKEN_SEMICOLON-> ";"
    | TOKEN_MARKER-> "###"
    | TOKEN_TILDE-> "~"
    | TOKEN_VARIABLE-> "variable"
    | TOKEN_PNAME-> "pragma name"
    | TOKEN_PVALUE-> "pragma value"
    | TOKEN_DNAMUTATION-> "dna mutation"
    | TOKEN_AAMUTATION-> "amino acid mutation"
    | TOKEN_LINKER-> "linker"
    | TOKEN_QUOTED_STRING-> "quoted string"
    | TOKEN_DOCSTRING-> "docstring"
    | TOKEN_STRING-> "string"
    | TOKEN_INT-> "int"
    | TOKEN_ID-> "identifier"
    | TOKEN_end_of_input -> "EOF"
    | TOKEN_error -> "error token"

let private tokenTagToString idx =
    idx
    |> tokenTagToTokenId
    |> tokenIdToString

let private ignoredTokens = [TOKEN_error; TOKEN_end_of_input] |> Set.ofList
let private acceptedToken t = not (ignoredTokens.Contains(t))


/// Given a parse state context, produce a human-readable list of acceptable completions.
let private formatTokens context =
    Seq.append context.reduceTokens context.shiftableTokens
    |> Set.ofSeq
    |> Seq.map tokenTagToTokenId
    |> Seq.filter acceptedToken
    |> Seq.map tokenIdToString
    |> List.ofSeq
    |> List.sort
    |> List.map (sprintf "'%s'")
    |> String.concat ", "

let private createErrorNode (inBuffer: LexBuffer<_>) =
    // the end position of the buffer is pretty much always one column further than the end of
    // the token of interest
    let shiftedPos =
        if inBuffer.EndPos.Column > 0 then inBuffer.EndPos.ShiftColumnBy(-1)
        else inBuffer.EndPos
    let pos = {s = shiftedPos; e = shiftedPos}
    ParseError({x=""; pos=Some(pos)})

/// If exn is a GslParseError, format it.
/// Otherwise, return None.
let private parseExceptionToError (errNode: AstNode) exn =
    match exn with
    | GslParseError(context) -> 
        let tokenName: string option =
            context.currentToken
            |> Option.map (unbox >> tagOfToken >> tokenTagToTokenId >> tokenIdToString)
        let msg = context.message

        let fullMsg =
            match tokenName with
            | Some(t) -> sprintf "%s; found '%s', expected one of [%s]." msg t (formatTokens context)
            | None -> msg

        Some(
            {msg = fullMsg;
             sourcePosition = errNode.pos
             node = errNode;
             msgType = ParserError;
             stackTrace = Some(StackTrace(exn))})
    | _ -> None

let lexAndParse verbose (source: GslSourceCode) =
    let inBuffer = LexBuffer<_>.FromString source.String

    let convertException exn =
        let errNode = createErrorNode inBuffer
        match parseExceptionToError errNode exn with
        | Some(msg) -> msg
        | None -> exceptionToError ParserError errNode exn

    let doLexParseCaptureException =
        doLexParse verbose
        |> captureException convertException

    doLexParseCaptureException inBuffer
