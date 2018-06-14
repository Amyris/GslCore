/// Types and functions for AST creation.
module AstTypes
open Microsoft.FSharp.Text.Lexing
open constants
open System
open utils

// =======================
// types and functions for lexing
// =======================

/// Convert matched characters during lexing into a string.
let lexeme = LexBuffer<_>.LexemeString

type SourcePosition =
    {s: Position; e: Position}
    with
    override x.ToString() =
        sprintf "@%d,%d-%d,%d"
            (x.s.Line+1) (x.s.Column+1) (x.e.Line+1) (x.e.Column+1)
    /// Return a nicely-formatted message for the start of this source position.
    member x.Format() =
        sprintf "near line %d col %d" (x.s.Line+1) (x.s.Column+1)
    /// Provide a code snippet with an indication of the start of this position.
    /// Returned as a sequence of strings, one sequence item for each line.
    /// Optionally override the default number of lines to use for context, defaults to 5.
    member x.SourceContext((GslSourceCode(source)): GslSourceCode, ?contextLines) = seq {
        let contextLines = defaultArg contextLines 5
        let lines = source.Replace("\r\n","\n").Split([| '\n' ; '\r'|])
        let p = x.s
        for line in max 0 (p.Line-contextLines) .. min (p.Line+contextLines) (lines.Length-1) do
            yield sprintf "%s" lines.[line]
            if line = p.Line then
                yield sprintf "%s^" (pad p.Column)
    }

/// Expand possibly multiple levels of source positions into a formatted string
let formatSourcePositionList (positions:SourcePosition list) =
    String.Join("; ",positions |> List.map (fun p ->p.Format()))

/// Interface type to allow generic retrieval of a source code position.
type ISourcePosition =
    abstract member OptionalSourcePosition : SourcePosition list with get

let emptySourcePosition = {s = Position.FirstLine(""); e = Position.FirstLine("")}

let getPos (lexbuf: LexBuffer<_>) = {s = lexbuf.StartPos; e = lexbuf.EndPos}

// TODO: We may want to collapse the distinction between Positioned and Node.  At present they
// are identical types except for the record field names.  We may wish to leave them distinct to
// avoid adding cruft to the lexer type, leaving Node free to accumulate more parsing-related
// fields that we don't want to have to add placeholders to.

/// Generic lexer token that stores lexing position.
type Positioned<'T> = {i: 'T; pos: SourcePosition}

type PUnit = Positioned<unit>
type PString = Positioned<string>
type PInt = Positioned<int>
type PFloat = Positioned<float>

/// Tokenize the item in the lex buffer using a pased conversion function.
let tokenize f lexbuf =
    let item = f (lexeme lexbuf)
    {i = item; pos = getPos lexbuf}

/// Create a unit token with position from lexbuf.
let tokenizeUnit = tokenize (ignore)

/// Tokenize a lex item as a string.
let tokenizeString = tokenize id

/// Tokenize a lex item as a string literal, stripping off the quotes.
let tokenizeStringLiteral = tokenize (fun (s: string) -> s.[1..s.Length-2])

/// Tokenize a lex item as an int.
let tokenizeInt = tokenize int

/// Tokenize a lex item as a float.
let tokenizeFloat = tokenize float

/// Tokenize a pragma name by trimming the first character
let tokenizeStringTrimFirstChar lexbuf =
    let name = (lexeme lexbuf).[1..]
    {i = name; pos = getPos lexbuf}

/// Create a new position bracketed by a pair of positions.
let posBracketTokens left right : SourcePosition = {s = left.pos.s; e = right.pos.e}

// =============================
// Wrapper type for all AST nodes.
// =============================

/// Wrapper type for every AST node.
/// This enables adding extensible metadata to the AST for tracking things such as source 
/// code position.  pos tracks position history of this node (when functions expand) with highest level call first
/// with 
[<CustomEquality>][<NoComparison>]
type Node<'T when 'T: equality> = {x: 'T; positions: SourcePosition list}
    with
    /// Override equality to ignore source code position.  We just care about semantic comparison.
    /// This is mostly to aid in testing.  We shouldn't need to be comparing generic AST nodes during parsing.
    override this.Equals other =
        match other with
        | :? Node<'T> as o -> this.x = o.x
        | _ -> false
    

/// Generic helper functions for wrapping node payloads.

/// Push a new source position onto stack of positions during function expansion
/// with first (most recently pushed) value being highest level function call
let prependPositionsNode (newPositions: SourcePosition list) (node: Node<'T>) =
    {node with positions=newPositions@node.positions}

/// Wrap a value in a NodeWrapper without source position.
let nodeWrap x = {x = x; positions = []}

/// Wrap a valye in a NodeWrapper with a position taken from a Positioned token.
let nodeWrapWithTokenPosition (token: Positioned<_>) x = {x = x; positions = [token.pos]}

/// Convert a Positioned token into a node by applying a function to the token's payload.
let tokenAsNodeAfter f (token: Positioned<_>) = {x = f (token.i); positions = [token.pos]}

/// Straight-up convert a Positioned token into a node.
let tokenAsNode (token: Positioned<_>) = {x = token.i; positions = [token.pos]}
 
// ==================
// AST type declaration
// ==================

// ------ Internal data types for certain nodes, non-recursive ------

/// One line of a docstring
type DocstringLine = PString

/// Amino acid vs. dna base mutation
and MType = AA | NT

type Mutation = {f:char; t:char; loc:int; mType:MType}

type Linker = {l1:string; l2:string; orient:string}

type ParseGene = {gene: string; linker:Linker option}

/// Supported binary operations on nodes.
type BinaryOperator = | Add | Subtract | Multiply | Divide

/// Supported types for variables.
/// We need NotYetTyped to allow constructs like let foo = &bar, as we cannot elide a type for &bar at this point.
type GslVarType = | NotYetTyped | IntType | FloatType | StringType | PartType
    with
    /// Print the actual name of the type.
    override x.ToString() =
        match x with
        | NotYetTyped -> "Untyped"
        | IntType -> "Int"
        | FloatType -> "Float"
        | StringType -> "String"
        | PartType -> "Part"

// ------ The AST itself. Node definitions follow. ------

///<summary>
/// Newtype declaration to ensure we don't mix up operations that are intended to operate
/// on single nodes vs. those which recursively operate on an entire tree.  Functions that operate
/// at the nodal level should accept AstNode and return, and those which operate on an entire tree
/// should accept and return one of these wrappers instead.
type AstTreeHead = AstTreeHead of AstNode
    with member x.wrappedNode = match x with AstTreeHead(n) -> n

/// AST for GSL.
and AstNode =
    // leaf nodes that hold values
    | Int of Node<int>
    | Float of Node<float>
    | String of Node<string>
    // docstrings
    | Docstring of Node<string>
    // variable leaf node
    | TypedVariable of Node<string*GslVarType>
    // variable binding
    | VariableBinding of Node<VariableBinding>
    // typed value
    | TypedValue of Node<GslVarType*AstNode>
    // Simple operations on values
    | BinaryOperation of Node<BinaryOperation>
    | Negation of Node<AstNode>
    // Slicing
    | ParseRelPos of Node<ParseRelPos> // parsed relative position, may contain variables and hasn't been built yet
    | RelPos of Node<RelPos> // built relative position, fully specified
    | Slice of Node<ParseSlice>
    // non-slice part mods
    | Mutation of Node<Mutation>
    | DotMod of Node<string>
    // generic part with mods, pragmas, direction
    | Part of Node<ParsePart>
    // AST nodes for base part types
    | Marker of Node<unit>
    | PartId of Node<string>
    | InlineDna of Node<string>
    | InlineProtein of Node<string>
    | HetBlock of Node<unit>
    | Gene of Node<ParseGene>
    | Assembly of Node<AstNode list>
    // AST nodes for Level 2 syntax support
    | L2Id of Node<L2Id>
    | L2Element of Node<L2Element>
    | L2Expression of Node<L2Expression>
    // Roughage support
    | Roughage of Node<Roughage>
    // pragmas
    | ParsePragma of Node<ParsePragma> // pragmas that we've parsed but haven't been "built" yet
    | Pragma of Node<pragmaTypes.Pragma> // pragmas that have had all variables resolved to literals
    // Block of code
    | Block of Node<AstNode list>
    // Function definition and call
    | FunctionDef of Node<ParseFunction>
    | FunctionLocals of Node<FunctionLocals> // in-block declaration of function arguments
    | FunctionCall of Node<FunctionCall>
    // Error during parsing, injected by the parser.
    | ParseError of Node<string>
    // Bootstrapping can turn one node into several that all need to be spliced in.
    // We keep track of this using this node type.
    // It acts purely as an opaque container, so foldmap operations do not recurse into it.
    // In a bootstrapping operation, we create these, and immediately follow with a cleaning step
    // to explode them into their outer contexts.
    | Splice of AstNode []
    with
    /// Get most recent position (highest level in nested function expansion) for node.
    member x.pos = x.positions |> List.tryHead
    /// Return list of all line numbers for the AstNode.  Where multiple line numbers due to function expansion
    /// the highest level call is first and lowest last.
    member x.positions =
        match x with
        | Int(nw) -> nw.positions
        | Float(nw) -> nw.positions
        | String(nw) -> nw.positions
        | Docstring(nw) -> nw.positions
        | TypedVariable(nw) -> nw.positions
        | TypedValue(nw) -> nw.positions
        | VariableBinding(nw) -> nw.positions
        | BinaryOperation(nw) -> nw.positions
        | Negation(nw) -> nw.positions
        | ParseRelPos(nw) -> nw.positions
        | RelPos(nw) -> nw.positions
        | Slice(nw) -> nw.positions
        | Mutation(nw) -> nw.positions
        | DotMod(nw) -> nw.positions
        | Part(nw) -> nw.positions
        | Marker(nw) -> nw.positions
        | PartId(nw) -> nw.positions
        | InlineDna(nw) -> nw.positions
        | InlineProtein(nw) -> nw.positions
        | HetBlock(nw) -> nw.positions
        | Gene(nw) -> nw.positions
        | L2Id(nw) -> nw.positions
        | L2Element(nw) -> nw.positions
        | L2Expression(nw) -> nw.positions
        | Roughage(nw) -> nw.positions
        | ParsePragma(nw) -> nw.positions
        | Pragma(nw) -> nw.positions
        | Block(nw) -> nw.positions
        | FunctionDef(nw) -> nw.positions
        | FunctionLocals(nw) -> nw.positions
        | FunctionCall(nw) -> nw.positions
        | Assembly(nw) -> nw.positions
        | ParseError(nw) -> nw.positions
        | Splice(_) -> []

    /// Get a string representation of the type of this node.
    member x.TypeName = GetUnionCaseName x

// ----- general programming nodes ------
/// A binding from a name to a type and value.
and VariableBinding = {name: string; varType: GslVarType; value: AstNode}

/// A parsed function.
/// Body should be a Block, and the first line of the block should be FunctionLocals.
and ParseFunction = {name: string; argNames: string list; body: AstNode}

/// In-block declaration of the local variables passed in as function arguments.
/// This is used as a placeholder inside the block to allow for easy block-scoped fold operations.
// This is a record type to allow for easy later extension, to add support for advanced features
// like functions with default arguments.
and FunctionLocals = {names: string list}

/// A function invocation.
and FunctionCall = {name: string; args: AstNode list}

/// Binary operation on two nodes.
and BinaryOperation = {op: BinaryOperator; left: AstNode; right: AstNode}

// ----- domain-specific nodes ------ 

/// Parse type for pragmas.  Values may be variables.
and ParsePragma = {name: string; values: AstNode list}

/// Enclosing node for recursively-defined parts.
and ParsePart =
    {basePart: AstNode; mods: AstNode list; pragmas: AstNode list; fwd: bool}

/// Qualifiers on relative positioning specifications.
and RelPosQualifier = | S | E | A | AS | SA | AE | EA

/// Which side of the slice expression does this appear?
and RelPosPosition = | Left | Right

/// Relative positioning.
/// i should reduce to an integer
and ParseRelPos = {i: AstNode; qualifier: RelPosQualifier option; position: RelPosPosition}

/// Slicing.
and ParseSlice = {left: AstNode; lApprox:bool; right: AstNode; rApprox:bool}

// ------ GSL level 2 syntax ------

// At the moment we don't explicitly support variables in Level 2, but we've still broken out
// the constituent parts as AST nodes for future flexibility.

/// Level 2 identifier.
and L2Id = {prefix: Node<string> option; id: Node<string>}
    with
    member x.String =
        match x.prefix with
        | None -> x.id.x
        | Some(prefix) -> sprintf "%s.%s" prefix.x x.id.x

/// Level 2 element, eg pABC1>gDEF2.
/// Both subnodes should resolve to L2Id.
and L2Element = {promoter: AstNode; target: AstNode}

/// A level 2 expression, eg z^ ; a>b ; c > d
and L2Expression = {locus: AstNode option; parts: AstNode list}

// ------ Roughage definitions ------

// GSLc support parsing Roughage as a preprocessor-style step.  Roughage is then converted to L2 GSL.
// Because we end up doing a fairly transparent conversion step, we parse a lot of roughage directly
// into the L2 datatypes.

and RoughagePTPair = {promoter: Node<L2Id>; target: Node<L2Id>}
    with
    member x.ToString(dir) =
        match dir with
        | RoughageFwd -> sprintf "%s>%s" x.promoter.x.String x.target.x.String
        | RoughageRev -> sprintf "%s<%s" x.target.x.String x.promoter.x.String

and RoughagePartDirection =
     | RoughageFwd
     | RoughageRev

/// Single part in a roughage expression.
and RoughageElement =
   {pt1: Node<RoughagePTPair>;
    pt2: Node<RoughagePTPair> option;
    marker: Node<string> option}

/// One classic roughage construct.
and Roughage = {locus: Node<L2Id> option; marker: Node<string> option; parts: Node<RoughageElement> list}
    with
    member x.HasMarker =
        match x.marker with
        | None -> // No marker attached to the locus knockout
            match x.parts |> List.tryPick (fun re -> re.x.marker) with
            | None -> None // No marker attached to a part either
            | Some(mw) -> Some(mw.x)
        | Some(mw) -> Some(mw.x) // Yes there is a marker attached to the locus knockout

/// Helper function for pushing new source code positions onto stack during function expansion
/// Most recently (first) postition refers to highest level function invocation.
let prependPositionsAstNode (newPos:SourcePosition list) (x:AstNode) =
    match x with
    | Int(nw) -> Int(prependPositionsNode newPos nw)
    | Float(nw) -> Float(prependPositionsNode newPos nw)
    | String(nw) -> String(prependPositionsNode newPos nw)
    | Docstring(nw) -> Docstring(prependPositionsNode newPos nw)
    | TypedVariable(nw) -> TypedVariable(prependPositionsNode newPos nw)
    | TypedValue(nw) -> TypedValue(prependPositionsNode newPos nw)
    | VariableBinding(nw) -> VariableBinding(prependPositionsNode newPos nw)
    | BinaryOperation(nw) -> BinaryOperation(prependPositionsNode newPos nw)
    | Negation(nw) -> Negation(prependPositionsNode newPos nw)
    | ParseRelPos(nw) -> ParseRelPos(prependPositionsNode newPos nw)
    | RelPos(nw) -> RelPos(prependPositionsNode newPos nw) 
    | Slice(nw) -> Slice(prependPositionsNode newPos nw)
    | Mutation(nw) -> Mutation(prependPositionsNode newPos nw)
    | DotMod(nw) -> DotMod(prependPositionsNode newPos nw)
    | Part(nw) -> Part(prependPositionsNode newPos nw)
    | Marker(nw) -> Marker(prependPositionsNode newPos nw)
    | PartId(nw) -> PartId(prependPositionsNode newPos nw)
    | InlineDna(nw) -> InlineDna(prependPositionsNode newPos nw)
    | InlineProtein(nw) -> InlineProtein(prependPositionsNode newPos nw)
    | HetBlock(nw) -> HetBlock(prependPositionsNode newPos nw)
    | Gene(nw) -> Gene(prependPositionsNode newPos nw)
    | L2Id(nw) -> L2Id(prependPositionsNode newPos nw)
    | L2Element(nw) -> L2Element(prependPositionsNode newPos nw)
    | L2Expression(nw) -> L2Expression(prependPositionsNode newPos nw)
    | Roughage(nw) -> Roughage(prependPositionsNode newPos nw)
    | ParsePragma(nw) -> ParsePragma(prependPositionsNode newPos nw)
    | Pragma(nw) -> Pragma(prependPositionsNode newPos nw)
    | Block(nw) -> Block(prependPositionsNode newPos nw)
    | FunctionDef(nw) -> FunctionDef(prependPositionsNode newPos nw)
    | FunctionLocals(nw) -> FunctionLocals(prependPositionsNode newPos nw)
    | FunctionCall(nw) -> FunctionCall(prependPositionsNode newPos nw)
    | Assembly(nw) -> Assembly(prependPositionsNode newPos nw)
    | ParseError(nw) -> ParseError(prependPositionsNode newPos nw)
    | Splice(_)  as x -> x // Slices are defined to have no position so don't try to update (see .pos below) 

// ------ Active patterns on the AST of general interest ------

// Note: active patterns allow expressing complex match idioms in a compact syntax.
// This technique allows us to create "categories" of nodes and use them in pattern matching,
// as well as create helpful unpackings of those node structures.

/// An active pattern to match only leaf nodes.
/// As the tree grows new leaves, this pattern should be updated which will automatically
/// propagate to all of the clients of this pattern.
let (|Leaf|_|) node =
    match node with
    | Int(_) | Float(_) | String(_)
    | Docstring(_)
    | DotMod(_) | Mutation(_)
    | Marker(_) | PartId(_) | InlineDna(_) | InlineProtein(_) | HetBlock(_) | Gene(_)
    | TypedVariable(_) | FunctionLocals(_)
    | L2Id(_)
    | Roughage(_)
    | Pragma(_) | RelPos(_) // the built version of these have no children
    | ParseError(_)
    | Splice(_) -> Some(node)
    | _ -> None

/// Match parts and their base parts together as a pair.
/// Unpacks both nodes for convenience.
// TODO: add other part combos as we need them
let (|AssemblyPart|GenePart|RecursivePart|Other|) node =
    match node with
    | Part(pw) ->
        match pw.x.basePart with
        | Assembly(aw) -> AssemblyPart(pw, aw)
        | Gene(gp) -> GenePart(pw, gp)
        | Part(pwInner) -> RecursivePart(pw, pwInner)
        | _ -> Other
    | _ -> Other

/// Match all nodes which are valid as base parts.
let (|ValidBasePart|_|) node =
    match node with
    | TypedVariable(_) | PartId(_)
    | Marker(_) | InlineDna(_) | InlineProtein(_) | HetBlock(_) | Gene(_)
    | Part(_) | Assembly(_) -> Some node
    | _ -> None

/// Match all nodes which have no literal representation in source code.
let (|BookkeepingNode|_|) node = 
    match node with
    | FunctionLocals(_) -> Some node
    | _ -> None

/// Extract the type of a node if it is a numeric variable.
let (|IntVariable|FloatVariable|OtherVariable|NotAVariable|) node =
    match node with
    | TypedVariable({x=(_, t); positions=_}) ->
        match t with
        | IntType -> IntVariable
        | FloatType -> FloatVariable
        | _ -> OtherVariable(t)
    | _ -> NotAVariable

/// Match nodes allowed in math expressions.
let (|AllowedInMathExpression|_|) node =
    match node with
    | Int _ | IntVariable -> Some node
    | _ -> None

/// Match variable declarations that effectively create a pathological self-reference.
let (|SelfReferentialVariable|_|) node =
    match node with
    | VariableBinding(vb) ->
        match vb.x.value with
        | TypedVariable(vbInner) -> // variable pointing to another variable
            if vb.x.name = fst vbInner.x then Some vb
            else None
        | _ -> None
    | _ -> None


// ====================
// helper functions for creating AST nodes in the parser
// ====================

// ------ deriving source code positions ------

/// Create a new position bracketed by a pair of positions.
let posBracket (left: AstNode) (right: AstNode) : SourcePosition option =
    match left.pos, right.pos with
    | Some(lp), Some(rp) -> Some({s = lp.s; e = rp.e})
    | _ -> None

/// Create a new position bracketed by the first and last item in a list of nodes that have positions
let posFromList (nodes: AstNode list) : SourcePosition option =
    let rec go (head: AstNode option) (tail: AstNode list) =
        match head, tail with
        | Some(h), [] -> h.pos
        | Some(h), [t] -> posBracket h t
        | Some(h), _::tl -> go (Some(h)) tl
        | None, [] -> None
        | None, [t] -> t.pos
        | None, hd::tl -> go (Some(hd)) tl

    go None nodes


// ------ general-purpose ------

/// Create a parse error with message and position.
let createParseError msg pos = ParseError({x=msg; positions=pos})

/// Wrap a value with position taken from another node.
let nodeWrapWithNodePosition (node: AstNode) v = {x = v; positions = node.positions}

/// Convert a string token to a TypedVariable
let tokenToVariable (token: PString) (t: GslVarType) : AstNode =
    let name = token.i
    TypedVariable(nodeWrapWithTokenPosition token (name, t))

// ------ general programming ------

/// Parse two integer literals separated by a dot as a float.
let createFloat (intPart: PInt) (fracPart: PInt) : AstNode =
    // position is bracketed by the two pieces
    let pos: SourcePosition = {s = intPart.pos.s; e = fracPart.pos.e}
    let v = sprintf "%d.%d" intPart.i fracPart.i |> float
    Float({x = v; positions = [pos]})

/// Create a binary operation node from two other AST nodes.
let createBinaryOp op left right =
    BinaryOperation({x = {op = op; left = left; right = right}; positions = posBracket left right |> Option.toList})

/// Create an AST node for negation.
let negate node = Negation(nodeWrapWithNodePosition node node)

/// Create an AST node for a typed variable declaration.
// TODO: improve positioning
let createVariableBinding name varType value =
    VariableBinding(nodeWrapWithTokenPosition name {name=name.i; varType=varType; value=value})
   
/// Create an AST node for a typed value passed to a function argument.
let createTypedValue t v = TypedValue(nodeWrapWithNodePosition v (t, v))

/// Create an AST node for a function declaration.
let createFunctionDeclaration name args bodyLines =
    let functionLocals = FunctionLocals(nodeWrapWithTokenPosition name {names = args})
    // tack the function local variables on the front of the block
    let block = Block(nodeWrap (functionLocals::bodyLines))
    FunctionDef(nodeWrapWithTokenPosition name {name = name.i; argNames = args; body = block})

/// Create an AST node for a function call.
let createFunctionCall name args =
    FunctionCall(nodeWrapWithTokenPosition name {name = name.i; args = args})

/// Create a pragma from pieces.
let createPragma pname pvals =
    // Take position from the name.  Could try to be fancier here in the future.
    ParsePragma(nodeWrapWithTokenPosition pname {name = pname.i; values = pvals})


// ------ creating nodes for parts and assemblies ------


let private stringToRelPosQualifier (s: string) =
    match s.ToUpper() with
    | "S" -> S | "E" -> E | "A" -> A | "AS" -> AS | "SA" -> SA | "AE" -> AE | "EA" -> EA
    | x -> failwithf "%s is not a valid qualifier for a relative position." x

let relPosQualifierToString rpq =
    match rpq with
    | S -> "S" | E -> "E" | A -> "A"
    | AS -> "AS" | SA -> "SA"
    | AE -> "AE" | EA -> "EA"

/// Encode the logic for parsing and computing relative positions in slices.
/// Use the position from the number as the position of this token.
let createParseRelPos number (qualifier: PString option) position =
    match qualifier with
    | None ->
        // basic case, just given a number
        ParseRelPos(nodeWrapWithNodePosition number {i = number; qualifier = None; position = position})
    | Some(q) ->
        // We've been passed a qualifying string.  Parse it as a valid union case.
        let qual = stringToRelPosQualifier q.i
        ParseRelPos(nodeWrapWithNodePosition number {i = number; qualifier = Some qual; position = position})

/// Create a parse slice AST node.
let createParseSlice (leftRPInt, leftRPQual) (rightRPInt, rightRPQual) lApprox rApprox =
    let left = createParseRelPos leftRPInt leftRPQual Left
    let right = createParseRelPos rightRPInt rightRPQual Right
    let pos = posBracket left right |> Option.toList
    Slice({x = {left = left; right = right; lApprox = lApprox; rApprox = rApprox}; positions = pos})


/// Create a mutation AST node.
let createMutation (s: PString) mutType =
    let mutStr = s.i
    let f = mutStr.[1]
    let t = mutStr.[mutStr.Length-1]
    let pos = Convert.ToInt32(mutStr.[2..mutStr.Length-2])
    let mut = {f = f; t = t; loc = pos; mType = mutType}
    Mutation({x = mut; positions = [s.pos]})

/// Create a top-level part.
let createPart mods pragmas basePart =
    Part({x = {basePart = basePart; mods = mods; pragmas = pragmas; fwd = true}; positions = basePart.positions})

/// Create a top-level part with empty collections and default values from a base part.
let createPartWithBase = createPart [] []

/// Create a top-level part given a gene ID.
let createGenePart (gene: PString) (linker: Linker option) =
    // The base part for this part will be a Gene AST node.
    createPartWithBase (Gene({x = {gene = gene.i; linker = linker}; positions = [gene.pos]}))

/// Capture a list of parsed mods and stuff them into their associated part.
let stuffModsIntoPart astPart mods =
    match astPart with
    | Part(pw) ->
        let part = pw.x
        let stuffedPart = {part with mods = part.mods@mods}
        Part(nodeWrapWithNodePosition astPart stuffedPart)
    | x -> failwithf "Mods may only be applied to Parts.  Tried to apply mods to %A." x

/// Capture a list of parsed inline pragmas and stuff them into their associated part.
let stuffPragmasIntoPart astPart prags =
    match astPart with
    | Part(pw) ->
        let part = pw.x
        let stuffedPart = {part with pragmas = part.pragmas@prags}
        Part(nodeWrapWithNodePosition astPart stuffedPart)
    | x -> failwithf "Inline pragmas may only be applied to Parts.  Tried to apply pragmas to %A." x

/// Reverse the direction of a part.
let revPart astPart =
    match astPart with
    | Part(pw) ->
        Part(nodeWrapWithNodePosition astPart {pw.x with fwd = false})
    | x -> failwithf "Can only apply the ! operator to Parts.  Tried to reverse a %A." x

/// Create a part whose base part is an assembly of the passed list of parts.
let createAssemblyPart parts =
    let pos = posFromList parts
    let assem = Assembly({x = parts; positions = pos|> Option.toList})
    createPart [] [] assem

// ------ creating level 2 GSL nodes ------

let createL2IdNode (prefix: Node<string> option) (id: Node<string>) =
    let pos =
        match prefix with
        | Some(p) -> posBracket (String(p)) (String(id)) |> Option.toList // be lazy and wrap these as nodes to use existing function
        | None -> id.positions
    {x={prefix = prefix; id = id}; positions=pos}

/// Create a level 2 id from optional prefix and id
let createL2Id prefix id = L2Id(createL2IdNode prefix id)

/// Create a level 2 element from a promoter and target.
/// Promoter and target should be L2 IDs.
let createL2Element (promoter: AstNode) (target: AstNode) =
    let pos = posBracket promoter target |> Option.toList
    L2Element({x={promoter=promoter; target=target}; positions=pos})

/// Create a level 2 expression from optional locus and list of elements.
let createL2Expression (locus: AstNode option) (parts: AstNode list) =
    let pos =
        match locus with
        | Some(l) -> posFromList (l::parts)
        | None -> posFromList parts
    L2Expression({x={locus=locus; parts=parts}; positions=pos |> Option.toList})

// ------ creating Roughage AST node ------

let createRoughagePart dir (p: Node<L2Id>) (t: Node<L2Id>) =
    let elem : RoughagePTPair = {promoter = p; target = t}
    let pos =
        match dir with
        | RoughageFwd -> posBracket (L2Id(p)) (L2Id(t))
        | RoughageRev -> posBracket (L2Id(t)) (L2Id(p))
    {x=elem; positions=pos |> Option.toList}

let createRoughageElement partFwd partRev marker =
    let pos = partFwd.positions
    {x={pt1 = partFwd; pt2 = partRev; marker = marker}; positions = pos}

let createRoughageLine (locus, marker) parts =
    // be lazy and use the position of whatever the first part is
    let pos =
        match parts with
        | [] -> []
        | hd::_ -> hd.positions
    Roughage({x={locus=locus; marker=marker; parts=parts}; positions=pos})
