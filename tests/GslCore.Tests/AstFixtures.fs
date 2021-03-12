/// Fixtures and fixture creation for testing the AST.
module AstFixtures
open System
open NUnit.Framework
open Amyris.ErrorHandling
open AstExpansion
open AstTypes
open AstErrorHandling
open AstProcess
open LexAndParse
open constants


/// Parse text and return the contents of the top-level block.
/// Raise an exception if this fails.
let bootstrapParseOnly source =
    match lexAndParse true (GslSourceCode(source)) with | Ok(AstTreeHead(Block(b)), _) -> b.x
    

let wrapInt v = Int(nodeWrap v)
let wrapFloat v = Float(nodeWrap v)
let wrapString v = String(nodeWrap v)

/// Wrap AST items as a block.
let blockify items = Block(nodeWrap items)

/// Group AST items as a block, wrapped as a whole tree.
let treeify items = AstTreeHead(blockify items)

/// Make a bare-bones assembly part out of some parts.
let assemble = createAssemblyPart

let addPragsToPart prags a =
    match a with
    | Part(pw) -> Part({pw with x={pw.x with pragmas = prags}})

let variableize name v =
    let t =
        match v with
        | BinaryOperation(_) -> NotYetTyped // right now we only support integer math so this is OK
        | Int(_) -> NotYetTyped
        | Float(_) -> NotYetTyped
        | String(_) -> NotYetTyped
        | Part(_) -> PartType
    VariableBinding(nodeWrap {name=name; varType=t; value=v})

let functionalize name args bodyLines =
    let locals = FunctionLocals(nodeWrap {names = args})
    FunctionDef(nodeWrap {name = name; argNames = args; body = blockify (locals::bodyLines)})

let emptyBlock = Block(nodeWrap [])
let fooEqual1 = variableize "foo" (wrapInt 1)

let namePragmaFoo = ParsePragma(nodeWrap {name = "name"; values = [String(nodeWrap "foo")]})


let createGenePart name = Gene(nodeWrap {gene = name; linker = None})
// fixtures and helper functions for parts
let fooGene = createGenePart "gFOO"

let createPart mods prags basePart = Part(nodeWrap {basePart = basePart; mods = mods; pragmas = prags; fwd = true})

let basePartWrap = createPart [] []

let fooGenePart = basePartWrap fooGene

let relPosLeft = ParseRelPos(nodeWrap {i = Int(nodeWrap 20); qualifier = None; position = Left})
let relPosRight = ParseRelPos(nodeWrap {i = Int(nodeWrap 200); qualifier = None; position = Right})
let testSlice = Slice(nodeWrap {left = relPosLeft; right = relPosRight; lApprox = true; rApprox = true})

let fooGeneWithSlice = createPart [testSlice] [] fooGene

let fooGeneWithPragma = createPart [] [namePragmaFoo] fooGene

let partVariable name = basePartWrap (TypedVariable(nodeWrap (name, PartType)))

let typedValue t v = TypedValue(nodeWrap (t, v))
