module LegacyParseTypes
open Microsoft.FSharp.Text.Lexing
open Amyris.Bio.primercore
open Amyris.ErrorHandling
open Amyris
open Amyris.Dna
open constants
open System
open uri
open pragmaTypes
open AstTypes
open AstProcess
open AstErrorHandling
open DesignParams

type Slice = {left:RelPos; lApprox:bool; right:RelPos; rApprox:bool}

type SliceContext =
    | Genomic
    | Library of string // string payload for helpful error message

/// Return a tuple of OneOffset left/right slice bounds from a slice record.
/// These bounds are both relative to the FivePrime end.
/// Requires the length of the feature being sliced to be interpreted correctly.
let getBoundsFromSlice (slice: Slice) featureLength context =
    let left =
        match slice.left.relTo with
        | FivePrime -> slice.left.x
        | ThreePrime -> (featureLength+1)*1<OneOffset> + slice.left.x
    let right =
        match slice.right.relTo with
        | FivePrime -> slice.right.x
        | ThreePrime -> (featureLength+1)*1<OneOffset> + slice.right.x

    match context with
    | Genomic ->
        // no validation necessary
        ok (left, right)
    | Library partId ->
        // the slice bounds are not allowed to fall outside the feature as we don't have
        // data on flanking regions in this context
        if left < 1<OneOffset> || right <= left || right > (featureLength*1<OneOffset>) then
            fail (sprintf
                "Illegal slice (%A) outside core gene range for library item %s."
                slice partId)
        else ok (left, right)

type Mod = 
    | MUTATION of Mutation
    | SLICE of Slice
    | DOTMOD of string

type PartIdLegacy = {id:string; mods:Mod list}
   
type GenePart = {gene:string; mods:Mod list; where: SourcePosition list}

type GenePartWithLinker = {part:GenePart; linker:Linker option}
type Part =
    | GENEPART of GenePartWithLinker 
    | MARKERPART
    | INLINEDNA of Dna 
    | INLINEPROT of string 
    | HETBLOCK 
    | SOURCE_CODE of GslSourceCode
    | PARTID of PartIdLegacy

/// Part plus a Pragma
and PPP = { part : Part ; pr : PragmaCollection ; fwd: bool}

/// Namespace bounded tag for an assembly (Used in Assembly)
type AssemblyTag = {nameSpace:string ; tag : string}

type Assembly =
   {parts: PPP list; 
    name: string option;
    uri: Uri option;
    linkerHint: string; 
    pragmas: PragmaCollection; 
    designParams: DesignParams;
    capabilities: Capabilities; 
    docStrings: string list;
    sourcePosition: SourcePosition list}
    interface ISourcePosition with
        member x.OptionalSourcePosition = x.sourcePosition


// ================================================
// Level 2 Definitions
// ================================================

/// Element of a level 2 line  e.g.  pABC1>gDEF2
type BuiltL2Element = {promoter:AstNode; target:L2Id}

/// L2 Top level container for the expression line  z^ ; a>b ; c > d etc
type BuiltL2Expression = {l2Locus:L2Id option; parts:BuiltL2Element List}

/// L2 Top level container
type L2Line =
   {l2Design: BuiltL2Expression; 
    name: string option;
    uri: Uri option; 
    pragmas: PragmaCollection; 
    capabilities: Capabilities}

// ========================
// pretty-printing legacy assemblies as GSL source code
// ========================

/// Pretty print a RelPos
let printRP (l:RelPos) = sprintf "%A/%s" l.x (match l.relTo with | FivePrime -> "S" | ThreePrime -> "E")
let printSlice (s:Slice) = 
    sprintf "[%s%A%s:%s%A%s]" 
        (if s.lApprox then "~" else "") s.left.x (match s.left.relTo with | FivePrime -> "S" | ThreePrime -> "E") 
        (if s.rApprox then "~" else "") s.right.x (match s.right.relTo with | FivePrime -> "S" | ThreePrime -> "E")    

let expandMods (ml:Mod list) =
        seq {
                for m in ml do
                    match m with
                        | MUTATION(m) ->
                            yield sprintf
                                "%c%c%d%c"
                                (match m.mType with | AA -> '$' | NT -> '*')
                                m.f
                                m.loc
                                m.t
                        | SLICE(s) -> yield printSlice s 
                        | DOTMOD(d) -> yield sprintf ".%s" d
                } |> fun x -> String.Join("",x)   

let rec printPPP ppp = 
    let partOut = 
        match ppp.part with
        | HETBLOCK -> "~ " // don't do anything at this level
        | INLINEDNA(s) -> sprintf "/%O/ " s   // inline DNA sequence
        | INLINEPROT(s) -> sprintf "/$%s/ " s // inline protein sequence
        | MARKERPART -> "### "
        | PARTID(p) -> sprintf "@%s" p.id + (expandMods p.mods)
        | SOURCE_CODE(s) -> s.String // Part that was already expanded into a string
        | GENEPART(gp) ->
            let lOut =
                match gp.linker with
                    | None -> ""
                    | Some(l) ->
                        sprintf "%s-%s-%s-" l.l1 l.l2 l.orient // Emit linker
            let p = gp.part
                            
            let gOut = p.gene
            let modOut = expandMods p.mods
                                
            lOut + gOut + modOut // String.Join("",Array.ofSeq modOut)   
    // Now add in any inline pragma part with braces, ; separated etc
    let prOut = 
        if ppp.pr.pmap.Count=0 then "" 
        else 
            ppp.pr.pmap
            |> Seq.map (fun pv -> sprintf "#%s %s" pv.Key (pv.Value.args |> String.concat " "))
            |> fun ss -> String.Join(";",ss)
            |> sprintf "{%s}"
    (if ppp.fwd then "" else "!") + partOut + prOut

/// Pretty print a built GSL assembly
let prettyPrintAssembly (assembly: Assembly) =
    [for ppp in assembly.parts -> printPPP ppp]
    |> String.concat ";"
    |> GslSourceCode

// =====================
// conversion from AST parts to legacy parts
// =====================

let private sliceFromAstSlice (s: ParseSlice) =
    match s.left, s.right with
    | RelPos(lw), RelPos(rw) -> 
        ok (SLICE({left = lw.x; lApprox = s.lApprox; right = rw.x; rApprox = s.rApprox}))
    | x, y ->
        let contextStr = sprintf "legacy slice construction; found [%s:%s]" x.TypeName y.TypeName
        internalTypeMismatch (Some(contextStr)) "RelPos" x

let private astNodeToLegacyMod node =
    match node with
    | Slice(sw) -> sliceFromAstSlice sw.x
    | Mutation(mw) -> ok (MUTATION(mw.x))
    | DotMod(dm) -> ok (DOTMOD(dm.x))
    | _ -> internalTypeMismatch (Some "legacy mod conversion") "Slice or Mutation or DotMod" node

let private convertMods mods = mods |> List.map astNodeToLegacyMod |> collect

/// Convert an AST base part into a legacy Part.
let private createLegacyPart part =
    match part.x.basePart with
    | Gene(gw) ->
        convertMods part.x.mods
        >>= (fun mods ->
            let genePart = {gene = gw.x.gene; mods = mods; where = gw.positions}
            ok (GENEPART({part=genePart; linker=gw.x.linker})))
    | Marker(_) -> ok MARKERPART
    | InlineDna(s) -> ok (INLINEDNA(Dna(s.x, true, AllowAmbiguousBases)))
    | InlineProtein(s) -> ok (INLINEPROT s.x)
    | HetBlock(_) -> ok HETBLOCK
    | PartId(p) ->
        convertMods part.x.mods
        >>= (fun mods -> ok (PARTID({id = p.x; mods=mods})))
    | x -> internalTypeMismatch (Some "legacy part conversion") "legacy-compatible base part" x

let private createPPP part =
    match part with
    | Part(p) ->
        createLegacyPart p
        >>= (fun legacyPart -> ok {part = legacyPart; pr = getPragmas p; fwd = p.x.fwd})
    | x -> internalTypeMismatch (Some "legacy part conversion") "Part" x

/// For assembly conversion, we need to accumulate both a pragma environment and docstrings.
/// Combine these two accumulation functions and state datastructures.
type AssemblyConversionContext = {pragmaEnv: PragmaEnvironment; docs: DocstringEnvironment}

let emptyConversionContext = {pragmaEnv = emptyPragmaEnvironment; docs = emptyDocstringEnvironment}

/// Accumulate both pragma and docstring context.
let updateConversionContext mode s node =
    let newPragmaEnv = updatePragmaEnvironment mode s.pragmaEnv node
    let newDocsEnv = updateDocstringEnvironment mode s.docs node
    {s with pragmaEnv = newPragmaEnv; docs = newDocsEnv}

/// Convert an AST assembly into a legacy assembly.
let convertAssembly (context: AssemblyConversionContext) (pw, aplw) =
    let assemblyPragmas = getPragmas pw
    let name = assemblyPragmas.TryGetOne("name")
    let uri = assemblyPragmas.TryGetOne("uri")
    let linkerHint =
        match assemblyPragmas.TryGetValues("linkers") with
        | Some(vals) -> (String.concat "" vals)
        | None -> ""
    designParamsFromPragmas initialDesignParams assemblyPragmas
    |> mapMessages (fun s -> errorMessage PragmaError s (Part(pw)))
    |> tupleResults (aplw.x |> List.map createPPP |> collect)
    >>= (fun (parts, designParams) ->
            ok {parts = parts;
                name = name;
                uri = uri;
                linkerHint = linkerHint; 
                pragmas = assemblyPragmas; 
                designParams = designParams;
                capabilities = context.pragmaEnv.capabilities; 
                docStrings = context.docs.assigned;
                sourcePosition = pw.positions})

// ======================
// conversion from L2 AST node to legacy L2 line type
// ======================
   
/// Build a concrete L2 element from an AST node.
let private buildL2Element node =
    match node with
    | L2Element(nw) ->
        match nw.x.promoter, nw.x.target with
        | L2Id(_), L2Id(tw) | Part(_), L2Id(tw) ->
            ok {promoter=nw.x.promoter; target=tw.x}
        
        | x, y ->
            let contextStr = sprintf "L2 element construction; found [%s>%s]" x.TypeName y.TypeName
            internalTypeMismatch (Some(contextStr)) "L2Id" node
    | x ->
        internalTypeMismatch (Some("L2 element construction")) "L2Id" x

let private unpackLocus nodeopt =
    match nodeopt with
    | Some(L2Id(lw)) -> ok (Some(lw.x))
    | Some(x) -> internalTypeMismatch (Some("L2 locus unpacking")) "L2Id" x
    | None -> ok None

/// Build a concrete L2 expression from an AST node.
let private buildL2Expression (ew: Node<L2Expression>) =
    ew.x.parts
    |> List.map buildL2Element
    |> collect
    |> tupleResults (unpackLocus ew.x.locus)
    >>= (fun (locus, parts) ->
        ok {l2Locus = locus; parts = parts})

/// Build a L2Line from an AST node and pragma environment.
let convertL2Line (pragmaEnv: PragmaEnvironment) (l2e: Node<L2Expression>) =
    let pragmas = pragmaEnv.persistent.MergeIn(pragmaEnv.assignedTransients)
    let name = pragmas.TryGetOne("name")
    let uri = pragmas.TryGetOne("uri")
    buildL2Expression l2e
    >>= (fun l2Design ->
        ok {l2Design = l2Design;
            name = name;
            uri = uri;
            pragmas = pragmas;
            capabilities = pragmaEnv.capabilities})
