/// AST versions of biological expansions
module AstExpansion
open System
open constants
open AstTypes
open AstLinting
open AstProcess
open AstErrorHandling
open AstAlgorithms
open Amyris.ErrorHandling
open RefGenome
open LegacyParseTypes
open DesignParams
open Amyris.Bio
open Amyris.Dna
open DnaCreation
open pragmaTypes
open alleleSwaps
open Amyris.Bio.utils
open applySlices
open commonTypes
open resolveExtPart
open LexAndParse
open PluginTypes
open FetchPart

// ==================
// phase 1 of AST reduction
// everything before bioinformatics gets involved
// ==================

let immediateValidations = validate (checkParseError &&& validBasePart)

/// Phase 1 is everything before bioinformatics really gets involved.
let phase1 legalCapas = 
    linters
    >=> immediateValidations
    >=> checkRecursiveCalls
    >=> resolveVariables
    >=> inlineFunctionCalls
    >=> stripFunctions
    >=> resolveVariablesStrict
    >=> stripVariables
    >=> reduceMathExpressions
    >=> buildPragmas legalCapas
    >=> collectWarnings
    >=> buildRelativePositions
    >=> expandRoughageLines // inline roughage expansion is pretty simple so we always do it
    >=> flattenAssemblies
    >=> (validate checkMods)
    >=> stuffPragmasIntoAssemblies

/// Prep a tree for phase 2, after phase 1 compilation is complete.
let prepPhase2 rgs library =
    checkGeneNames rgs library
    >=> nameAssemblies

// ==================
// bootstrapping literal source into an AST node
// ==================

let bootstrapError expectedType note tree =
    let extraText =
        match note with
        | Some(n) -> sprintf " %s" n
        | None -> ""
    let msg = sprintf "Unable to unpack as a '%s'.%s" expectedType extraText
    error (BootstrapError(Some(tree))) msg tree

/// Bootstrapped expansion phases don't have meaningful source positions as a result of expansion.
/// Instead, replace all of the positions with one provided from the external context to at least
/// locate the source of the error to the line in the original input source.
let private replaceSourcePosition pos node =
    match node with
    | Int(nw) -> Int({nw with positions = pos})
    | Float(nw) -> Float({nw with positions = pos})
    | String(nw) -> String({nw with positions = pos})
    | Docstring(nw) -> Docstring({nw with positions = pos})
    | TypedVariable(nw) -> TypedVariable({nw with positions = pos})
    | TypedValue(nw) -> TypedValue({nw with positions = pos})
    | VariableBinding(nw) -> VariableBinding({nw with positions = pos})
    | BinaryOperation(nw) -> BinaryOperation({nw with positions = pos})
    | Negation(nw) -> Negation({nw with positions = pos})
    | ParseRelPos(nw) -> ParseRelPos({nw with positions = pos})
    | RelPos(nw) -> RelPos({nw with positions = pos})
    | Slice(nw) -> Slice({nw with positions = pos})
    | Mutation(nw) -> Mutation({nw with positions = pos})
    | DotMod(nw) -> DotMod({nw with positions = pos})
    | Part(nw) -> Part({nw with positions = pos})
    | Marker(nw) -> Marker({nw with positions = pos})
    | PartId(nw) -> PartId({nw with positions = pos})
    | InlineDna(nw) -> InlineDna({nw with positions = pos})
    | InlineProtein(nw) -> InlineProtein({nw with positions = pos})
    | HetBlock(nw) -> HetBlock({nw with positions = pos})
    | Gene(nw) -> Gene({nw with positions = pos})
    | L2Id(nw) -> L2Id({nw with positions = pos})
    | L2Element(nw) -> L2Element({nw with positions = pos})
    | L2Expression(nw) -> L2Expression({nw with positions = pos})
    | Roughage(nw) -> Roughage({nw with positions = pos})
    | ParsePragma(nw) -> ParsePragma({nw with positions = pos})
    | Pragma(nw) -> Pragma({nw with positions = pos})
    | Block(nw) -> Block({nw with positions = pos})
    | FunctionDef(nw) -> FunctionDef({nw with positions = pos})
    | FunctionLocals(nw) -> FunctionLocals({nw with positions = pos})
    | FunctionCall(nw) -> FunctionCall({nw with positions = pos})
    | Assembly(nw) -> Assembly({nw with positions = pos})
    | ParseError(nw) -> ParseError({nw with positions = pos})
    | Splice(x) -> Splice(x)

/// Replace all source positions in a bootstrapped expanded tree with the position of the node
/// that was expanded into source.
let private replaceSourcePositions pos = map Serial TopDown (promote (replaceSourcePosition pos))

/// If any messages emanated from a bootstrapped parsing, replace their positions with the input position.
let private replaceMessagePositions pos =
    mapMessages (fun (msg: AstMessage) -> {msg with sourcePosition = pos})

///<summary>
/// Later phases of the compiler currently output literal source code which is parsed again.
/// This function accepts literal source code which is parsed and the resuling AST is run
/// through the provided operation.  This function unpacks the contents of the top-level
/// block that results from compilation and re-packs it as a Splice, to indicate
/// to a subsequent expansion pass that this node needs to be unpacked into its outer context.
///</summary>
let bootstrap originalPosition (op: AstTreeHead -> TreeTransformResult) (source: GslSourceCode) =
    /// Unpack a bootstrapped AST to a block or fail.
    let asBlock tree =
        match tree with
        | AstTreeHead(Block(nw)) -> ok (Splice(Array.ofList nw.x))
        | AstTreeHead(node) -> bootstrapError "Block" None node

    let contextMsg =
        sprintf
            "An error occurred while parsing this internally-generated GSL source code:\n%s"
            source.String

    lexAndParse false source
    |> addContextIfError
        (errorMessage
            (InternalError(ParserError))
            contextMsg
            (String({x=source.String; positions=originalPosition})))
    >>= (replaceSourcePositions originalPosition)
    >>= op
    >>= asBlock

/// Parse string source code, run compiler phase 1, and return the resulting contents of the
/// top-level block.
let bootstrapPhase1 legalCapas originalPosition = bootstrap originalPosition (phase1 legalCapas)

// =================
// splicing bootstraps back into the tree
// =================

/// Determine if a list of nodes contains any splices.
let private containsSplice nodes =
    nodes
    |> List.tryPick (fun node -> match node with Splice(_) -> Some(node) | _ -> None)
    |> Option.isSome

/// Explode Splice nodes into their enclosing context.
/// They can appear in Blocks or Assemblies.
let private healSplice node =
    match node with
    | Block(bw) -> 
        let nodeList = bw.x
        // if no splices, do nothing
        if not (containsSplice nodeList) then Block(bw)
        else
            let newNodeList =
                nodeList // make an array out of each node
                |> List.map (fun node ->
                    match node with
                    | Splice(newNodes) -> newNodes
                    | x -> [| x |])
                |> Array.concat // concat the arrays
                |> List.ofArray
            Block({bw with x=newNodeList})
    | _ -> node

/// Explode all Splices into their enclosing context.
let healSplices = map Serial TopDown (promote healSplice)


// ==================================
// structure of a bootstrapped expansion step
// ==================================

/// Convert an assembly into a Splice using an expansion function and a bootstrap operation.
/// Since the expansion function may raise an exception, we capture that exception
/// and inject it into the result stream.
let bootstrapExpandLegacyAssembly
        errorMsgType
        (expansionFunction: Assembly -> GslSourceCode)
        bootstrapOperation
        assemblyConversionContext
        node
    : NodeTransformResult =
    /// Perform the expansion operation, capturing any exception as an error.
    let expandCaptureException =
        expansionFunction
        |> captureException (exceptionToError errorMsgType node)
    match node with
    | AssemblyPart(apUnpack) ->
        convertAssembly assemblyConversionContext apUnpack
        >>= expandCaptureException
        >>= (bootstrapOperation ((fst apUnpack).positions))
    | _ -> ok node

/// Execute a complete bootstrapped expansion on an AST.
/// Runs foldmap on the provided expansion function, followed by
/// an operation that heals all of the scars in the AST left by the expansion.
/// This is necessary because some bootstrapped expansion phases convert a single
/// node into a miniature block, which we want to expand into the outer context.
let executeBootstrap bootstrappedExpansionFunction mode (tree: AstTreeHead) =
    foldmap // run the bootstrapped expand operation
        mode
        TopDown
        updateConversionContext
        emptyConversionContext
        bootstrappedExpansionFunction
        tree
    >>= healSplices // heal the splices
    >>= stuffPragmasIntoAssemblies // Bootstrapped assemblies need their pragma environment reinjected

// ==========================
// expanding L2 GSL
// ==========================

/// Core expansion of a single L2 expression line.
let private expandL2Expression
        (providers: L2Provider list)
        (rgs:GenomeDefs)
        (construct : L2Line)
        : GslSourceCode =
    let line = construct.l2Design
    let pragmas = construct.pragmas
    let capabilities = construct.capabilities
    // TODO TODO:   ensuring key parts must be valid GSL parts e.g. can't titrate a non g part,  can't delete a non genome part
       
    //
    // Different styles of expansion
    //
    // Explicit locus
    ////////////////////////////////////
    // No package, just deletion
    // HO^                    =====>   uHO ; ### ; dHO
    //
    // Explicit deletion locus plus expressoin
    // HO^ ; pA > gB           ====>   uHO ; pA ; gB ; ### ; dHO
    //
    // Explicit locus with two or more genes
    // HO^ ; pA > gB ; pC > gD ====>   uHO ; pA ; gB ; ### ; !gD ; !pA ; dHO
    //
    // Titrations:
    ////////////////////////////////////
    // Titration of native gene
    // pA>gB                   =====>  uB ; ### ; pA ; ~oB (DS_G_CDS in thumper parlance)
    //
    //
    // Titration of native gene with additional expression constructs
    //
    // pA>gB ; pc>gD          ======> uB ; pC ; gD ; ### ; pA ; gD[1:~500]

   
    /// Stitch or megastitch; if neither, default to megastitch, and megastitch stomps stitch
    let megastitch = match assemblyMode pragmas with | Megastitch -> true | Stitch -> false
    
    /// Which reference genome are we using
    let refGenome' =
        match pragmas.TryGetOne("refgenome") with
        // specifying a different reference genome implies a non standard
        // DNA source, so we can use that too (they can override with dnasrc)
        | Some(rg) -> rg.Trim([| ' ' ; '\t' |]) 
        | None -> constants.defaultRefGenome
       
    /// Parameters to pass to specific L2 algorithm implementation  
    let designParams = { megastitch = megastitch
                         rgs = rgs
                         refGenome = refGenome' 
                         line = line
                         pragmas = pragmas
                        }

    /// List including lower-level GSL strings
    match line.l2Locus with
    | Some(locusWithPrefix) ->      
        // Explicit locus case.  E.g.  gADH1^::pTDH3>mERG10
        // Choose provider
        let fn = providers |> 
                     List.choose (fun provider -> 
                                            match provider.jobScorer capabilities with
                                                | None -> None
                                                | Some(score) -> Some (score,provider.explicitLocusProvider)
                                    ) |>
                     List.sortWith(fun (a,_) (b,_) -> compare b a)  |> // sort largest first
                     List.head |>
                     snd

        fn locusWithPrefix designParams // returns string list
    | None -> 
        // Implicit locus case.  E.g.  gADH1^::pTDH3>mERG10
        // Choose provider
        let fn = providers |> 
                     List.choose (fun provider -> 
                                            match provider.jobScorer capabilities with
                                                | None -> None
                                                | Some(score) -> Some (score,provider.implicitLocusProvider)
                                    ) |>
                     List.sortWith(fun (a,_) (b,_) -> compare b a)  |> // largest first
                     List.head |>
                     snd

        fn designParams // returns string list

let validateNoAssemblyInL2Promoter (node: AstNode) = 
    match node with
    |L2Element(e) ->
        // if you see an L2 element, check if the promoter looks like an Assembly
        match e.x.promoter with
        | AssemblyPart(a) -> 
            error L2ExpansionError "Unsupported use of an Assembly." node
        | RecursivePart(_) -> error (InternalError L2ExpansionError) "Unexpected recursive part definition in L2 promoter position." node
        | _ -> good
    | _ -> good

/// Expand all level 2 expressions.
let expandLevel2
    legalCapas
    (providers: L2Provider list) 
    (rgs:GenomeDefs)
    tree =

    let bootstrapExpandL2Expression pragmaContext node =
        /// Perform the expansion operation, capturing any exception as an error.
        let expandCaptureException =
            expandL2Expression providers rgs
            |> captureException (exceptionToError L2ExpansionError node)
        match node with
        | L2Expression(l2e) ->
            convertL2Line pragmaContext l2e
            >>= expandCaptureException
            >>= (bootstrapPhase1 legalCapas l2e.positions)
        | _ -> ok node

    foldmap // run the bootstrapped expand operation
        Serial
        TopDown
        updatePragmaEnvironment
        emptyPragmaEnvironment
        bootstrapExpandL2Expression
        tree
    >>= healSplices // heal the splices
    >>= stuffPragmasIntoAssemblies // Bootstrapped assemblies need their pragma environment reinjected


// ==================================
// determining which bootstrapped expansion to run
// ==================================

type BootstrapExpasionMode =
    | ExpandMutation
    | ExpandProtein
    | ExpandHetBlock

let private expansionPriority mode =
    match mode with
    | ExpandHetBlock -> 5
    | ExpandProtein -> 10
    | ExpandMutation -> 20

let private prioritize mode1 mode2 =
    if expansionPriority mode1 > expansionPriority mode2 then mode1
    else mode2

/// Given a node, determine what expansion step it requires to continue.
let private expansionMode node =
    match node with
    | Mutation(_) -> Some(ExpandMutation)
    | InlineProtein(_) -> Some(ExpandProtein)
    | HetBlock(_) -> Some(ExpandHetBlock)
    | _ -> None

/// Given an AST, determine the highest priority of expansion needed to continue.
let expansionNeeded tree =
    let expansionsNeeded =
        tree
        |> traverse
        |> Seq.map expansionMode
        |> Seq.choose id
        |> Set.ofSeq
    if expansionsNeeded.IsEmpty then None
    else Some(expansionsNeeded |> Seq.reduce prioritize)

///<summary>
/// We only want to run the assemblies that require expansion through the bootstrapper.
/// Wrap a bootstrap operation in a check that passes the existing node through if it doesn't need
/// the current expansion step.
///</summary>
let maybeBypassBootstrap mode bootstrapOperation state (node: AstNode) =
    let modesRequiredByThisNode =
        AstTreeHead(node)
        |> traverse
        |> Seq.choose expansionMode
        |> Set.ofSeq
    
    if modesRequiredByThisNode.Contains(mode) then
        // this node needs to be bootstrapped, run it
        bootstrapOperation state node
    else ok node

// =========================
// expanding mutations
// =========================

let modIsMutation m =
    match m with
    | MUTATION(m) -> Some(m)
    | _ -> None

/// Remove mutation definitions and replace with a lower level representation.
/// Note that mutations can expand to more than a single line of GSL, so bootstrap these as a block.
let private expandMut 
    verbose 
    (providers : AlleleSwapProvider list)
    (rgs:GenomeDefs) 
    (codonProvider: ICodonProvider) 
    (assembly: Assembly) =
    /// Rewrite an individual part p in an assembly a.
    /// Assembly is passed in for context if needed
    let rewritePPP (a:Assembly) (p:PPP) =
        let aName =
            match a.name with
            | Some(n) -> n
            | None -> a.parts.Head.part.ToString()
            // FIXME: we should have already expanded names, so this shouldn't be necessary
            // Refactor Assembly to have an unconditional name.
        match p.part with
        | HETBLOCK -> p // don't expect this but just in case
        | INLINEDNA(_) -> p
        | INLINEPROT(_) -> p
        | MARKERPART -> p
        | SOURCE_CODE(_) -> p
        | PARTID(part) ->
            // Does it contain a mutation modification
            match part.mods |> List.choose modIsMutation with
            | [] -> p
            | [mutMod] ->
                let rg' = getRG a rgs p.pr
                let asAACheck =
                    match a.pragmas.TryFind("warnoff") with
                    | Some(p) -> not (p.hasVal "asaacheck")
                    | None -> true
                if verbose then printfn "***** %A" a.pragmas

                // Leave pragmas intact
                {p with
                    part = SOURCE_CODE(alleleSwaps.expandSimpleMut asAACheck rg' part mutMod)}
            | tooManyMuts -> failwithf "Internal error, found more than one mutation mod: %A" tooManyMuts

        | GENEPART(gp) ->
            // Does it contain a mutation modification?
            // Make sure we have only one if so.
            // TODO: this restriction may not be necessary
            match gp.mods |> List.choose modIsMutation with
            | [] -> p
            | [mutMod] ->
                if (not (gp.gene.[0] = 'G' || gp.gene.[0] = 'g')) then
                    failwithf
                        "Allele swap gene must be g-type  e.g gABC1$x1234y.  '%c' is not a legal prefix for %s"
                        (gp.gene.[0]) gp.gene
                let rg' = getRG a rgs p.pr

                // FIXME: unclear if this was the right behavior, as the rg is selected from both
                // assembly and part pragmas yet the actual ref genome selected here was only using
                // the assembly pragmas.  Uggh.
                //// Need to select a codon usage table
                //let refGenome = chooseRefGenome (a.pragmas)

                let codonUsage = codonProvider.GetCodonLookupTable(rg')
                
                let endPref =
                    match p.pr.TryGetOne("swapend") with
                    | Some("5") -> NTERM
                    | Some("3") -> CTERM
                    | Some(_) -> failwithf "#swapend argument should be 5 or 3"
                    | None -> NONETERM

                if verbose then
                    printf "Mutation endpreference: %s\n"
                        (match endPref with
                         | NTERM -> "Nterm"
                         | CTERM -> "Cterm"
                         | NONETERM -> "No end preference")

                if not (rg'.IsValid(gp.gene.[1..])) then
                    failwithf "Undefined gene '%s' %O\n"
                        (gp.gene.[1..]) (gp.where)

                let asAACheck =
                    match a.pragmas.TryFind("warnoff") with
                    | Some(p) -> not (p.hasVal "asaacheck")
                    | None -> true

                // Check if there is a style pragma on the part itself.  User can designate
                // short or long allele swap styles at the part level
                let longStyle =
                    match p.pr.TryGetOne("style") with
                    | None -> true // default is long style
                    | Some("long") -> true // long style
                    | Some("short") -> false // short style
                    | Some(x) ->
                        failwithf
                            "Undefined allele swap style '%s', options are long or short" x
                 
                let swapImpl =
                    alleleSwaps.expandAS
                        providers
                        asAACheck
                        aName
                        verbose
                        rg'
                        codonUsage
                        gp.gene
                        mutMod
                        endPref
                        a.capabilities
                        (p.pr.MergeIn(a.pragmas)) // combine the part and assembly pragmas to pass to the swap
                        longStyle

                {p with part = SOURCE_CODE(swapImpl) } // Leave pragmas intact
            | tooManyMuts -> failwithf "Internal error, found more than one mutation mod: %A" tooManyMuts

    // To perform an allele swap, we require an assembly with exactly one part.
    // Check this now and fail if we can't do it.
    let newParts =
        match assembly.parts with
        | [] -> [] // empty assembly; weird but no reason to blow up here.
        | [singlePart] -> // we can handle this case
            [rewritePPP assembly singlePart]
        | x ->
            failwithf "Tried to perform a mutation expansion on an assembly with more than one part: %A.  This is currently not supported." x
    prettyPrintAssembly {assembly with parts = newParts}
    // note that the assembly which contains a part which is expanded to literal GSL that actually may
    // amount to more than a single line.  Be careful.

/// Expand all mutations in an AST.
let expandMutations
    verbose 
    legalCapas
    (providers : AlleleSwapProvider list)
    (rgs:GenomeDefs)
    codonProvider
    (tree: AstTreeHead) =

    let assemblyExpansion = expandMut verbose providers rgs codonProvider

    let bootstrapOperation =
        bootstrapExpandLegacyAssembly
            MutationError
            assemblyExpansion
            (bootstrapPhase1 legalCapas)

    let expansionOnlyOnNodesWithMutations = maybeBypassBootstrap ExpandMutation bootstrapOperation

    executeBootstrap expansionOnlyOnNodesWithMutations Serial tree

// ====================
// expanding inline protein sequences
// ====================

/// Take inline protein sequences and expand them out to DNA sequences
let private expandProtein
        verbose
        (rgs:GenomeDefs)
        (unconfiguredCodonProvider: ICodonProvider)
        (assembly: Assembly) =

    let rewritePPP (codonProvider: ICodonProvider) (refGenome:string) (p:PPP) =
        match p.part with
        | INLINEPROT(s) ->
            // Check amino acid sequence is legal
            for c in s do
                if not (aaLegal.Contains(c)) then
                    failwithf
                        "Protein sequence contains illegal amino acid '%c'"
                        c

            let refGenome' =
                match p.pr.TryGetOne("refgenome") with
                | Some(rg) -> rg
                | None -> refGenome

            match rgs.TryFind refGenome' with
            | None ->
                failwithf
                    "Unable to load refgenome %s to determine environment"
                    refGenome'
            | Some(genomeDef) ->
                // Check to see if there is a local #seed parameter and extract it else
                // fall back on the version in the codon opt parameters globally
                let seedOverride =
                        match p.pr.TryGetOne("seed") with
                        | None -> None
                        | Some(seed) ->
                            match System.Int32.TryParse seed with
                            | true,s -> Some(s)
                            | _ ->
                                failwithf
                                    "#seed argument '%s' is not a valid integer"
                                    seed
                let codonOptTask =
                   {verbose = verbose;
                    seedOverride = seedOverride;
                    refGenome = genomeDef;
                    aminoAcidSequence = s}
                let result = codonProvider.DoCodonOpt codonOptTask
                {p with part = INLINEDNA(result)}
        | _ -> p

    let refGenome = chooseRefGenome (assembly.pragmas)
        
    let configuredCodonProvider = unconfiguredCodonProvider.Setup(assembly.pragmas)

    let expandedParts =
        assembly.parts
        |> List.map (rewritePPP configuredCodonProvider refGenome)

    {assembly with parts = expandedParts} |> prettyPrintAssembly

/// Expand all inline protein sequences in an AST.
let expandInlineProteins
    doParallel
    verbose
    legalCapas
    (rgs:GenomeDefs)
    codonProvider
    tree =
      
    let mode = if doParallel then Parallel else Serial

    let assemblyExpansion = expandProtein verbose rgs codonProvider

    let bootstrapOperation =
        bootstrapExpandLegacyAssembly
            ProteinError
            assemblyExpansion
            (bootstrapPhase1 legalCapas)

    let expansionOnlyOnNodesWithProteins = maybeBypassBootstrap ExpandProtein bootstrapOperation

    executeBootstrap expansionOnlyOnNodesWithProteins mode tree


// ==========================
// expanding heterology blocks
// ==========================


/// Expand heterology blocks.
let private expandHB
        verbose
        (rgs: GenomeDefs)
        (codonProvider: ICodonProvider)
        (assemblyIn: Assembly) =

    let modIsNotSlice m =
        match m with
        | SLICE(_) -> false
        | _ -> true

    let getLenPragma (pr:PragmaCollection) =
        match pr.TryGetOne("len") with
        | None -> None
        | Some(v) ->
            match Int32.TryParse(v) with
            | true,i -> Some(i)
            | _ -> failwithf "Expected integer in #len pragma, not '%s'" v

    let capUsing (a: Dna) (b: Dna) =
        Seq.zip a b
        |> Seq.map (fun (a,b) ->
            if a=b then b
            else
                match b with
                | 'G' -> 'g'
                | 'C' -> 'c'
                | 'A' -> 'a'
                | 'T' -> 't'
                | _ as x ->x)
        |> Dna

    let rec scan
            (a:Assembly)
            (res:(Part*PragmaCollection*bool) list)
            (p: (Part*PragmaCollection*bool) list)  =
        // Need to select a codon usage table
        let rg = rgs.[chooseRefGenome (a.pragmas)]
        let codonUsage = codonProvider.GetCodonLookupTable(rg)

        match p with
        // Lone heterology block with no inlined sequence adjacent
        | (GENEPART(gpUp),pr1,fwd1)
          ::(HETBLOCK,pr2,fwd2)
          ::(GENEPART(gp),pr3,fwd3)
          ::tl ->
            let rg' = getRG a rgs pr3
            let rg'' = getRG a rgs pr1

            let sliceSeq = realizeSequence verbose a.pragmas fwd3 rg' gp // Get DNA sequence for this slice
            let sliceSeqUp = realizeSequence verbose a.pragmas fwd1 rg'' gpUp // Get DNA sequence for upstream slice
            let targetAALen = getLenPragma pr2 // Get optional length spec for the HB

            // Generate an alternative prefix for the GENEPART on RHS
            let alt =
                generateRightHB
                    codonUsage
                    minHBCodonUsage
                    targetAALen
                    a.designParams
                    sliceSeqUp
                    (Dna(""))
                    sliceSeq
            // tricky part - need to slightly adjust the slice range of gp,
            // but that's embedded down in the mod list

            // Assume they must be using a gene part next to a het block.  Bad?
            if (not (gp.gene.StartsWith("g"))) then
                failwithf "Heterology block must be adjacent to g part, %s not allowed" gp.gene
            let s = translateGenePrefix a.pragmas rg' GENE // Start with standard slice
            let startSlice = applySlices verbose gp.mods s // Apply modifiers
            let newSlice =
                {startSlice with
                    left =
                        {startSlice.left with
                            x = startSlice.left.x + (alt.Length*1<OneOffset>)}} // Chop it

            assert(alt <> sliceSeq.[0..alt.Length-1])
            // Assemble new mod list by getting rid of existing slice mods and
            // putting in new consolidated slice.
            let newMods =
                SLICE(newSlice)
                ::(gp.mods |> List.filter modIsNotSlice)

            // Prepend backwards as we will flip list at end - watch out, pragmas are reversed as well
            scan
                a
                ((GENEPART({gp with mods = newMods}),pr3,fwd3)
                 ::(INLINEDNA(alt), returnOrFail (pr2.Add("inline")), fwd2)
                 ::(GENEPART(gpUp),pr1,fwd1)
                 ::res)
                tl

        | (GENEPART(gpUp),pr1,fwd1)
          ::(INLINEDNA(i),pr2,fwd2)
          ::(HETBLOCK,pr3,_(*fwd3*))
          ::(GENEPART(gp),pr4,fwd4)
          ::tl ->
            let rg' = getRG a rgs pr4
            let rg'' = getRG a rgs pr1

            // Get DNA sequence for this slice
            let sliceSeq = realizeSequence verbose a.pragmas fwd4 rg' gp
            // Get DNA sequence for upstream slice
            let sliceSeqUp = realizeSequence verbose a.pragmas fwd1 rg'' gpUp
            // Get optional length spec for the HB
            let targetAALen = getLenPragma pr3
            // Generate an alternative prefix for the GENEPART on RHS
            let alt =
                generateRightHB
                    codonUsage minHBCodonUsage targetAALen a.designParams sliceSeqUp i sliceSeq

            assert(alt <> sliceSeq.[0..alt.Length-1])
            if verbose then
                printf "// %O -> %O\n"
                    alt sliceSeq.[0..alt.Length-1]

            // tricky part - need to slightly adjust the slice range of gp,
            // but that's embedded down in the mod list

            // Assume they must be using a gene part next to a het block.  Bad?
            if not (gp.gene.[0] = 'g') then
                failwithf
                    "Slices adjacent to het block elements ~ must be gene slices - %s has '%c' gene part"
                    gp.gene gp.gene.[0]

            let s = translateGenePrefix a.pragmas rg'' GENE // Start with standard slice
            let startSlice = applySlices verbose gp.mods s // Apply modifiers
            let newSlice =
                {startSlice with
                    left =
                        {startSlice.left with
                            x = startSlice.left.x + (alt.Length*1<OneOffset>)}} // Chop it

            let newInline = DnaOps.append i alt

            // Assemble new mod list by getting rid of existing slice mods and putting in new consolidated slice.
            let newMods =
                SLICE(newSlice)
                ::(gp.mods |> List.filter modIsNotSlice)
            // Prepend backwards as we will flip list at end
            // Note - currently destroy any pragmas attached to the heterology block itself
            scan
                a
                ((GENEPART({gp with mods = newMods}), pr4, fwd4)
                 ::(INLINEDNA(newInline), returnOrFail (pr2.Add("inline")),fwd2)
                 ::(GENEPART(gpUp),pr1,fwd1)
                 ::res)
                tl

        | (GENEPART(gp),pr1,fwd1)
          ::(HETBLOCK,pr2,_(*fwd2*))
          ::(INLINEDNA(ic),pr3,fwd3)
          ::(GENEPART(gpDown),pr4,fwd4)
          ::tl ->

            // get reference genomes warmed up
            let rg' = getRG a rgs pr1
            let rg'' = getRG a rgs pr4

            // get actual sequence for slices (with mods applied)
            let sliceSeq = realizeSequence verbose a.pragmas fwd1 rg' gp // Get DNA sequence for this slice
            let sliceSeqDown = realizeSequence verbose a.pragmas fwd4 rg'' gpDown // Get DNA sequence for this slice
            let targetAALen = getLenPragma pr2 // Get optional length spec for the HB

            // generate hetblock section off upstream slice
            // Generate an alternative prefix for the GENEPART on LHS
            let alt =
                generateLeftHB
                    codonUsage minHBCodonUsage targetAALen a.designParams sliceSeq ic sliceSeqDown
            // tricky part - need to slightly adjust the slice range of gp,
            // but that's embedded down in the mod list

            // Assume they must be using a gene part next to a het block.  Bad?
            assert(gp.gene.[0] = 'g')

            // now build up the slice again and apply from scratch to the gene
            let s = translateGenePrefix a.pragmas rg' GENE // Start with standard slice
            let startSlice = applySlices verbose gp.mods s // Apply modifiers

            // modify slice to take into account the bit we chopped off
            let newSlice =
                {startSlice with
                    right =
                        {startSlice.right with
                            x = startSlice.right.x - (alt.Length*1<OneOffset>)}} // Chop it
            let newInline = DnaOps.append alt ic
            assert(alt <> sliceSeq.[sliceSeq.Length-alt.Length..])
            if verbose then
                //let fr = alt
                let t = sliceSeq.[sliceSeq.Length-alt.Length..sliceSeq.Length-1]
                let sim = Array.map2 (fun a b -> if a = b then '.' else ' ') alt.arr t.arr
                let simProp =
                    (seq { for x in sim -> if x = '.' then 1.0 else 0.0} |> Seq.sum)
                  / float(alt.Length) * 100.0

                printf
                    "// From: %O \n// To  : %O\n//     : %s %3.0f%%\n"
                    alt (t |> capUsing alt) (arr2seq sim) simProp
            // Assemble new mod list by getting rid of existing slice mods and
            // putting in new consolidated slice.
            let newMods =
                SLICE(newSlice)
                ::(gp.mods |> List.filter modIsNotSlice)
            // Prepend backwards as we will flip list at end
            // Note - currently destroy pr2 pragmas associated with the hetblock
            scan
                a
                ((GENEPART(gpDown),pr4,fwd4)
                 ::(INLINEDNA(newInline), returnOrFail (pr3.Add("inline")),fwd3)
                 ::(GENEPART({gp with mods = newMods}), pr1, fwd1)
                 ::res)
                tl

        | (PARTID(pid1),pr1,fwd1)
          ::(HETBLOCK,pr2,_(*fwd2*))
          ::(INLINEDNA(ic),pr3,fwd3)
          ::(PARTID(pid4),pr4,fwd4)
          ::tl ->
            // External part variation
            // ===============================================

            match fetchPart pid1.id with
            | Bad msgs -> failwithf "Fail fetching %s %s" pid1.id (msgs |> String.concat " ")
            | Ok(part1, _) ->
                match fetchPart pid4.id with
                | Bad msgs -> failwithf "Fail fetching %s %s" pid1.id (msgs |> String.concat " ")
                | Ok(part4, _) ->
                    let s1= getExtPartSlice verbose pid1
                    let s4= getExtPartSlice verbose pid4

                    // Build up upstream and downstream DNA slice
                    let sliceSeq1 = applySliceToExtSequence verbose part1 pr1 fwd1 pid1 s1
                    let sliceSeq4 = applySliceToExtSequence verbose part4 pr4 fwd4 pid4 s4

                    let targetAALen = getLenPragma pr2 // Get optional length spec for the HB

                    // generate hetblock sequence by cutting into upstream sequence
                    // Generate an alternative prefix for the GENEPART on LHS
                    let alt =
                        generateLeftHB
                            codonUsage minHBCodonUsage targetAALen
                            a.designParams sliceSeq1.dna ic sliceSeq4.dna

                    // Build up the slice mods for the upstream part from scratch
                    // Modify upstream slice to account for the part we chopped off
                    let newSlice:Slice =
                        {s1 with
                            right =
                                {s1.right with x = s1.right.x - (alt.Length*1<OneOffset>)}} // Chop it
                    let newInline = DnaOps.append alt ic
                    assert(alt <> sliceSeq1.dna.[sliceSeq1.dna.Length-alt.Length..])
                    if verbose then
                        let t = sliceSeq1.dna.[sliceSeq1.dna.Length-alt.Length..]
                        let sim = Array.map2 (fun a b -> if a = b then '.' else ' ') alt.arr t.arr
                        let simProp =
                            (seq { for x in sim -> if x = '.' then 1.0 else 0.0} |> Seq.sum )
                          / float(alt.Length) * 100.0

                        printf
                            "// From: %O \n// To  : %O\n//     : %s %3.0f%%\n"
                            alt (t |> capUsing alt) (arr2seq sim) simProp
                    // Assemble new mod list by getting rid of existing slice mods
                    // and putting in new consolidated slice.
                    let newMods =
                        SLICE(newSlice)
                        ::(pid1.mods |> List.filter modIsNotSlice)
                    // Prepend backwards as we will flip list at end
                    // Note - currently destroy pr2 pragmas associated with the hetblock
                    scan
                        a
                        ((PARTID(pid4),pr4,fwd4)
                         ::(INLINEDNA(newInline), returnOrFail (pr3.Add("inline")),fwd3)
                         ::(PARTID({ pid1 with mods = newMods}),pr1,fwd1)
                         ::res)
                        tl
        | hd::tl -> scan a (hd::res) tl // do nothing
        | [] -> List.rev res
    /// Easier to process part/pragma pairs if they are explicit tuples
    let tuplePPP (ppp: PPP list) =
        ppp |> List.map (fun p -> p.part,p.pr,p.fwd)
    let untuplePPP ab =
        ab |> List.map (fun (a,b,c) -> { part = a ; pr = b ; fwd= c} )

    let newParts = scan assemblyIn [] (tuplePPP assemblyIn.parts) |> untuplePPP

    let res = {assemblyIn with parts = newParts}

    let remainingHetblocks =
        res.parts
        |> List.exists (fun ppp -> match ppp.part with | HETBLOCK -> true | _ -> false)
    let newSource = prettyPrintAssembly res
    if remainingHetblocks then
        failwithf
            "Attempt to expand heterology block in design %s left remaining hetblock"
            newSource.String
    else newSource

/// Expand all heterology blocks in an AST.
let expandHetBlocks
    verbose
    legalCapas
    (rgs:GenomeDefs)
    codonProvider
    tree =

    let assemblyExpansion = expandHB verbose rgs codonProvider

    let bootstrapOperation =
        bootstrapExpandLegacyAssembly
            HetBlockError
            assemblyExpansion
            (bootstrapPhase1 legalCapas)

    let expansionOnlyOnNodesWithHetBlocks = maybeBypassBootstrap ExpandHetBlock bootstrapOperation

    executeBootstrap expansionOnlyOnNodesWithHetBlocks Serial tree

   
// ===========================
// putting it all together: recursively expanding a post-phase-1 tree until we're done
// ===========================

/// Perform phase 2 compilation on a post-phase-1 tree.
/// This step is recursive and will execute until all expansions are
/// complete, or we hit the limit on recursion optionally set by maxPasses.
/// This ensures we catch misbehaving expansion steps in a clear fashion.
/// Well-behaved expansion should never create an expansion phase higher than itself,
/// so we should probably have a hard limit at N^2, where N is the number of nontrivial expansion
/// passes.
// FIXME: way too many arguments
let phase2 oneShot maxPasses doParallel verbose legalCapas asProviders rgs codonTableCache treeIn =

    let runPhase2 mode tree =
        match mode with
        | ExpandMutation ->
            expandMutations verbose legalCapas asProviders rgs codonTableCache tree
        | ExpandProtein ->
            expandInlineProteins doParallel verbose legalCapas rgs codonTableCache tree
        | ExpandHetBlock ->
            expandHetBlocks verbose legalCapas rgs codonTableCache tree
           
    let rec doPhase2 passNumber (tree: AstTreeHead) =
        match maxPasses with
        | Some(limit) when passNumber > limit -> // if we're past a limit passed in, fail.
            errorf (InternalError(Error)) "Compiler phase 2 hit recursion limit of %d." limit tree.wrappedNode
        | _ -> // otherwise, run the expansion step
            match expansionNeeded tree with
            | Some(mode) ->
                runPhase2 mode tree
                >>= doPhase2 (passNumber+1)
            | None -> ok tree

    // if we just want to expand one step and re-emit literal source code
    if oneShot then
        match expansionNeeded treeIn with
        | Some(mode) -> runPhase2 mode treeIn
        | None -> ok treeIn
    else doPhase2 0 treeIn


// ===============================
// gathering every assembly in the tree for output generation
// ===============================

// because we need the pragma environment to perform assembly conversion, we need to be sneaky here
// we will use a closure that holds onto a mutable data structure by reference, and adds assemblies
// to it as it converts them during tree processing.

let private convertAndGatherAssembly (accum: ResizeArray<Assembly>) conversionContext node =
    match node with
    | AssemblyPart(pieces) ->
        convertAssembly conversionContext pieces
        >>= (fun convertedAssembly ->
                accum.Add(convertedAssembly)
                ok node) // pass the node through unchanged
    | _ -> ok node

/// Convert all assembly parts to legacy Assemblies, and gather them in a mutable accumulator.
/// Return the accumulation if all conversions were successful.
let convertAndGatherAssemblies tree =
    let accum = new ResizeArray<Assembly>()
    foldmap
        Serial
        TopDown
        updateConversionContext
        emptyConversionContext
        (convertAndGatherAssembly accum)
        tree
    >>= (fun treeOut ->
        // if successful, return the accumulated assemblies and the tree itself
        ok ((accum |> List.ofSeq), treeOut))

