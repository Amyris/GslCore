/// Top-level compiler operations.
module gslcProcess

open System
open System.IO
open AstExpansion
open AstTypes
open LegacyParseTypes
open commonTypes
open DnaCreation
open PrimerCreation
open Amyris.Bio
open utils
open ProcessCmdLineArgs
open PluginTypes
open LexAndParse
open Amyris.ErrorHandling

/// Run GSLC on string input.
let rec processGSL (s: ConfigurationState) gslText =

    let opts, plugins, ga = s.opts, s.plugins, s.ga

    let verbose = opts.verbose

    /// Build up all legal capabilities by going through plugins
    // FIXME: need to inject this and validate legal capabilities
    // Should also eliminate global pragma state while we're at it, if possible.
    let legalCapas = plugins |>
                     List.map (fun pi->pi.providesCapas) |>
                     List.concat |>
                     set

    let alleleSwapAlgs = plugins |> getAllProviders getAlleleSwapAAProviders

    let l2Providers = plugins |> getAllProviders getL2KOTitrationProviders

    let phase2WithData =
        phase2 (not opts.iter) (Some(10)) opts.doParallel verbose legalCapas alleleSwapAlgs ga.rgs ga.codonProvider

    /// Main compiler pipeline.
    let phase1Result =
        lexAndParse verbose gslText
        >>= phase1 legalCapas

    if opts.onlyPhase1 then
        phase1Result
        >>= convertAndGatherAssemblies
    else
        phase1Result
        //>>= failOnAssemblyInL2Promoter
        >>= expandLevel2 legalCapas l2Providers ga.rgs
        >>= prepPhase2 ga.rgs ga.seqLibrary
        >>= phase2WithData
        >>= convertAndGatherAssemblies // collect the assemblies in the tree and return them

/// Convert all assemblies to DnaAssemblies.
let materializeDna (s:ConfigurationState) (assem:seq<Assembly>) =
    let opts, library, rgs = s.opts, s.ga.seqLibrary, s.ga.rgs

    let markerProviders = s.plugins |> getAllProviders getMarkerProviders

    if opts.verbose then printf "Processing %d assemblies\n" (Seq.length assem)

    assem
    |> Seq.mapi (fun i a ->
        try
            expandAssembly opts.verbose markerProviders rgs library i a
            |> ok
        with e ->
            fail(exceptionToAssemblyMessage a e))
    |> collect
    >>= (fun assemblies ->

        if opts.verbose then
            printf "log: dnaParts dump\n"
            for a in assemblies do
                printf "log: dnaPart: %s\n" a.name
                for p in a.dnaParts do
                    printf "log:      %s\n" p.description
                    printf "%s\n" (format60 p.dna.arr)

        // Check for reused pieces and number them accordingly
        // Make a list of all parts, determine unique set and assign ids
        let partIDs =
            seq {for a in assemblies do
                    for p in a.dnaParts do
                        yield p.dna} // Base this on DNA for now, but could involve linkers down the road}
            |> Set.ofSeq
            |> Seq.mapi (fun i dna -> (dna,i))
            |> Map.ofSeq // TODO - could be faster with checksums
        // Relabel the pieces with IDs  - tedious, we have to reconstruct the tree
        List.map
            (fun a ->
                {a with dnaParts = List.map
                    (fun (p:DNASlice) -> { p with id = Some(partIDs.[p.dna]) })
                    a.dnaParts})
            assemblies
        |> ok
    )


/// Promote long slices to regular rabits to avoid trying to build
/// impossibly long things with oligos.
let cleanLongSlicesInPartsList (p:pragmaTypes.PragmaCollection) (l:DNASlice list) =
    l |> List.map (fun s ->
        if (s.sliceType = INLINEST &&
            (s.pragmas.ContainsKey("amp") || s.dna.Length > 30) &&
            not (s.pragmas.ContainsKey("inline")))
        then
            {s with
                sliceType = REGULAR;
                dnaSource =
                    match s.pragmas.TryGetOne("dnasrc") with
                    | Some(x) -> x
                    | None ->
                        match p.TryGetOne("refgenome") with
                        | None -> "synthetic"
                        | Some(x) -> x
                // add in an amp tag on this guy too, since we are now comitting to
                // not placing it inline using primers
                pragmas = match s.pragmas.TryFind("amp") with
                            | Some _ -> s.pragmas // already there
                            | None ->
                                match s.pragmas.Add("amp") with
                                | Result.Ok(result,_) -> result
                                | Bad messages ->
                                    // has to be a cleaner way of converting result to
                                    // exn if necessary
                                    failwithf "%s" (String.Join(";",messages))
            }
        else s)

/// Promote long slices to regular rabits to avoid trying to build
/// impossibly long things with oligos.
let cleanLongSlices _ (a:DnaAssembly) =
    ok {a with dnaParts = cleanLongSlicesInPartsList a.pragmas a.dnaParts}
    
/// Demote short slices to inlinest to build in oligos
/// rather than PCR 
let cleanShortSlicesInPartsList (p:pragmaTypes.PragmaCollection) (l:DNASlice list) =
    l |> List.map (fun s ->
        if (s.sliceType = REGULAR &&
            (s.pragmas.ContainsKey("inline") || (s.dna.Length <= 30)) &&
            not (s.pragmas.ContainsKey("amp")))
        then
            {s with
                sliceType = INLINEST;
                dnaSource =
                    match s.pragmas.TryGetOne("dnasrc") with
                    | Some(x) -> x
                    | None ->
                        match p.TryGetOne("refgenome") with
                        | None -> "synthetic"
                        | Some(x) -> x
                // add in an amp tag on this guy too, since we are now comitting to
                // not placing it inline using primers
                pragmas = match s.pragmas.TryFind("inline") with
                            | Some _ -> s.pragmas // already there
                            | None ->
                                match s.pragmas.Add("inline") with
                                | Result.Ok(result,_) -> result
                                | Bad messages ->
                                    // has to be a cleaner way of converting result to
                                    // exn if necessary
                                    failwithf "%s" (String.Join(";",messages))
            }
        else s)

/// Promote long slices to regular rabits to avoid trying to build
/// impossibly long things with oligos.
let cleanShortSlices _ (a:DnaAssembly) =
    ok {a with dnaParts = cleanShortSlicesInPartsList a.pragmas a.dnaParts}


/// we run into trouble during primer generation if a virtual part (fuse) gets between two parts that
/// would otherwise get fused anyway (a dna slice and a linker for example).   Strip out the fuse diective
/// in this case, otherwise primer doesn't get built against the real target
let preProcessFuse _ (a: DnaAssembly) =
    let rec proc (l:DNASlice list) res =
        match l with
            | [] -> List.rev res
            | hd::middle::tl when
                    hd.sliceType = SliceType.FUSIONST
                    && middle.sliceType = SliceType.INLINEST ->
                proc tl (middle::res)
            | hd::tl ->
                proc tl (hd::res)

    ok {a with dnaParts = (proc a.dnaParts [])}

/// Once GSL is expanded as far as possible,
/// go into more target-specific activities like assigning parts, reusing parts, etc.
let transformAssemblies (s: ConfigurationState) (assemblies:DnaAssembly list) =

    let atContext: ATContext = {opts = s.opts; ga = s.ga}

    let builtinAssemblyTransforms =
        [cleanLongSlices;
         cleanShortSlices;
         preProcessFuse;]

    let assemblyTransformers =
        builtinAssemblyTransforms@(getAllProviders getAssemblyTransformers s.plugins)

    // do all the assembly transformation steps
    // the transformations are done in the order in which plugins were passed in, and in order
    // inside each plugin if it provides multiple transformers.
    // TODO: should force transformers to provide a description, then provide command line args for
    // listing all available transformations and showing which transformations will run given the
    // provided args.
    /// use all assembly transformers to transform an assembly
    let transformAssembly a =
        assemblyTransformers
        |> List.fold
            (fun r transformer ->
                r >>= (transformer atContext))
            (ok a)

    /// Attempt to transform all of the assemblies
    assemblies
    |> List.map transformAssembly
    |> collect

let doPrimerDesign opts assemblyOuts =
    if opts.noPrimers then
        None, assemblyOuts
    else
        let p, t = designPrimers opts assemblyOuts
        Some(p), t



let doOutputGeneration (s: ConfigurationState) primers assemblies =
    let outputData = {ga = s.ga; opts = s.opts; assemblies = assemblies; primers = primers}
    if outputData.opts.verbose then printfn "ok"

    // Use any output providers provided by plugins
    // They have already been configured to run or not during command line arg parsing.
    for op in getAllProviders getOutputProviders s.plugins do
        op.ProduceOutput(outputData)

