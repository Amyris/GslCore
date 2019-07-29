module DnaCreation
open System
open Amyris.Bio.biolib
open constants
open LegacyParseTypes
open RefGenome
open pragmaTypes
open commonTypes
open applySlices
open Amyris.Bio
open Amyris.ErrorHandling
open Amyris.Dna
open ryse
open PluginTypes

// ================================================================================================
// Slice manipulation routines for getting from gene notation down to specific genomics coordinates
// ================================================================================================
let validateMods errorRef (where: AstTypes.SourcePosition list) (mods: Mod list) =
    for m in mods do
        match m with
        | SLICE(s) ->
            if s.left.relTo = s.right.relTo && s.left.x > s.right.x then
                // TODO: better to report coordinates of slice text rather than gene
                failwithf "slice left %A greater than right %A %O in %s"
                    s.left.x s.right.x (AstTypes.formatSourcePositionList where) errorRef
        | _ -> () // No checks for now TODO

/// Given a feature and a relative position, find the zero based physical
/// coordinate for the position.
/// Determine range of DNA needed, translating into physical coordinates
/// Final start of the piece.  Determine which end we are working relative to
let adjustToPhysical (feat:sgd.Feature) (f:RelPos) =
    let featOffset =
        match f.relTo, feat.fwd with
        | FivePrime, true | ThreePrime, false -> feat.l
        | ThreePrime, true | FivePrime, false -> feat.r

    let geneRelativeOffset =
        match f.relTo, f.x/1<OneOffset> with
        | FivePrime, o when o > 0 ->    o - 1
        | FivePrime, o ->               o
        | ThreePrime, o when o > 0 ->   o
        | ThreePrime, o ->              o + 1
        * (if feat.fwd then 1 else -1)

    (featOffset + geneRelativeOffset) * 1<ZeroOffset>

/// Generate logical coordinates for the start and end of the gene part
/// these are relative to the gene, not the genome for now.  We transform
/// to genomic coordinates below.
/// Transform all non gXXX forms of gene into gXX forms.
let translateGenePrefix (pragmas:PragmaCollection) (gd : GenomeDef) (gPart : StandardSlice) =
    match gPart with
    | PROMOTER ->
        let leftPos =
            match pragmas.TryGetOne "promlen" with
            | None -> -gd.getPromLen() 
            | Some p -> (int p) * -1<OneOffset> 
        {left = {x = leftPos; relTo = FivePrime};
         lApprox = true;
         rApprox = false;
         right = {x = -1<OneOffset>; relTo = FivePrime}}
    | UPSTREAM ->
       {left = {x = -gd.getFlank(); relTo = FivePrime};
        lApprox = true;
        rApprox = false;
        right = {x = -1<OneOffset>; relTo = FivePrime}}
    | TERMINATOR ->
        let rightPos =
            match pragmas.TryGetOne "termlen" with
            | None -> gd.getTermLen() 
            | Some p -> (int p) * 1<OneOffset> 
        {left = {x = 1<OneOffset>; relTo = ThreePrime};
         lApprox = false;
         rApprox = true;
         right = {x = rightPos; relTo = ThreePrime}}
    | DOWNSTREAM ->
        {left = {x = 1<OneOffset>; relTo = ThreePrime};
         lApprox = false;
         rApprox = true;
         right = {x = gd.getFlank(); relTo = ThreePrime}}
    | FUSABLEORF ->
        {left = {x = 1<OneOffset>; relTo = FivePrime};
         lApprox = false;
         rApprox = false;
         right = {x = -4<OneOffset>; relTo = ThreePrime}}
    | ORF ->
        {left = {x = 1<OneOffset>; relTo = FivePrime};
         lApprox = false;
         rApprox = false;
         right = {x = -1<OneOffset>; relTo = ThreePrime}}
    | GENE ->
        {left = {x = 1<OneOffset>; relTo = FivePrime};
         lApprox = false;
         rApprox = false;
         right = {x = -1<OneOffset>; relTo = ThreePrime}}
    | MRNA ->
        let rightPos =
            match pragmas.TryGetOne "termlenmrna"  with
            | None -> gd.getTermLenMRNA() 
            | Some p -> (int p) * 1<OneOffset>
        {left = {x = 1<OneOffset>; relTo = FivePrime};
         lApprox = false;
         rApprox = true;
         right = {x = rightPos; relTo = ThreePrime}}

/// Translate gene part label.  Raises an exception for errors.
let lookupGenePart errorDescription prefix (modList:Mod list) =
    let sliceType =
        match charToSliceType prefix with
        | Some(t) -> t
        | None ->
            failwithf
                "unknown gene prefix '%c' should be one of %s in %s"
                prefix (sliceTypeChars.ToString()) errorDescription

    let dotModList =
        seq {
            for m in modList do
                match m with
                | DOTMOD(dm) -> yield dm
                | _ -> ()}
        |> List.ofSeq

    match dotModList with
    | [] -> sliceType
    | [dm] ->
        // if it's a dot mod, it must be on a 'g' part (gYNG2) (For now...)
        if sliceType <> GENE then
            failwithf
                "cannot apply DOTMODS to non-gene parts (must be gXYZ). Used %c"
                prefix

        match dm with
        | "up" -> UPSTREAM
        | "down" -> DOWNSTREAM
        | "mrna" -> MRNA
        | x ->
            failwithf
                "unimplemented DOTMOD %s. Options: up, down, mrna"
                x
    | _ -> failwithf "multiple DOTMODS applied to %s" errorDescription


/// Get the reference genome for a given assembly and set of pragmas.
/// Exception on error.
let getRG (a:Assembly) (rgs:GenomeDefs) (pr:PragmaCollection) =
    // Prefer reference genome from passed-in pragmas over assembly.
    let prags = [pr; a.pragmas]
    match getRGNew rgs prags with
    | Ok(g, _) -> g
    | Bad(msgs) -> failwith msgs.[0]

/// Take a genepart and slices and get the actual DNA sequence.
let realizeSequence verbose  (pragmas:PragmaCollection) fwd (rg:GenomeDef) (gp:GenePart) =

    if verbose then
        printf "realizeSequence:  fetch fwd=%s %s\n"
            (if fwd then "y" else "n") gp.gene

    // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
    // Description of part to give in case of error
    let errorDesc = gp.gene
    let genePart = lookupGenePart errorDesc (gp.gene.[0]) (gp.mods)

    // Lookup gene location
    let feat = rg.get(gp.gene.[1..])

    // Come up with an initial slice based on the gene prefix type
    let s = translateGenePrefix pragmas rg genePart

    let finalSlice = applySlices verbose gp.mods s
    let left = adjustToPhysical feat finalSlice.left
    let right = adjustToPhysical feat finalSlice.right

    // One final adjustment needed.  We have defined left and right relative to
    // the gene, but if the gene is on the crick strand, we need to both flip
    // the coordinates and reverse complement the resulting DNA to keep it
    // oriented with the construction orientation.
    if (left > right && feat.fwd) || (right>left && (not feat.fwd)) then
        failwithf
            "[realizeSequence] slice results in negatively lengthed DNA piece gene=%s slice=%s\n"
            feat.gene (printSlice finalSlice)

    let left', right' = if feat.fwd then left, right else right, left
    rg.Dna(errorDesc, sprintf "%d" feat.chr, left', right')
        |> DnaOps.revCompIf (not feat.fwd)
        |> DnaOps.revCompIf (not fwd)


/// Extract slice name from a PPP, if it has one.
let getSliceName (ppp:PPP) =
    match ppp.pr.TryGetOne("name") with
    | Some(name) -> name
    | None -> ""

/// Extract URI from a PPP, if it has one.
let getUri (ppp:PPP) = ppp.pr.TryGetOne("uri")

/// Expand a marker part into DNA pieces.
/// Exception on failure.

let expandInlineDna
    dnaSource
    (ppp:PPP)
    (dnaFwd: Dna) =

    let dna = dnaFwd |> DnaOps.revCompIf (not ppp.fwd)

    {id = None;
     extId = None;
     sliceName = getSliceName ppp;
     uri = getUri ppp;
     dna = dna;
     sourceChr = "inline";
     sourceFr = 0<ZeroOffset>;
     sourceTo = (dna.Length-1)*1<ZeroOffset>;
     sourceFwd = true;
     sourceFrApprox = false;
     sourceToApprox = false;
     // NB - for now allow that this might be amplified, but could change later
     template = Some dna;
     amplified = false;
     // Don't assign coordinates to pieces until later when we decide how they are getting joined up
     destFr = 0<ZeroOffset>;
     destTo = 0<ZeroOffset>;
     destFwd = ppp.fwd;
     description = (if ppp.fwd then dnaFwd.str else "!" + dnaFwd.str);
     sliceType = INLINEST;
     dnaSource = dnaSource;
     pragmas = ppp.pr;
     breed = B_INLINE;
     materializedFrom = Some(ppp);
     annotations = []}

let expandGenePart
    verbose
    (rgs:GenomeDefs)
    (library: SequenceLibrary)
    (a:Assembly)
    specifiedDnaSource
    (ppp:PPP)
    (gp:GenePart) =

    // If the dna source is empty, then we are going to pull the DNA
    // part from the default reference genome, so we should make the
    // dnaSource field reflect this
    let dnaSource = 
        if specifiedDnaSource = "" then defaultRefGenome
        else specifiedDnaSource

    // Check the genes are legal
    //let prefix = gp.part.gene.[0]
    let g = gp.gene.[1..].ToUpper()
    let rg' = getRG a rgs ppp.pr

    if not (rg'.IsValid(g)) then
        // Not a genomic reference but might still be in our library
        if library.ContainsKey(g) then
            // Yes! =- make up a little island of sequence for it
            let dna = library.[g]

            // Need to adjust for any slicing carefully since the DNA island is small
            // Validate mods to gene
            let errorRef = match a.name with | None -> sprintf "%A" a | Some(x) -> x
            validateMods errorRef gp.where gp.mods

            // Come up with an initial slice based on the gene prefix type

            // Get standard slice range for a gene
            let s = translateGenePrefix a.pragmas rg' GENE
            let finalSlice = applySlices verbose gp.mods s

            // Ban approx slices to stay sane for now
            if finalSlice.lApprox || finalSlice.rApprox then
                failwithf
                    "sorry, approximate slices of library genes not supported yet in %A\n"
                    (prettyPrintAssembly a)

            let sliceContext = Library(gp.gene)

            let x, y =
                getBoundsFromSlice finalSlice dna.Length sliceContext
                |> returnOrFail

            let finalDNA =
                dna.[(x/1<OneOffset>)-1..(y/1<OneOffset>)-1]
                |> DnaOps.revCompIf (not ppp.fwd)

            let orfAnnotation = orfAnnotationFromSlice finalSlice finalDNA.Length ppp.fwd sliceContext

            let name1 =
                if gp.mods.Length = 0 then gp.gene
                else (gp.gene + (printSlice finalSlice))
            let name2 = if ppp.fwd then name1 else "!"+name1

            {id = None;
             extId = None;
             sliceName = getSliceName ppp;
             uri = getUri ppp; // TODO: should we also check a returned library part for a URI?
             dna = finalDNA;
             sourceChr = "library";
             sourceFr = (finalSlice.left.x/(1<OneOffset>)-1)*1<ZeroOffset>;
             sourceTo = (finalSlice.right.x/(1<OneOffset>)-1)*1<ZeroOffset>;
             sourceFwd = true;
             sourceFrApprox = false;
             sourceToApprox = false;
             amplified = false;
             // This is what we are expecting to amplify from (library part)
             template = Some finalDNA;
             // Don't assign coordinates to pieces until later when we decide how they are getting joined up
             destFr = 0<ZeroOffset>;
             destTo = 0<ZeroOffset>;
             destFwd = ppp.fwd;
             description = name2;
             sliceType = REGULAR;
             dnaSource = dnaSource;
             pragmas = ppp.pr;
             breed = B_X;
             materializedFrom = Some(ppp);
             annotations = [Orf(orfAnnotation)]}
        else // no :( - wasn't in genome or library
            failwithf "undefined gene '%s' %O\n" g gp.where
    else
        // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
        let errorDesc = gp.gene
        let genePart = lookupGenePart errorDesc gp.gene.[0] gp.mods
        // Lookup gene location
        let rg' = getRG a rgs ppp.pr

        let feat = rg'.get(g)

        let breed1 =
            match genePart with
            | PROMOTER -> B_PROMOTER
            | TERMINATOR -> B_TERMINATOR
            | MRNA -> B_GST
            | DOWNSTREAM -> B_DOWNSTREAM
            | UPSTREAM -> B_UPSTREAM
            | FUSABLEORF -> B_FUSABLEORF
            | GENE -> B_X
            | ORF -> B_GS

        // WARNING - very similar logic in realizeSequence and both versions
        // should be considered when changing logic.
        // FIXME: this common logic should be refactored into a generic function
        // and called in both places.

        // Validate mods to gene
        let errorRef = match a.name with | None -> sprintf "%A" a | Some(x) -> x

        validateMods errorRef gp.where gp.mods
        // Come up with an initial slice based on the gene prefix type
        let s = translateGenePrefix a.pragmas rg' genePart
        if verbose then printf "log: processing %A\n" a

        // finalSlice is the consolidated gene relative coordinate of desired piece
        let finalSlice = applySlices verbose gp.mods s

        // Gene relative coordinates for the gene slice we want
        let finalSliceWithApprox =
            // Calculate some adjusted boundaries in case the left/right edges are approximate
            let leftAdj =
                {finalSlice.left with x = finalSlice.left.x-(approxMargin * 1<OneOffset>)}
            let rightAdj =
                {finalSlice.right with x = finalSlice.right.x+(approxMargin * 1<OneOffset>)}

            {lApprox = finalSlice.lApprox;
             left = (if finalSlice.lApprox then leftAdj else finalSlice.left);
             rApprox = finalSlice.rApprox;
             right = if finalSlice.rApprox then rightAdj else finalSlice.right}

        if verbose then
            printf "log: finalSlice: %s%s %s%s\n"
                (if finalSlice.lApprox then "~" else "")
                (printRP finalSlice.left)
                (if finalSlice.rApprox then "~" else "")
                (printRP finalSlice.right)

            printf "log: finalSliceWA: %s%s %s%s\n"
                (if finalSliceWithApprox.lApprox then "~" else "")
                (printRP finalSliceWithApprox.left)
                (if finalSliceWithApprox.rApprox then "~" else "")
                (printRP finalSliceWithApprox.right)

        // FinalSliceWithApprox is a gene relative coordinate system, but we
        // need genomic coordinates for the gene

        // Left is the genomic left end of the element
        let left = adjustToPhysical feat finalSliceWithApprox.left
        // Right is the genomic right end of the element
        let right = adjustToPhysical feat finalSliceWithApprox.right

        if verbose then
            printf "log: gene: %s %d %d %s\n"
                feat.gene feat.l feat.r (if feat.fwd then "fwd" else "rev")
            printf "log: prefinal: %s %A %A\n" feat.gene left right

        // One final adjustment needed.  We have defined left and right relative
        // to the gene, but if the gene is on the crick strand, we need to both
        // flip the coordinates and reverse complement the resulting DNA to keep
        // it oriented with the construction orientation.

        if (left > right && feat.fwd) || (right>left && (not feat.fwd)) then
            failwithf
                "slice results in negatively lengthed DNA piece for %s\n"
                (gp.gene + (printSlice finalSlice))

        /// left' is the genomic coordinate of the start of the element (i.e gene upstream)
        let left', right' = if feat.fwd then left, right else right, left
        if verbose then printf "log: final: %s %A %A\n" feat.gene left' right'

        // TOO BLUNT gp.part.gene = "gURA3"
        // TODO: hard coded detection of split marker
        let isMarker = false
        let rg' = getRG a rgs ppp.pr

        if verbose then
            printf "gettingdna for %s fwd=%s\n"
                feat.gene (if ppp.fwd then "y" else "n")

        let dna =
            rg'.Dna(errorDesc,sprintf "%d" feat.chr,left',right')
            |> DnaOps.revCompIf (not feat.fwd)
            // One potential final flip if user wants DNA backwards
            |> DnaOps.revCompIf (not ppp.fwd)

        let description1 =
            match gp.mods with
            | [] -> gp.gene
            | [DOTMOD(d)] ->
                match d with
                | "up" -> "u" + gp.gene.[1..]
                | "down" -> "d" + gp.gene.[1..]
                | "mrna" -> "m" + gp.gene.[1..]
                | x -> failwithf "unimplemented DOTMOD %s" x
            | _ -> "g" + gp.gene.[1..] + (printSlice finalSlice)
        let description2 = if ppp.fwd then description1 else "!"+description1

        let promStart = {x = -300<OneOffset>; relTo = FivePrime}
        let promEnd = {x = -1<OneOffset>; relTo = FivePrime}
        let termStart = {x = 1<OneOffset>; relTo = ThreePrime}
        let termEnd = {x = 150<OneOffset>; relTo = ThreePrime}

        let near (a:RelPos) (b:RelPos) (tolerance) =
            a.relTo = b.relTo && abs((a.x-b.x)/1<OneOffset>)< tolerance

        let breed =
            match breed1 with
            | B_X ->
                let z = finalSliceWithApprox
                if near z.left termStart 1 && near z.right termEnd 100 then
                    B_TERMINATOR
                elif near z.left promStart 400 && near z.right promEnd 40 then
                    B_PROMOTER
                elif z.left.x=1<OneOffset> &&
                     z.left.relTo=FivePrime &&
                     near z.right termEnd 100
                    then B_GST
                else B_X
            | x -> x

        let orfAnnotation = orfAnnotationFromSlice finalSlice feat.Length ppp.fwd Genomic

        // Note regarding orientation: We are currently building a single piece
        // of final DNA left to right. There is no consideration for stitch
        // orientation, so even (in RYSEworld) B stitch parts are laid out left
        // to right from marker (innermost 9 linker) through to the outler linker.
        // If something is reversed then, it points towards the middle, marker
        // part of the stitch.
        {id = None;
         extId = None;
         sliceName = getSliceName ppp;
         uri = getUri ppp;
         dna = dna;
         sourceChr = feat.chr |> string;
         sourceFr = (if ppp.fwd then left' else right'); // 5' end of gene element in the genome
         sourceTo = (if ppp.fwd then right' else left'); // 3' end of gene element in the genome
         sourceFwd = feat.fwd;
         amplified = true;
         template = Some dna; // This is what we are expecting to amplify from genome
         // Don't assign coordinates to pieces until later when we decide how
         // they are getting joined up. Left and Right are absolute genomic
         // coordinate relative (i.e left has a smaller coordinate than left)

         // This logic is a bit twisted. SourceFrApprox is misleading, this
         // designates whether the *left* end is approx or now and that operation
         // occurs before the orientation of the rabit is applied, so
         // !gABC1[100:~200E] has a sourceFrApprox = false initially.  We flipped
         // the actual piece of DNA so the l and r approx need to move with the DNA
         sourceFrApprox = (if ppp.fwd then finalSlice.lApprox else finalSlice.rApprox);
         sourceToApprox = (if ppp.fwd then finalSlice.rApprox else finalSlice.lApprox);
         destFr = 0<ZeroOffset>;
         destTo = 0<ZeroOffset>;
         destFwd = ppp.fwd;
         description = description2;
         dnaSource = dnaSource;
         sliceType = (if isMarker then MARKER else REGULAR);
         pragmas = ppp.pr;
         breed = breed;
         materializedFrom = Some(ppp);
         annotations = [Orf(orfAnnotation)]}

/// Choose the most appropriate marker provider based on capabilities and availability.
let private chooseMarkerProvider capabilities (providers: IMarkerProvider list) markerSet =
    /// Get all providers that are in play based on capabilities and sort by score.
    let sortedProviders =
        providers
        |> Seq.choose (fun provider ->
            match provider.ScoreJob(capabilities) with
            | None -> None
            | Some(score) -> Some (provider, score))
        |> Seq.sortByDescending snd
        |> Seq.map fst
        |> List.ofSeq

    /// Filter down to just the providers that can handle the specified marker set.
    let legalProviders = sortedProviders |> List.filter (fun provider -> provider.IsLegal(markerSet))

    match List.tryHead legalProviders with
    | Some(provider) -> ok provider
    | None when sortedProviders.IsEmpty ->
        fail (sprintf "No marker providers available for capas %O." capabilities)
    | None ->
        sortedProviders
        |> List.map (fun provider -> provider.ListMarkers())
        |> List.concat
        |> String.concat ", "
        |> sprintf "Unknown marker set %s, expected one of %s" markerSet
        |> fail

let private determineTopology (pragmas : PragmaCollection) : Topology =
    match pragmas.TryFind(Topology.PragmaName) with
    | Some(pragma) -> pragma.args |> Topology.parse |> returnOrFail
    | None -> Linear

/// Take a parsed assembly definition and translate it
/// to underlying DNA pieces, checking the structure in the process.
/// Raises an exception on error.
let expandAssembly
    verbose
    (markerProviders:IMarkerProvider list)
    (rgs:GenomeDefs)
    (library: SequenceLibrary)
    index
    (a:Assembly) =

    let rec expandPPPList pppList =
        seq {
            // NOTE: have access to part.pragmas to the extent they influence generation
            for ppp in pppList do
                let dnaSource =
                    match ppp.pr.TryGetOne("dnasrc") with
                    | Some(d) -> d
                    | None ->
                        // specifying a different reference genome implies a non standard
                        // DNA source, so we can use that too (they can override with dnasrc)
                        match ppp.pr.TryGetOne("refgenome") with
                        | Some (rg) -> rg
                        | None ->
                            match a.pragmas.TryGetOne("refgenome") with
                            | Some(rg) -> rg
                            | None -> "" // Revert to the current default part origin

                match ppp.part with
                | MARKERPART ->
                    let markerSet =
                        a.pragmas.TryGetOne("markerset")
                        |> Option.map (fun m -> m.ToLower())
                        |> Option.defaultValue "default"

                    let markerProvider =
                        chooseMarkerProvider a.capabilities markerProviders markerSet
                        |> returnOrFail

                    let task = { dnaSource = dnaSource ; ppp = ppp ; markerSet = markerSet}
                    yield markerProvider.CreateDna(task)

                | PARTID(partId) ->
                    yield resolveExtPart.fetchSequence verbose ppp partId
                | INLINEDNA(dna) ->
                    yield expandInlineDna dnaSource ppp dna
                | INLINEPROT(_) ->
                    failwith "unexpanded protein inline encountered during DNA generation"
                | HETBLOCK ->
                    failwith "unexpanded heterology block encountered during DNA generation"
                | SOURCE_CODE(_) -> ()
                | GENEPART(gp) ->
                    yield expandGenePart verbose rgs library a dnaSource ppp gp
                //
                // Might also want to yield a fusion slice
                //
                if ppp.pr.ContainsKey("fuse") then
                    yield fusionSliceConstant
            } |> List.ofSeq |> recalcOffset
    let materializedParts = expandPPPList a.parts
    let assemblyName = 
         match a.name with
         | None -> sprintf "A%d" index
         | Some(s) -> s
    
    let topology = a.pragmas |> determineTopology
    { id = Some(index)
      dnaParts = materializedParts
      name = assemblyName
      uri = a.uri
      linkerHint = a.linkerHint
      pragmas = a.pragmas
      designParams = a.designParams
      docStrings = a.docStrings
      materializedFrom = a
      topology = topology }
