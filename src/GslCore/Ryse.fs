module ryse
open System.IO
open System.Text.RegularExpressions
open FSharp.Data
open System
open pragmaTypes
open commonTypes
open Amyris.Bio.utils
open Amyris.Dna
open Amyris.ErrorHandling
//open System.Collections.Generic
open System.Collections.Concurrent
open constants
open uri
open sbolExample
open AstTypes
open PluginTypes
open FetchPart

// ==================================================================
// RYSE megastitch architecture
// ==================================================================

/// RYSE verbose flag
let private verbose = false

/// Load name\tsequence text file of RYSE linkers and return a map
let loadRyseLinkers (f:string) =
    if not (File.Exists f) then
        failwithf "could not locate reference file '%s'\n" f

    eachLineIn f
    |> Seq.choose (fun l ->
        match l.Split([|','|]) with
        | [|""|] -> None
        | [| n ; s |] -> Some (n, {name = n; dna = Dna(s)})
        | _ -> failwithf "Bad linker specification: '%s'" l)
    |> Map.ofSeq

let extractLinker (s:string ) =
    if s.StartsWith("Linker_") then s.[7..]
    else failwithf "ERROR: unable to parse linker name '%s'" s

module ThumperPartProvider =

    type ThumperPartProvider(lookupUrlBase: string option, useCache: bool) =
        let partLookupUrl route =
            match lookupUrlBase with
            | Some urlBase -> sprintf "%s/%s" urlBase route
            | None -> failwith "No global url provided for part lookup."
        let idRegex = Regex("^[Rr]\d+$")
        // CAUTION: this cache can become stale when GSLC is embedded in a long-running application.
        let fetchCache = new ConcurrentDictionary<string, ExtFetchSeq>()
        do
            ()
        

        with
        interface IPartProvider with
            member __.ProvidedArgs() = [] // TODO: move to Amyris private repo and use existing thumperUrl arg.
            member x.Configure(_) = x :> IPartProvider // TODO: use command line arg to set lookup URL.
            member x.ConfigureFromOptions(_) = x :> IPartProvider
            member x.Name = "thumper"
            member x.Accept(partId) = idRegex.IsMatch(partId)
            member x.Retrieve(partId) =
                let route = sprintf "rycod/rabit_spec/%s" partId.[1..]
                let url = partLookupUrl route

                let lookup () =
                    let response = Http.Request(url, silentHttpErrors = true)
                    match response.Body with
                    | Binary _ ->
                        fail <| sprintf
                            "Unexpected binary response from %s, with status code %d."
                            url
                            response.StatusCode
                    | Text body ->
                        if response.StatusCode = 200 then
                            let rycod = rycodExample.ThumperRycod.Parse(body)

                            let rabit = rycod.RabitSpecs.[0]

                            let linkers =
                                let five = rabit.UpstreamLink.String |> Option.defaultValue ""
                                let three = rabit.DownstreamLink.String |> Option.defaultValue ""
                                Some(five, three)

                            ok {
                                name = rabit.Name
                                dna = Dna(rabit.DnaElementSpecs.[0].DnaSequence)
                                linkers = linkers
                            }
                        else
                            fail <| sprintf
                                "Request to %s failed with status code %d: %s."
                                url
                                response.StatusCode
                                body

                if useCache then
                    match fetchCache.TryGetValue(url) with
                    | (true, x) -> ok x
                    | (false, _) ->
                        lookup()
                        >>= (fun part ->
                            fetchCache.TryAdd(url, part) |> ignore
                            ok part)
                else
                    lookup()

/// Determine which sets of linkers to use for a design
let getLinkerSetsForDesign (aIn: DnaAssembly) =
    let defaultLinkers = ["0";"2";"A";"3";"9"]
    if aIn.linkerHint = "" then
        (defaultLinkers, defaultLinkers)
    else
        let split (s:string) =
            s.Trim().Split([| ',' |])
            |> Array.map (fun s -> s.Trim())
            |> List.ofArray
        let validLinkerSet (s:string list) =
            s.Head = "0" && (List.rev s |> List.head = "9")
        let invalidLinkerWarn a b =
            let linkerWarnOff =
                match aIn.pragmas.TryFind("warnoff") with
                | Some(w) -> w.hasVal("zeronine")
                | None -> false
            not linkerWarnOff &&
            (not (validLinkerSet a) || not (validLinkerSet b))

        let (altLinkers1, altLinkers2) =
            match Array.map (split) (aIn.linkerHint.Split([| '|' |])) with
            | [| a ; b |] ->
                if invalidLinkerWarn a b then
                    printf
                        "linker sets must start with linker 0 and end with linker 9.  %s fails\n"
                        aIn.linkerHint
                    // FIXME: this error condition just prints a message, should it blow up?
                a, b
            | _ ->
                failwithf
                    "bad #linkers structure, should be one part with | sep, not %s"
                    (aIn.linkerHint)

        if verbose then printf "Using alternative linkers: %A,%A" altLinkers1 altLinkers2
        (altLinkers1, altLinkers2)

/// Determine how many junctions will require RYSE linkers.
/// Inline dna segments won't for example unless they have rabitstart/end pragmas
let rec countRyseLinkersNeeded printVerbose total (l:DNASlice list) =
    match l with   // REGULAR | MARKER | LINKER | INLINEST
    | [] ->
        printVerbose (sprintf "  countRyseLinkersNeeded:  done total=%d" total)
        total
    | a::b::tl when a.sliceType = FUSIONST ->
        printVerbose (sprintf
            "  countRyseLinkersNeeded:  +0 fusion slice, part=%s , fused to %s 0 linkers needed"
            a.description b.description)
        countRyseLinkersNeeded printVerbose total tl
    | a::_::tl when
            a.sliceType = INLINEST &&
            (a.pragmas.ContainsKey("rabitstart") || a.pragmas.ContainsKey("rabitend")) ->
        printVerbose (sprintf
            "  countRyseLinkersNeeded:  +0 inline slice at start/end of rabit 0 more linkers needed for %s"
            a.description)
        countRyseLinkersNeeded printVerbose (total+1) tl // Add one more rabit if we need to insert a linker next to this inline slice
    | a::b::tl when a.sliceType = INLINEST && b.sliceType = REGULAR ->
        printVerbose (sprintf
            "  countRyseLinkersNeeded:  +0 inline slice before a regular slice, 0 more linkers needed for %s"
            a.description)
        countRyseLinkersNeeded printVerbose total tl
    | a::tl ->
        printVerbose (sprintf
            "  countRyseLinkersNeeded:  +1 basic case, part=%s" a.description)
        countRyseLinkersNeeded printVerbose (total+1) tl


/// Assign ryse linkers to the design.
let mapRyseLinkers
        (opts:ParsedOptions)
        (getLinker: string -> RYSELinker)
        (aIn : DnaAssembly) =

    let printVerbose msg =
        if opts.verbose then printfn "%s" msg

    printVerbose "ENTERING: mapRyseLinkers"
    /// If they are building just a stitch, we need to know not to look for the marker
    let megaMono = match assemblyMode aIn.pragmas with | Megastitch -> false | Stitch -> true

    // First establish which linker set we are using
    let (allLinkers1, allLinkers2) = getLinkerSetsForDesign aIn

    // Replace DNA parts with expanded version including linkers
    let dnaName = aIn.dnaParts |> List.map (fun d -> d.description) |> fun x -> String.Join(";",x)

    // An error description for user in the event problems happen
    let errorDesc =
        "linkers=["
      + String.Join(",",allLinkers1)
      + "]|["
      + String.Join(",",allLinkers2)
      + "]/" + dnaName

    /// Assign RYSE linkers to junctions that need them
    let rec assign startLinkers (phase:bool) (l:DNASlice list) (linkers: string list) res =
        let prepLinker (n:string) =
            let linker = getLinker n

            // DNA for the linker
            let dna = linker.dna |> fun x -> if phase then x else x.RevComp()
            // Build the linker entry
            {id = None;
             extId = None;
             sliceName = "";
             uri = Some(uri.linkerUri linker.name)
             dna = dna;
             sourceChr = "linker";
             sourceFr = 0<ZeroOffset>;
             sourceTo = 0<ZeroOffset>;
             template = None;
             amplified = false;
             sourceFrApprox = false;
             sourceToApprox = false;
             destFr = -999<ZeroOffset>;
             destTo = -999<ZeroOffset>;
             sourceFwd = phase;
             description = sprintf "Linker_%s" n;
             sliceType = LINKER;
             destFwd = phase;
             dnaSource = "";
             pragmas = EmptyPragmas;
             breed = B_LINKER;
             materializedFrom = None;
             annotations = []}

        let noLinkersLeftMsg =
            sprintf "mapRyseLinkers: out of linkers.  Started with %A" startLinkers

        // We *PRE* assign linkers to pieces so as we recognize a pattern, we
        // are emitting the linker that comes before (upstream in dna construct
        // orientation) the rabit part.  At the end we tack on the final '0'
        // linker for the second megastitch.  Note the pre assignment happens
        // the same way for the B stitch which is still constructed left to
        // right (relative to final megastitch construct), so we are reporting the
        // linker to the left of the B stitch elements (downstream of Rabit in
        // B stitch orientation)
        printVerbose (sprintf
            "\n\n==============================================\nin mapRyseLinkers:\nassign: sliceList=%A \nlinkers=%A\n"
            (List.map (fun (x:DNASlice) ->x.description) l) linkers)
        // recursive match expression
        match l with
        | [] ->
            if megaMono && linkers.Length > 0 then
                let last = List.rev linkers |> List.head
                printVerbose (sprintf
                    "in mapRyseLinkers:assign: end of megaMono, picking %A for last primer"
                    last)
                (prepLinker last)::res
            else
                if phase then // Must be in phase two by the time we get here
                    failwith "in mapRyseLinkers:assign, ran out of linkers while still in phase one :(  .  Are you missing a ### marker for your megastitch?"

                match linkers with
                | [linkerName] ->
                    printVerbose (sprintf
                        "in mapRyseLinkers:assign, finishing on %A linker\n"
                        linkerName)
                    (prepLinker linkerName)::res
                | x ->
                    failwithf
                        "mapRyseLinkers: unexpected linker complement  '%s' left at end \nphase=%s (%s)\n"
                        (x.ToString()) (if phase then "phase1" else "phase2") errorDesc

        | a::b::c when
                a.sliceType = INLINEST &&
                (b.sliceType = REGULAR || b.sliceType=SliceType.INLINEST) &&
                a.pragmas.ContainsKey("rabitstart") ->
            // a in an inline type with a pragma telling us to initiate the
            // start of a rabit here, so it needs to be preceded by a linker in
            // the final part list
            //
            //     / inline {#rabitstart} /  ; regularRabit
            //     LINKER :: inline :: regularRabit ..
            printVerbose "in mapRyseLinkers: a is INLINEST with rabitstart"
            match linkers with
            | [] ->
                failwith noLinkersLeftMsg
            | linkerName::lt ->
                printVerbose (sprintf
                    "inlineST starting following rabit, assign linker %s\n"
                    linkerName)

                let linker = prepLinker linkerName
                assign startLinkers phase c lt (b::a::linker::res)

        | a::b::c when
                a.sliceType = INLINEST &&
                (b.sliceType = REGULAR || b.sliceType = MARKER || b.sliceType = INLINEST)
                && a.pragmas.ContainsKey("rabitend") ->
            // a in an inline type with a pragma telling us to end a rabit here,
            // so it needs to be followed by a rabit in the final part list
            //
            //     / inline {#rabitend} /  ; regularRabit
            //     inline :: LINKER :: regularRabit ..
            match linkers with
            | [] -> failwith noLinkersLeftMsg
            | linkerName::_ ->
                //let linker = prepLinker linkerName
                //
                // NB: we push B back into the work list, since we might
                // want to process is specially if it's a marker for example,
                // we can't simply push it to the output part list
                printVerbose (sprintf
                    "inlineST ending preceding rabit, assign linker %s, remaining %A\n"
                    linkerName (b::c))
                // fixed this - wasn't pushing b out
                assign startLinkers phase (b::c) linkers (a::res)

        | [a;b] when b.sliceType = INLINEST && a.sliceType = REGULAR ->
            // a is regular and b inline - special terminal inline case
            //
            printVerbose "terminal regular::inlineST case, take one linker\n"
            match linkers with
            | [] -> failwith noLinkersLeftMsg
            | linkerName::ltl ->
                printVerbose (sprintf
                    "assign linker %s, no remaining parts\n"
                    linkerName)
                let linker = prepLinker linkerName

                assign startLinkers phase [] ltl (b::a::linker::res)

                //assign startLinkers phase c linkers (b::a::res)

        | a::b::c when a.sliceType = INLINEST && b.sliceType = REGULAR ->
            // a is an inline and b regular, so take b and a and move them to
            // the output
            printVerbose "inlineST no linker needed\n"
            assign startLinkers phase c linkers (b::a::res)

        | a::b::c when a.sliceType = FUSIONST ->
            // No need for a linker before or after a fusion place holder, since
            // it doesn't really exist, but is a hint to join the two adjacent/
            // pieces.
            printVerbose "Fusion ST no linker needed\n"

            assign startLinkers phase c linkers (b::a::res)

        | hd::tl -> // General case, chomp one linker
            match linkers with
            | [] -> failwith noLinkersLeftMsg
            | linkerName::lt ->
                let linker = prepLinker linkerName
                printVerbose (sprintf
                    "Assigning linker %s to %s/%s\n"
                    linkerName hd.description (formatST hd.sliceType))

                // DETECT MARKER, transition to phase II
                if hd.sliceType = MARKER then
                    // Reconstruct output with linker and moved piece
                    // If we hit the marker, flip orientation, restart linker list but backwards
                    printVerbose
                        "countRyseLinkersNeeded in phase II start with 1 for final leading 0 linker (note 9 linker not included in count)"
                    let linkersReq = countRyseLinkersNeeded printVerbose 1 tl

                    // check this first
                    if linkersReq > allLinkers2.Length then
                        failwithf
                            "mapRyseLinkers - need %d linkers to finish, only %d available %A\n"
                            linkersReq allLinkers2.Length errorDesc

                    printVerbose (sprintf
                        "\n\n#############################################\npart 2 of megastitch - %d linkers required, using %A\n"
                        linkersReq (Seq.take linkersReq allLinkers2 |> List.ofSeq) )

                    // Flipping part around, but only for marker case.
                    // Should this also happen for the regular parts?  Must be dealt with elsewhere ;(
                    let hd' = { hd with destFwd = if phase then hd.destFwd else not hd.destFwd }

                    // phase set to false to denote second phase,
                    // grab the second set of linkers allLinkers2
                    assign allLinkers2 false tl (Seq.take linkersReq allLinkers2 |> List.ofSeq |> List.rev) (hd'::linker::res)
                else
                    // We are putting linker before the piece hd (output gets flipped at the end).
                    // Make sure linker is appropriate to precede part hd.
                    // Matters in the case where hd is reuse of a ryse part.
                    match hd.extId with
                    | Some(extId) -> //when x.[0] = 'R' || x.[0] = 'r' ->
                        let h = fetchPart extId |> returnOrFail

                        let hFive, hThree = h.linkers |> Option.defaultValue ("", "")

                        let linkerName = extractLinker linker.description
                        let linkerNameNext =
                            match lt with
                            | hd::_ -> hd // (List.head lt)
                            | [] -> failwith noLinkersLeftMsg

                        let failWithLinkerErrorMsg whichEnd hEnd name =
                            failwithf
                                "part %s expects %s linker (%s) and linker (%s) used instead \nERROR:(%s)"
                                extId whichEnd hEnd name errorDesc

                        if phase then
                            if linkerName <> hFive then
                                failWithLinkerErrorMsg "5'" hFive linkerName
                            if linkerNameNext <> hThree then
                                failWithLinkerErrorMsg "3'" hThree linkerNameNext
                        else
                            if linkerName <> hThree then
                                failWithLinkerErrorMsg "3'" hThree linkerName
                            if linkerNameNext <> hFive then
                                failWithLinkerErrorMsg "5'" hFive linkerNameNext
                    | _ -> ()

                    assign startLinkers phase tl lt (hd::linker::res) // Reconstruct output with linker and moved piece

    let res = {aIn with dnaParts = assign allLinkers1 true aIn.dnaParts allLinkers1 []|> List.rev |> recalcOffset }
    printVerbose "DONE:  mapRyseLinkers\n"
    res


// ==================================================================
// RYSE components in SBOL format
// ==================================================================

// --- static URIs ---

/// Return the URIs for linker ComponentDefintion and Sequence
let linkerUris linkCode =
    (unwrap (buildUri ["Component"; "Linker"] linkCode),
     unwrap (buildUri ["ComponentSequence"; "Linker"] linkCode))

/// Return the SBOL specification of a RYSE linker.
let sbolLinker (linker:RYSELinker) =
    let cdUri, seqUri = linkerUris linker.name
    {id = {identity = cdUri; name = Some("RYSE linker " + linker.name); description = None};
     roles = [sbolExample.ryseLinkerRoleUri];
     sequence =
        Some(
            {id = {identity = seqUri; name = None; description = None};
             elements = linker.dna.str});
     subcomponents = [];
     gslProg = None}

type PrimerType =
    | Amplification
    | Quickchange

/// Create the SBOL objects for a primer.
let sbolPrimer
        (name:string)
        (tail: Dna)
        (body: Dna)
        (kind:PrimerType) =

    // make tail and body items
    let tailComp =
       {id = {identity = uri.createTempUri(); name = Some(name + "_tail"); description = None};
        roles = [sbolExample.primerTailRoleUri];
        sequence = Some(sbolExample.seqFromDna tail);
        subcomponents = [];
        gslProg = None}
    let tailSubcomp = tailComp.asSubcomponent([], [sbolExample.primerTailRoleUri])

    let bodyComp =
       {id = {identity = uri.createTempUri(); name = Some(name + "_body"); description = None};
        roles = [sbolExample.primerBodyRoleUri];
        sequence = Some(sbolExample.seqFromDna body);
        subcomponents = [];
        gslProg = None}
    let bodySubcomp = bodyComp.asSubcomponent([], [sbolExample.primerBodyRoleUri])

    let primerRole =
        match kind with
        | Amplification -> sbolExample.ampPrimerRoleUri
        | Quickchange -> sbolExample.quickchangePrimerRoleUri

    let fullComp =
       {id = {identity = uri.createTempUri(); name = Some(name); description = None};
        roles = [primerRole];
        sequence = Some(sbolExample.seqFromDna (DnaOps.append tail body));
        subcomponents = [tailSubcomp; bodySubcomp];
        gslProg = None}


    (fullComp, [fullComp; tailComp; bodyComp])

/// Return the ComponentDefintion for a Rabit DNA element.
/// Primers are passed as two-tuples with the implicit
/// ordering (fwd/5', rev/3')
let sbolDnaElement
        (name:string)
        (desc:string option)
        (compUri:Uri option)
        (dna: Dna)
        (ampPrimers:ComponentDefinition*ComponentDefinition)
        (quickchangePrimers:(ComponentDefinition*ComponentDefinition) option) =

    let mutable subcomps =
        [(fst ampPrimers).asSubcomponent([], [fivePrimePrimerRoleUri]);
         (snd ampPrimers).asSubcomponent([], [threePrimePrimerRoleUri])]

    match quickchangePrimers with
    | Some(qc5p, qc3p) ->
        subcomps <- subcomps@
            [qc5p.asSubcomponent([], [fivePrimePrimerRoleUri; quickchangePrimerRoleUri]);
             qc3p.asSubcomponent([], [threePrimePrimerRoleUri; quickchangePrimerRoleUri])]
    | None -> ()

    {id = {identity = (match compUri with | Some(u) -> u | None -> uri.createTempUri());
           name = Some(name);
           description = desc};
     roles = [rabitDnaRoleUri];
     sequence = Some(sbolExample.seqFromDna dna);
     subcomponents = subcomps;
     gslProg = None}

/// Return the ComponentDefintion for a Rabit.
/// Linkers are passed as a two-tuple with implicit ordering (5', 3')
let sbolRabit
        (name:string)
        (desc:string)
        (compUri:Uri option)
        breed
        (orientation:Orientation)
        (dna:Dna)
        (dnaElements:ComponentDefinition list)
        (linker5p, linker3p) =

    if dnaElements.IsEmpty then
        failwithf "Tried to make an SBOL Rabit '%s' with no DNA elements!" name
    // Integrate the linkers and dna elements with explicit locations

    /// Create a range subcomponent in a linear sequence
    let rangeSubcomp (comp:ComponentDefinition) lastbp orient roles =
        let startbp = lastbp + 1
        let endbp =
            match comp.sequence with
            | Some(s) -> lastbp + s.elements.Length
            | None -> failwithf "Rabit %s has a DNA element without a sequence." name
        let loc = Range({start = startbp; stop = endbp; orient = orient})
        comp.asSubcomponent([loc], roles), endbp

    let linker5pSC, lastbp = rangeSubcomp linker5p 0 FWD [fivePrimeLinkerRoleUri]

    /// assign explicit locations to all dna elements
    let rec createDnaSubcomps comps lastbp =
        match comps with
        | [comp] ->
            let sc, endbp = rangeSubcomp comp lastbp orientation [rabitDnaRoleUri]
            [sc], endbp
        | comp::tail ->
            let sc, endbp = rangeSubcomp comp lastbp orientation [rabitDnaRoleUri]
            let others, finalbp = createDnaSubcomps tail endbp
            sc::others, finalbp
        | [] -> [], lastbp // if you handed us an empty list

    let dnaSubcomps, lastbp = createDnaSubcomps dnaElements lastbp

    let linker3pSC, lastbp = rangeSubcomp linker3p lastbp FWD [threePrimeLinkerRoleUri]

    if lastbp <> dna.Length then
        failwithf "Subcomponent lengths added up to %d, but rabit '%s' has dna sequence of length %d."
            lastbp name dna.Length

    {id = {identity = (match compUri with | Some(u) -> u | None -> uri.createTempUri());
           name = Some(name);
           description = Some(desc)};
     roles = [rabitRoleUri; (sbolExample.rabitBreedRole breed) ];
     sequence = Some(sbolExample.seqFromDna dna);
     subcomponents = linker5pSC::linker3pSC::dnaSubcomps;
     gslProg = None}

/// Return the ComponentDefintion for a Stitch.
let sbolStitch (name:string) (desc:string) (compUri:Uri option) (rabits:ComponentDefinition list) =
    if rabits.IsEmpty then
        failwithf "Tried to make an SBOL Stitch '%s' with no rabits!" name

    let rec rabitSubcomponents (rlist:ComponentDefinition list) rabitScs =
        match rlist with
        | [r] ->
            let rsc = r.asSubcomponent([], [stitchRabitRoleUri])
            rsc, rsc::rabitScs
        | r::nr::tl ->
            let nrsc, rabitScs = rabitSubcomponents (nr::tl) rabitScs
            let rsc = r.asSubcomponent([Precede(nrsc)], [stitchRabitRoleUri])
            rsc, rsc::rabitScs
        | [] -> failwith "Unreachable match condition in rabit subcomponent construction."

    let _, rabitScs = rabitSubcomponents rabits []

    // actually make the ComponentDefinition
    {id = {identity = (match compUri with | Some(u) -> u | None -> uri.createTempUri());
           name = Some(name);
           description = Some(desc)};
     roles = [stitchRoleUri];
     sequence = None;
     subcomponents = rabitScs;
     gslProg = None}

/// Return the ComponentDefintion for a Megastitch.
let sbolMegastitch
    (name:string)
    (desc:string)
    (compUri:Uri option)
    (stitchA:ComponentDefinition)
    (stitchB:ComponentDefinition option) =

    let subcomps =
        match stitchB with
        | Some(s) ->
            let sBsc = s.asSubcomponent([], [stitchRoleUri])
            [stitchA.asSubcomponent([Precede(sBsc)], [stitchRoleUri]); sBsc]
        | None -> [stitchA.asSubcomponent([], [stitchRoleUri])]

    // actually make the ComponentDefinition
    {id = {identity = (match compUri with | Some(u) -> u | None -> uri.createTempUri());
           name = Some(name);
           description = Some(desc)};
     roles = [megastitchRoleUri];
     sequence = None;
     subcomponents = subcomps;
     gslProg = None}
