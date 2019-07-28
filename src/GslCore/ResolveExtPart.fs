module resolveExtPart
open commonTypes
open pragmaTypes
open LegacyParseTypes
open ryse
open applySlices
open Amyris.Bio.biolib
open constants
open Amyris.Dna
open Amyris.ErrorHandling
open PluginTypes

let legalPrefixes = [ ("r","rabit") ; ("b","biobrick")  ]

/// Does this part id start with a legal external part prefix
let legalPartPrefix (pid:string) =
    let pidLower = pid.ToLower()
    let rec checkPrefix (prefs:(string*string) list) =
        match prefs with
        | [] -> None
        | (tag,name)::_ when pidLower.StartsWith(tag) -> Some(name,pid.[tag.Length..])
        | _::tl -> checkPrefix tl

    checkPrefix legalPrefixes

let fetchSequence (verbose:bool) (library: SequenceLibrary) (ppp:PPP) (partId:PartIdLegacy) =
// Sequence can come either from the libary or preferably from the hutch directly
    let pid = partId.id
    let sliceName =
        match ppp.pr.TryGetOne("name") with
        | Some(name) -> name
        | None -> ""
    let uri = ppp.pr.TryGetOne("uri")
    match legalPartPrefix pid with
    | None ->
        failwithf
            "ERROR: partId reference %s isn't a defined alias and doesn't start with r for rabit\n"
            pid
    | Some(partSpace, _) ->
        match partSpace with
        | "rabit" ->
            let libName = "@"+pid.ToUpper()
            if not (library.ContainsKey(libName)) then

                // Have part from the hutch.  We might just use it verbatim or we might be
                // some modifications to it to make a new part
                let part = getRabit (int(pid.[1..]))

                // Check for slice modifications.  We can't handle any other type of mod at this point, so
                // ensure there are none.
                if partId.mods |> List.exists (fun m -> match m with | SLICE(_)-> false | _ -> true) then
                    failwithf "ERROR:  could not process mods for rabit %s %A\n" partId.id partId.mods

                // Look for simple case.  If we are just using the part from the hutch unadulterated, then
                // we specify things differently, referring to the external id
                if partId.mods.Length = 0 then
                    let dna = part.dna |> DnaOps.revCompIf (not ppp.fwd)

                    {id = None; 
                     extId = Some(pid.[1..]); 
                     sliceName = sliceName;
                     uri = uri; // TODO: use the URI of rabit from hutch here instead?
                     dna = dna; 
                     sourceChr = "library";
                     sourceFr = 0<ZeroOffset>;
                     sourceTo = (dna.Length-1)*1<ZeroOffset>;
                     sourceFwd = ppp.fwd;
                     sourceFrApprox = false;
                     sourceToApprox = false;
                     // Don't assign coordinates to pieces until later when we decide
                     // how they are getting joined up
                     destFr = 0<ZeroOffset>; 
                     destTo = 0<ZeroOffset>; 
                     destFwd = ppp.fwd;
                     description = part.name; 
                     sliceType = REGULAR; 
                     amplified = false;
                     template = Some dna; // not amplifying from this
                     dnaSource =
                        match ppp.pr.TryGetOne("dnasrc") with
                        | Some(d) -> d
                        | None -> pid;
                     pragmas = ppp.pr;
                     breed = B_X; // will be replaced at final submission
                     materializedFrom = Some(ppp);
                     annotations = []} // FIXME: need to generate annotations based on Rabit metadata
                else
                    // Otherwise, they are taking a hutch part and doing something to it,
                    // so the hutch is just another DNA source and they are effectively
                    // building a new rabit

                    // Start off assuming it's the full DNA slice
                    let startSlice =
                       {left = {x = 1<OneOffset>; relTo = FivePrime};
                        lApprox = false; 
                        rApprox = false;
                        right = {x = -1<OneOffset>; relTo = ThreePrime}}

                    // Apply the slice(s) to get a final coordinate range
                    let finalSlice = applySlices verbose partId.mods startSlice 

                    // Find the left and right hand ends of the slice
                    let x, y =
                        getBoundsFromSlice finalSlice part.dna.Length (Library(partId.id))
                        |> returnOrFail
                
                    let finalDNA =
                        part.dna.[(x/1<OneOffset>)-1..(y/1<OneOffset>)-1]
                        |> DnaOps.revCompIf (not ppp.fwd)

                    let name1 =
                        if partId.mods.Length = 0 then part.name
                        else (part.name + (printSlice finalSlice))
                    let name2 = if ppp.fwd then name1 else "!" + name1

                    {id = None; 
                     extId = None;
                     sliceName = sliceName;
                     uri = uri; // TODO: use URI from hutch part?  mint new URI?
                     dna = finalDNA; 
                     amplified = false;
                     template = Some finalDNA;
                     sourceChr = "library"; 
                     sourceFr = (finalSlice.left.x/(1<OneOffset>)-1)*1<ZeroOffset>; 
                     sourceTo = (finalSlice.right.x/(1<OneOffset>)-1)*1<ZeroOffset>;
                     sourceFwd = true;
                     sourceFrApprox = false;
                     sourceToApprox = false;
                     // Don't assign coordinates to pieces until later when we decide how they are getting joined up
                     destFr = 0<ZeroOffset>;
                     destTo = 0<ZeroOffset>;
                     destFwd = ppp.fwd;
                     description = name2;
                     sliceType = REGULAR; 
                     dnaSource =
                        match ppp.pr.TryGetOne("dnasrc") with
                        | Some(d) -> d
                        | None -> pid;
                     pragmas = ppp.pr;
                     breed = B_X; // they are hacking rabit, all bets are off
                     materializedFrom = Some(ppp);
                     annotations = []} // FIXME: need to generate annotations based on Rabit metadata

            else
                // Part is in the library
                let dna = library.[libName]
                {id = None;
                 extId = Some(pid.[1..]);
                 sliceName = sliceName;
                 uri = uri; // TODO: mint new URI if None?
                 dna = dna; 
                 template= Some dna;
                 amplified = false;
                 sourceChr = "library";
                 sourceFr = 0<ZeroOffset>;
                 sourceTo = (dna.Length-1)*1<ZeroOffset>
                 sourceFwd = true;
                 sourceFrApprox = false;
                 sourceToApprox = false;
                 // Don't assign coordinates to pieces until later when we decide
                 // how they are getting joined up
                 destFr = 0<ZeroOffset>;
                 destTo = 0<ZeroOffset>;
                 destFwd = ppp.fwd;
                 description = libName;
                 sliceType = REGULAR;
                 dnaSource = "library";
                 pragmas = ppp.pr;
                 breed = B_X;
                 materializedFrom = Some(ppp);
                 annotations = []} // FIXME: determine what metadata is available here

        | x ->
            failwithf "ERROR: unimplemented external partSpace %s\n" x


/// Get the full part sequence for this external reference, don't apply any slice mods to it
let fetchFullPartSequence (_ (* verbose*):bool) (library: SequenceLibrary) (partId:PartIdLegacy) =
// Sequence can come either from the libary or preferably from the hutch directly
    let pid = partId.id
    match legalPartPrefix pid with
    | None -> fail( sprintf "ERROR: partId reference %s isn't a defined alias and doesn't start with r for rabit\n" pid)
    | Some(partSpace, _) ->
        match partSpace with
        | "rabit" ->
            let libName = "@"+pid.ToUpper()
            if not (library.ContainsKey(libName)) then
                getRabit (int(pid.[1..]))
                |> ok
            else
                // Part is in the library
                ok(
                   {dna = library.[libName];
                    source = "library";
                    id = pid;
                    name = libName;
                    linkers = None})
        | x ->
            failwithf "ERROR: unimplemented external partSpace %s\n" x

let getExtPartSlice (verbose:bool) (partId:PartIdLegacy) =
    // Start off assuming it's the full DNA slice
    let startSlice =
       {left = {x = 1<OneOffset>; relTo = FivePrime};
        lApprox = false;
        rApprox = false;
        right = {x = -1<OneOffset>; relTo = ThreePrime}}

    // Apply the slice(s) to get a final coordinate range
    let finalSlice = applySlices verbose partId.mods startSlice 
    finalSlice

let applySliceToExtSequence
        (_ (* verbose*):bool)
        (extPart:ExtFetchSeq)
        (pr:PragmaCollection)
        (fwd:bool)
        (partId:PartIdLegacy)
        (finalSlice:Slice)  =
    let sliceName =
        match pr.TryGetOne("name") with | Some(n) -> n | None -> ""
    let uri = pr.TryGetOne("uri")
    if partId.mods.Length = 0 then
        let dna = extPart.dna |> DnaOps.revCompIf (not fwd)
        
        {id = None;
         extId = Some(extPart.id.[1..]);
         sliceName = sliceName; 
         uri = uri; // TODO: mint new URI if None?
         dna = dna;
         template = None;
         amplified = false;
         sourceChr = extPart.source;
         sourceFr = 0<ZeroOffset>;
         sourceTo = (extPart.dna.Length-1)*1<ZeroOffset>;
         sourceFwd = fwd;
         sourceFrApprox = false;
         sourceToApprox = false;
        // Don't assign coordinates to pieces until later when we decide how they are getting joined up
         destFr = 0<ZeroOffset>;
         destTo = 0<ZeroOffset>;
         destFwd = fwd;
         description = extPart.name;
         sliceType = REGULAR; 
         dnaSource = match pr.TryGetOne("dnasrc") with Some(d) -> d | None -> extPart.id;
         pragmas = pr;
         breed = B_X;
         materializedFrom = None;
         annotations = []} // FIXME: what do we have available here?
    else
        // Otherwise, they are taking a hutch part and doing something to it, so the hutch is just another
        // DNA source and they are effectively building a new rabit
        // Find the left and right hand ends of the slice
        let x, y =
            getBoundsFromSlice finalSlice extPart.dna.Length (Library(extPart.id))
            |> returnOrFail

        let finalDNA =
            extPart.dna.[(x/1<OneOffset>)-1..(y/1<OneOffset>)-1]
            |> DnaOps.revCompIf (not fwd)

        let name1 =
            if partId.mods.Length = 0 then extPart.name
            else (extPart.name + (printSlice finalSlice))
        let name2 = if fwd then name1 else "!" + name1

        {id = None;
         extId = None;
         sliceName = sliceName;
         uri = uri; // TODO: mint new URI if None?
         dna = finalDNA;
         template = Some finalDNA;
         amplified = true;
         sourceChr = extPart.source;
         sourceFr = (finalSlice.left.x/(1<OneOffset>)-1)*1<ZeroOffset>; 
         sourceTo = (finalSlice.right.x/(1<OneOffset>)-1)*1<ZeroOffset>;
         sourceFwd = true;
         sourceFrApprox = false;
         sourceToApprox = false;
         // Don't assign coordinates to pieces until later when we decide how they are getting joined up
         destFr = 0<ZeroOffset>;
         destTo = 0<ZeroOffset>;
         destFwd = fwd;
         description = name2;
         sliceType = REGULAR; 
         dnaSource = match pr.TryGetOne("dnasrc") with Some(d) -> d | None -> extPart.id;
         pragmas = pr;
         breed = B_X;
         materializedFrom = None;
         annotations = []} // FIXME: we can probably do better here
