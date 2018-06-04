/// Assembly transforming plugin that implements seamless part assembly.
module SeamlessPlugin

open System
open System.IO
open AstExpansion
open AstTypes
open LegacyParseTypes
open commonTypes
open commandConfig
open DnaCreation
open PrimerCreation
open Amyris.Bio
open utils
open PluginTypes
open constants
open Amyris.ErrorHandling
open gslcProcess

let mt = {
    id = None;
    extId = None;
    sliceName = "";
    uri = Some(uri.linkerUri "MT")
    dna = Amyris.Dna.Dna("");
    sourceChr = "linker";
    sourceFr = 0<ZeroOffset>;
    sourceTo = 0<ZeroOffset>;
    template = None;
    amplified = false;
    sourceFrApprox = false;
    sourceToApprox = false;
    destFr = -999<ZeroOffset>;
    destTo = -999<ZeroOffset>;
    sourceFwd = true;
    description = sprintf "Linker_MT";
    sliceType = LINKER;
    destFwd = true;
    dnaSource = "";
    pragmas = pragmaTypes.EmptyPragmas;
    breed = B_LINKER;
    materializedFrom = None;
    annotations = []}

let mtRev = {mt with sourceFwd = false}

let dumpSliceLayout (slices:DNASlice list) =
    String.Join(";", slices |> List.map (fun s -> formatST s.sliceType) )
 
/// Insert FUSE directives and MT linkers to get a seamless design from later primergen
let procInsertFuse verbose (l:DNASlice list) =
    if verbose then
        printfn "Entering procInsertFuse"
        let names = l |> List.map (fun l -> l.sliceName)
        printfn "Slices presented: %s" (String.Join(";",names))
    let rec procInsertFuseInternal (l:DNASlice list) res =
        if verbose then printfn "placeFuseForSeamless: top l=%s" (dumpSliceLayout l)
        let finish parts =mt::(List.rev parts)@[mtRev]

        match l with
            | [] -> finish l
            | [x] -> finish (x::res) // don't fuse after last piece
            | hd::tl when hd.sliceType = SliceType.FUSIONST ->
                if verbose then printfn "placeFuseForSeamless: .. skipping existing fuse"
                // pass existing fuse elements straight through
                procInsertFuseInternal tl (hd::res) 
            | hd::tl when hd.sliceType = SliceType.INLINEST ->
                // inline segments should get primer gen anyway I think
                if verbose then printfn "placeFuseForSeamless: .. ignoring inline"
                procInsertFuseInternal tl (hd::res) 

            | hd::middle::tl when hd.sliceType = SliceType.FUSIONST && middle.sliceType=SliceType.INLINEST ->
                // omit hd, not needed
                procInsertFuseInternal tl (middle::res)

            | hd::middle::tl when hd.sliceType = SliceType.REGULAR && middle.sliceType=SliceType.INLINEST ->
                procInsertFuseInternal tl (middle::hd::res)

            | hd::tl ->
                if verbose then printfn "placeFuseForSeamless: .. general case gets fuse"
                procInsertFuseInternal tl (fusionSliceConstant::hd::res)

            | hd::tl ->
                procInsertFuseInternal tl (hd::res)
    procInsertFuseInternal l []

/// strategic place fuse directives into linker free constructs to effectively
/// request a seamless design
let placeFuseForSeamless (at:ATContext) (a:DnaAssembly) =
    let printVerbose message = if at.opts.verbose then printfn "%s" message


    let linkered = a.dnaParts |> List.exists (fun d -> d.sliceType = SliceType.LINKER)
    if linkered then
        printVerbose "placeFuseForSeamless: skipping, since linkers present"
        ok a // we don't touch cases where linkers are already placed
    else
        printVerbose "placeFuseForSeamless: examining need for fuse slices"
        printVerbose
            (sprintf "placeFuseForSeamless: starting layout: %s" (dumpSliceLayout a.dnaParts))

        // flank final result with empty linkers to get end primer generation
        let dnaPartsProcessed = procInsertFuse at.opts.verbose a.dnaParts

        printVerbose
            (sprintf "placeFuseForSeamless: final layout: %s" (dumpSliceLayout dnaPartsProcessed))

        ok {a with dnaParts = dnaPartsProcessed}

let seamlessArg =
   {name = "seamless";
    param = ["(true or false)"];
    alias = [];
    desc = "Perform seamless assembly."}


type SeamlessAssembler = {
    run: bool
    /// Optionally attach a function to this plugin behavior to permit its operation to be
    /// configured by command line arguments injected by other plugins.  This is necessary because
    /// seamless assembly can alter a lot of expectations of downstream processing steps.
    processExtraArgs: ParsedCmdLineArg -> SeamlessAssembler -> SeamlessAssembler}
    with
    interface IAssemblyTransform with
        member x.ProvidedArgs() = [seamlessArg]
        member x.Configure(arg) =
            if arg.spec = seamlessArg then
                let run =
                    match arg.values with
                    | ["true"] -> true
                    | ["false"] -> false
                    | [x] -> failwithf "Invalid argument for seamless: '%s'. Options are 'true' or 'false'." x
                    | _ -> failwithf "Seamless plugin received the wrong number of command line arguments."
                {x with run = run}
            else x
            |> x.processExtraArgs arg
            :> IAssemblyTransform
        member x.ConfigureFromOptions(opts) =
            if opts.noPrimers then
                {x with run = false}
            else x
            :> IAssemblyTransform
        member x.TransformAssembly context assembly =
            if x.run then
                placeFuseForSeamless context assembly
                // Run existing fuse processor to clean up fuses between certain part combinations.
                // Is this necessary or does the seamless logic already account for this?
                >>= preProcessFuse context
            else
                ok assembly

/// Produce an instance of the seamless assembly plugin with the provided extra argument processor.
let createSeamlessPlugin defaultRun extraArgProcessor =
   {name = "seamless_assembly";
    description = Some "Perform seamless assembly by liberally fusing slices."
    behaviors =
      [{name = None;
        description = None;
        behavior = AssemblyTransform({run = defaultRun; processExtraArgs = extraArgProcessor})}]
    providesPragmas = [];
    providesCapas = []}

/// By default do not take any other command line args into account, and always run in seamless mode.
let seamlessPlugin = createSeamlessPlugin true (fun _ x -> x)