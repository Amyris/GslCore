module ProcessCmdLineArgs
open Amyris.ErrorHandling
open commandConfig
open PluginTypes
open commonTypes
open utils
open Amyris.Bio.utils
open System.IO
open RefGenome

let getArgAndAliases (a: CmdLineArgSpec) = seq {
    yield (a.name, a)
    for alias in a.alias -> (alias, a)}

let checkConflict pluginName context (existingSpecs: Map<string,CmdLineArgSpec>) (name, spec) =
    match existingSpecs.TryFind(name) with
    | Some(maybeConflict) when spec <> maybeConflict ->
        failwithf "The plugin '%s' provided the command line argument '%s' whose definition conflicts with %s."
            pluginName name context
    | _ -> (name, spec)


/// Collect and validate all command line arguments provided by plugins.
/// Return an initialized command line arg collection.
let collectCommandLineArgs plugins =
    let builtinSpecs =
        builtinCmdLineArgs |> Map.toSeq
        |> Seq.map (fun (name, a) -> (name, a.spec))
        |> Map.ofSeq

    let specsByPlugin =
        plugins
        |> List.map (fun (p: Plugin) ->
            let argsAndAliases =
                p.ProvidedArgs()
                |> List.map getArgAndAliases
                |> Seq.concat
            (p.name, argsAndAliases))

    let pluginSpecs =
        specsByPlugin
        |> List.fold
            (fun collectedSpecs (pluginName, specs) ->
                specs
                |> Seq.map (checkConflict pluginName "a built-in" builtinSpecs)
                |> Seq.map (checkConflict pluginName "that from another plugin" collectedSpecs)
                |> Seq.append (collectedSpecs |> Map.toSeq)
                |> Map.ofSeq)
            Map.empty


    {builtins = builtinSpecs;
     builtinsWithProc = builtinCmdLineArgs;
     fromPlugins = pluginSpecs}

/// Check an arg list against a arg defintion, ensuring enough arguments are present.
/// If so, return a type that indicates successful format, as well as any remaining args.
let private tryParseArg (a: CmdLineArgSpec) argList =
    let nameWithDashes = sprintf "--%s" a.name

    let rec getParams n argList ps =
        // if we got all our parameters, done
        if n = 0 then ((List.rev ps), argList)
        else
            match argList with
            | [] ->
                failwithf "Insufficient params for %s; got %A, needed %d more"
                    nameWithDashes (List.rev ps) n
            | [p] -> getParams (n-1) [] (p::ps)
            | p::tl -> getParams (n-1) tl (p::ps)

    let ps, restOfArgList = getParams a.param.Length argList []
    // Make sure none of the parameters are another command
    let badParams =
        List.filter (fun (i:string) -> i.StartsWith("--")) ps
    if not badParams.IsEmpty then
        failwithf "%s got bad parameters '%A'" nameWithDashes badParams
    ({spec = a; values = ps}, restOfArgList)

/// Parse all command line arguments, and return a list of parsed args and file names
/// or fail with an exception.
let private parseAllCommandLineArgs
        (plugins: Plugin list)
        (argSpecs: CollectedCommandLineArgs)
        (argList: string list) =
    /// Parse one item and return the rest of the arg list, the parsed arg set,
    /// and any file names we've accumulated.
    let parseOneItem (h:string) (argList:string list) (accumulatedArgs: ParsedCmdLineArg list) files =
        // Should only be a command or an input file.
        // FIXME: we should fail here if we find a "file" in between two --args
        if h.StartsWith("--") then
            let arg = h.[2..]
            if arg = "help" then
                printfn "%s" (usageText argSpecs)
                (argList, accumulatedArgs, files)
            elif arg = "plugins" then
                let pluginNames = plugins |> List.map (fun p -> p.name) |> String.concat "\n"
                printfn "Installed plugins:\n%s" pluginNames
                (argList, accumulatedArgs, files)
            else
                match argSpecs.TryFind arg with
                | None ->
                    failwithf "Unrecognied command line argument: %s" arg
                | Some(a) ->
                    let parsedArg, remainingArgs = tryParseArg a argList
                    // Call the parameter's config function and return the result
                    (remainingArgs, parsedArg::accumulatedArgs, files)
        // Otherwise this must be a input file (or bad input...)
        else
            (argList, accumulatedArgs, h::files)
    
    /// Recursively parse each input command.
    let rec parseCmdRec (argList:string list) parsedArgs files = 
        match argList with
        // nothing left to parse, we're done
        | [] -> (List.rev parsedArgs, List.rev files)
        // parse one item
        | h::tl -> parseCmdRec <||| (parseOneItem h tl parsedArgs files)

    parseCmdRec argList [] []

type ConfigurationState =
    {opts: ParsedOptions; files: string list; plugins: Plugin list; ga: GlobalAssets}
    with
    /// Convenience property, assumes we've validated that we have exactly one input file.
    member x.InputFile = x.files.[0]
   
/// generate list of available reference genome folders
let enumerateLibs (opts:ParsedOptions) =
    Directory.EnumerateDirectories(opts.libDir)
    |> Seq.map (Amyris.Bio.utils.baseName) |> List.ofSeq

/// Load static assets and initialize global caches.
let loadGlobalAssets (opts:ParsedOptions) =
    let lib = opj opts.libDir "lib.fa"

    // Crude sequence library for misc pieces
    let library =
        if File.Exists lib then
            Amyris.Bio.biolib.readReference lib
            |> Seq.map (fun kv -> (kv.Key.ToUpper(),kv.Value |> basesUpper ))
            |> Map.ofSeq
        else Map.empty

    if opts.verbose then printfn "opts.libDir=%s" opts.libDir

    let availRefs = enumerateLibs opts

    if opts.verbose then printfn "availrefs=%A" availRefs

    let rgs =
        seq {
            for s in availRefs do
                let p = opj opts.libDir s
                if not (Directory.Exists(p)) then
                    failwithf "ERROR: unable to find genome reference dir %s\n" p
                if File.Exists(opj p (sprintf "%s.fsa" s)) then
                    yield (s, new GenomeDef(opts.libDir, s))
            } |> Map.ofSeq

    // Debugging - dump list of available genomes
    if opts.verbose then
        printf "loadedgenomes %A\n"
            (rgs |> Seq.map (fun kv -> kv.Key) |> List.ofSeq)

    library, rgs

/// Parse a command line arguments.  Return the parsed options and the list of
/// input files.
let configure loadGA argSpecs (plugins: Plugin list) (argList:string list) =

    let parsedArgs, files = parseAllCommandLineArgs plugins argSpecs argList

    /// Use the args to update the builtin options.
    let parsedOptions =
        parsedArgs
        |> List.fold
            (fun opts (arg: ParsedCmdLineArg) ->
                // get the appropriate definion, and use its processor to update the parsed options
                match argSpecs.builtinsWithProc.TryFind(arg.spec.name) with
                | Some(a) -> a.proc arg.values opts
                | None -> opts)
            defaultOpts

    /// Now use the args to update all of the plugins.
    let updatedPlugins =
        plugins
        |> List.map (fun plugin ->
            plugin.ConfigureFromCommandLineArgs(parsedArgs))

    let codonProvider =
        match getAllProviders getCodonProviders updatedPlugins with
        | [p] -> p
        | [] -> failwithf "No CodonProvider plugins found.  GSLc requires exactly one to be provided."
        | x -> failwithf "%d CodonProvider behaviors found.  GSLc requires excatly one to be provided." x.Length

    // Load static assets and initialize caches.
    let ga =
        if loadGA then
            let library, rgs = loadGlobalAssets parsedOptions
            {seqLibrary = library; codonProvider = codonProvider; rgs = rgs}
        else {seqLibrary = Map.empty; codonProvider = codonProvider; rgs = Map.empty}

    {opts = parsedOptions; files = files; plugins = updatedPlugins; ga = ga}
