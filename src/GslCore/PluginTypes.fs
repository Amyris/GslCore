/// Definitions of plug-in types and interfaces.
module PluginTypes
open System
open Amyris.ErrorHandling
open commonTypes
open commandConfig
open Amyris.Bio
open constants
open LegacyParseTypes
open pragmaTypes
open AstTypes
open RefGenome
open Amyris.Bio.IO.CodonUsage
open utils
open Amyris.Dna

/// Interface specification for plugins that want to inject command line arguments and
/// be configured from the command line.
type IConfigurable<'T> =
    /// Return the list of the names of command line arguments this behavior accepts.
    abstract member ProvidedArgs : unit -> CmdLineArgSpec list
    /// Configure this behavior and return a configured version, allowing for immutable behaviors.
    abstract member Configure : ParsedCmdLineArg -> 'T
    /// Configure this behavior using the built-in compiler options.
    /// This enables sharing of items like the lib directory, verbose flag, etc.
    abstract member ConfigureFromOptions : ParsedOptions -> 'T

// =======================
// plugin behavior defintion for codon cache and optimization
// =======================

type CodonOptTask =
   {verbose:bool;
    seedOverride:int option;
    refGenome:GenomeDef;
    aminoAcidSequence:string}

type ICodonProvider =
    /// Allow codon opt providers to add command line args and be configurable.
    inherit IConfigurable<ICodonProvider>

    /// Codon optimizers use the pragma environment to configure themselves locally.
    abstract member Setup : PragmaCollection -> ICodonProvider

    ///<summary>Perform codon optimization using a particular reference genome on a string
    /// representing a protein sequence, returning a
    /// codon-optimized version.  Optionally override the RNG seed for this particular run, as well
    /// as set verbosity.
    ///</summary>
    // FIXME: this should both accept and return a domain type from Amyris.Bio.  May need to
    // define a domain type for AA sequences to match Amyris.Dna
    abstract member DoCodonOpt : CodonOptTask -> Dna

    ///Provide a codon usage lookup table for the given ref genome.
    abstract member GetCodonLookupTable : GenomeDef -> CodonLookup


/// Helpful wrapper type for handing around GSLC's static assets and caches.
type GlobalAssets =
    {seqLibrary: SequenceLibrary;
     codonProvider: ICodonProvider;
     rgs: Map<string, GenomeDef>;}
// =========================
// plugin behavior defintion for allele swaps
// =========================

type EndPref = NTERM | CTERM | NONETERM

/// Amount of extra dna adjacent to the ORF to include
let orfPlusMargin = 100

type AlleleSwapJobAccept = Capabilities->float<PluginScore> option
type AlleleSwapDesignParams = {
    verbose:bool
    longStyle:bool
    endPref:EndPref
    codonLookup:CodonLookup
    gene:string
    name:string
    rg:GenomeDef
    f:sgd.Feature
    m:Mutation
    len:int<ZeroOffset>
    mutOff:int<ZeroOffset>
    orf: Dna
    orfPlus: Dna
    pragmas: PragmaCollection
}

type AlleleSwapProvider = { jobScorer:AlleleSwapJobAccept ; provider:AlleleSwapDesignParams -> GslSourceCode }



// ==================================================
// Marker handler for converting ### into sequence
// ==================================================

/// Information provided when a marker materialization happens
type MarkerMaterializationTask = { markerSet : string ; dnaSource : string ; ppp:PPP}

type IMarkerProvider =
    /// Allow marker providers to add command line args and be configurable.
    inherit IConfigurable<IMarkerProvider>
    /// Emit a materialized DNA sequence for the requested marker and part.
    abstract member CreateDna : MarkerMaterializationTask -> commonTypes.DNASlice
    abstract member IsLegal : string -> bool
    abstract member ListMarkers : unit -> string list
    abstract member ScoreJob : Capabilities -> float<PluginScore> option

// =======================
// plugin behavior definition for l2 expansion
// =======================

type L2JobAccept = Capabilities->float<PluginScore> option

type L2DesignParams = {
        rgs:GenomeDefs
        megastitch : bool
        refGenome: string
        line : BuiltL2Expression
        pragmas: PragmaCollection
}
type L2Provider = {
        jobScorer:L2JobAccept
        explicitLocusProvider:L2Id->L2DesignParams-> GslSourceCode
        implicitLocusProvider:L2DesignParams-> GslSourceCode
}

// ==================================================
// plugin for retrieving an existing part
// ==================================================

type ExtFetchSeq = {
    /// Human-readable name.
    name: string
    /// DNA sequence of this part.
    dna: Dna
    /// Optional RYSE linker specification (5' link code, 3' link code).
    linkers: (string*string) option
}

type IPartProvider =
    /// Allow part providers to add command line args and be configurable.
    inherit IConfigurable<IPartProvider>
    /// The name of this part provider service.
    abstract member Name: string
    /// Return true if this provider thinks it recognizes the provided identifier.
    abstract member Accept: string -> bool
    /// Call the service to retrieve this part.
    /// Implementors may assume that this method will only be called if Accept has returned true.
    abstract member Retrieve: string -> Result<ExtFetchSeq, string>

// ======================
// plugin behavior definition for output assembly transformations
// ======================
   
type ATContext = {ga: GlobalAssets; opts: ParsedOptions}

type AssemblyTransformationMessageKind = | ATError | ATWarning
    with
    override x.ToString() = match x with | ATError -> "Error" | ATWarning -> "Warning"

/// Domain type for errors encountered during transformation.
/// The type of assembly is left generic to permit re-use of this type during both
/// DNA materialization and transformation of DnaAssemblies.
type AssemblyTransformationMessage<'A when 'A :> ISourcePosition> =
    {msg: string;
     kind: AssemblyTransformationMessageKind;
     assembly: 'A;
     stackTrace: System.Diagnostics.StackTrace option;
     fromException: System.Exception option}
    with
    member x.Format(phase, ?sourceCode, ?verbose) =
        let verbose = defaultArg verbose false
        seq {
            match (x.assembly :> ISourcePosition).OptionalSourcePosition with
            | [] ->
                yield sprintf "%O during %s:" x.kind phase
                yield x.msg
            |  hd::tl -> 
                yield sprintf "%O during %s %s:" x.kind phase (AstTypes.formatSourcePositionList (hd::tl))
                yield x.msg
                yield "================================================================="
                match sourceCode with
                | Some(source) -> yield! hd.SourceContext(source)
                | None -> ()

            if verbose then
                yield sprintf "\n%+A" x.assembly
                match x.stackTrace with
                | Some(s) -> yield s.ToString()
                | None -> ()
        } |> String.concat "\n"

/// Convert an exception during assembly transformation into a message.
let exceptionToAssemblyMessage assembly (exc: System.Exception) =
    {msg = exc.Message;
     kind = ATError;
     assembly = assembly;
     stackTrace = Some(System.Diagnostics.StackTrace(exc));
     fromException = Some exc}

/// Interface specification for output assembly transformations.
type IAssemblyTransform =
    inherit IConfigurable<IAssemblyTransform>
    /// Perform a transformation of an assembly.
    abstract member TransformAssembly :
        ATContext -> DnaAssembly -> Result<DnaAssembly, AssemblyTransformationMessage<DnaAssembly>>

// =======================
// plugin behavior defintion for output file generation
// =======================

type OutputGenerationData =
    {ga: GlobalAssets;
     opts: ParsedOptions;
     assemblies: DnaAssembly list;
     primers: DivergedPrimerPair list list option}

/// Interface specification for output file format providers.
type IOutputFormat =
    inherit IConfigurable<IOutputFormat>
    /// Possibly produce output if this provider has been configured to run.
    abstract member ProduceOutput : OutputGenerationData -> unit

/// Each GSL plugin can provide customized behaviors from this list.
/// This type ensures that individual plugins will not have to update their source code
/// as additional pluggable behaviors are added, as they will simply appear as new options
/// in this type.
type PluginBehavior =
    | AlleleSwapAA of AlleleSwapProvider
    | L2KOTitration of L2Provider
    | OutputFormat of IOutputFormat
    | AssemblyTransform of IAssemblyTransform
    | PartProvider of IPartProvider
    | CodonProvider of ICodonProvider
    | MarkerProvider of IMarkerProvider
    with
    member b.ProvidedArgs() =
        match b with
        | OutputFormat(f) -> f.ProvidedArgs()
        | AssemblyTransform(a) -> a.ProvidedArgs()
        | PartProvider(p) -> p.ProvidedArgs()
        | CodonProvider(c) -> c.ProvidedArgs()
        | _ -> []

/// Wrapper around behavior to allow giving individual behaviors names and descriptions.
type PluginBehaviorWrapper =
   {name: string option;
    description: string option;
    behavior: PluginBehavior}
    with
    /// Provide a sequence of strings describing this behavior.
    member x.Info =
        seq {
            match x.name with | Some(n) -> yield sprintf "Name: %s" n | None -> ()
            match x.description with | Some(d) -> yield sprintf "Description: %s" d | None -> ()
            yield sprintf "Type: %s" (GetUnionCaseName x.behavior)}
        |> String.concat "\n"

let configureBehavior arg b =
    match b.behavior with
    | OutputFormat(f) -> {b with behavior = OutputFormat(f.Configure(arg))}
    | AssemblyTransform(a) -> {b with behavior = AssemblyTransform(a.Configure(arg))}
    | PartProvider(p) -> {b with behavior = PartProvider(p.Configure(arg))}
    | CodonProvider(c) -> {b with behavior = CodonProvider(c.Configure(arg))}
    | MarkerProvider(m) -> {b with behavior = MarkerProvider(m.Configure(arg))}
    | AlleleSwapAA _
    | L2KOTitration _ -> b

let configureBehaviorFromOpts opts b =
    match b.behavior with
    | OutputFormat(f) -> {b with behavior = OutputFormat(f.ConfigureFromOptions(opts))}
    | AssemblyTransform(a) -> {b with behavior = AssemblyTransform(a.ConfigureFromOptions(opts))}
    | PartProvider(p) -> {b with behavior = PartProvider(p.ConfigureFromOptions(opts))}
    | CodonProvider(c) -> {b with behavior = CodonProvider(c.ConfigureFromOptions(opts))}
    | MarkerProvider(c) -> {b with behavior = MarkerProvider(c.ConfigureFromOptions(opts))}
    | AlleleSwapAA _
    | L2KOTitration _ -> b

/// Data structure specifying one or more behaviors
type Plugin =
   {/// short name
    name: string;
    /// longer description
    description: string option;
    /// behaviors provided by this plugin
    behaviors: PluginBehaviorWrapper list;
    /// new pragmas provided by this plugin
    providesPragmas: pragmaTypes.PragmaDef list;
    /// new capabilities enabled by this plugin
    providesCapas: string list}
    with
    /// Return specs for any command line args this plugin provides.
    member x.ProvidedArgs() =
        x.behaviors
        |> List.map 
            (fun b -> b.behavior.ProvidedArgs())
        |> List.concat
    /// Given parsed command line args, update any behaviors that need them, returning a configured
    /// plugin.
    member x.Configure(args, opts) =
        let configuredBehaviors =
            args
            |> List.fold // each iteration of fold uses one arg and updates all behaviors
                (fun behaviors arg ->
                    behaviors
                    |> List.map (configureBehavior arg))
                x.behaviors
            |> List.map (configureBehaviorFromOpts opts)
        {x with behaviors = configuredBehaviors}
    /// Provide a extended description of this plugin and capabilities it provides.
    member x.Info =
        let indent (s: string) =
            s.Split('\n')
            |> Array.map (sprintf "    %s")
            |> String.concat "\n"

        let args = x.ProvidedArgs()

        seq {
            yield sprintf "Name: %s" x.name
            match x.description with | Some(d) -> yield sprintf "Description:\n    %s" d | None -> ()
            if not (x.behaviors.IsEmpty) then
                yield "Behaviors:"
                yield
                    x.behaviors
                    |> List.map (fun b -> indent b.Info)
                    |> String.concat "\n\n"
            if not (x.providesPragmas.IsEmpty) then
                yield "Provides pragmas:"
                for p in x.providesPragmas -> indent (formatPragma p)
            if not (x.providesCapas.IsEmpty) then
                yield "Provides capas:"
                for c in x.providesCapas -> indent c
            if not (args.IsEmpty) then
                yield "Provides command line arguments:"
                for a in args do yield! (printCmdLineArg a)
            
        } |> String.concat "\n"

/// Get all of the marker providers from a plugin.
let getMarkerProviders (plugin: Plugin) =
    plugin.behaviors
    |> List.choose (fun b -> match b.behavior with | MarkerProvider(m) -> Some(m) | _ -> None)


/// Get all of the allele swap providers from a plugin.
let getAlleleSwapAAProviders (plugin: Plugin) =
    plugin.behaviors
    |> List.choose (fun b -> match b.behavior with | AlleleSwapAA(a) -> Some(a) | _ -> None)

let getL2KOTitrationProviders (plugin: Plugin) =
    plugin.behaviors
    |> List.choose (fun b -> match b.behavior with | L2KOTitration(a) -> Some(a) | _ -> None)

let getAssemblyTransformers (plugin: Plugin) =
    plugin.behaviors
    |> List.choose (fun b -> match b.behavior with | AssemblyTransform(a) -> Some(a.TransformAssembly) | _ -> None)

let getPartProviders (plugin: Plugin) =
    plugin.behaviors
    |> List.choose (fun b -> match b.behavior with | PartProvider(a) -> Some(a) | _ -> None)

let getOutputProviders (plugin: Plugin) =
    plugin.behaviors
    |> List.choose (fun b -> match b.behavior with | OutputFormat(a) -> Some(a) | _ -> None)

let getCodonProviders (plugin: Plugin) =
    plugin.behaviors
    |> List.choose (fun b -> match b.behavior with | CodonProvider(a) -> Some(a) | _ -> None)

/// Use a provider extraction function to get every provider from a list of plugins.
let getAllProviders mode plugins =
    plugins
    |> List.map mode
    |> List.concat




