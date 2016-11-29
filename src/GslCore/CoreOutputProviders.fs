module CoreOutputProviders
open Amyris.Bio
open PluginTypes
open commandConfig
open cloneManager
open ape
open dumpFlat
open commonTypes
open System.IO

// =============================
// standard output format providers
// =============================

/// Basic output provider, switched using a single command line parameter.
/// Boilerplate removal base class for basic, configurationless output generation.
[<AbstractClass>]
type ConfigurableOutputProvider<'T> (param: 'T option) =
    abstract member ArgSpec : CmdLineArgSpec
    abstract member UseArg : ParsedCmdLineArg -> IOutputFormat
    abstract member DoOutput : 'T * OutputGenerationData -> unit
    interface IOutputFormat with
        member x.ProvidedArgs() = [x.ArgSpec]
        member x.Configure(parsedArg) =
            if parsedArg.spec = x.ArgSpec then
                x.UseArg(parsedArg)
            else x :> IOutputFormat
        member x.ProduceOutput(data) =
            match param with
            | Some(p) -> x.DoOutput(p, data)
            | None -> ()

/// Create a basic plug and play output plugin.
let outputPlugin name desc provider =
   {name = name;
    description = desc;
    behaviors =
       [{name = None; description = None; behavior = OutputFormat(provider)}];
    providesPragmas = [];
    providesCapas = []}

type GslFlatFileOutputProvider (outPath) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
    override x.ArgSpec =
        {name = "flat"; param = ["outfile"]; alias = [];
         desc = "write a flat file format for results to outputfile"}
    override x.UseArg(arg) =
        GslFlatFileOutputProvider(Some(arg.values.[0]))
        :> IOutputFormat
    override x.DoOutput(path, data) = dumpFlat path data.assemblies

let flatFileOutputPlugin =
    outputPlugin
        "flat_file"
        (Some "GSL flat file output provider.")
        (GslFlatFileOutputProvider(None))


type CloneManagerOutputProvider (outParams) =
    inherit ConfigurableOutputProvider<(string*string)>(outParams)
    with
    override x.ArgSpec =
        {name = "cm"; param = ["outDir"; "prefix"]; alias = [];
         desc="write clone manager output to prefix_##.cx5 to output directory outDir"}
    override x.UseArg(arg) =
        CloneManagerOutputProvider(Some(arg.values.[0], arg.values.[1]))
        :> IOutputFormat
    override x.DoOutput((path,tag), data) = dumpCM path tag data.assemblies data.primers

let cloneManagerOutputPlugin =
    outputPlugin
        "clone_manager"
        (Some "Clone Manager output format provider.")
        (CloneManagerOutputProvider(None))

type ApeOutputProvider (outParams) =
    inherit ConfigurableOutputProvider<(string*string)>(outParams)
    with
    override x.ArgSpec =
        {name = "ape"; param = ["outDir"; "prefix"]; alias = [];
         desc = "write APE output to prefix_##.ape to outDir\n(http://biologylabs.utah.edu/jorgensen/wayned/ape/)"}
    override x.UseArg(arg) =
        ApeOutputProvider(Some(arg.values.[0], arg.values.[1]))
        :> IOutputFormat
    override x.DoOutput((path,tag), data) = dumpAPE path tag data.assemblies

let apeOutputPlugin =
    outputPlugin
        "APE"
        (Some "APE (A Plasmid Editor) output format provider.")
        (ApeOutputProvider(None))


 /// Create output file with user or algorithm documentation of the designs
let private dumpDocStrings (path:string) (assemblies:DnaAssembly list) =
    use outF = new StreamWriter(path)
    for a in assemblies do
        outF.WriteLine(sprintf "@name=%s" a.name)
        for line in a.docStrings do
            outF.WriteLine(line)
        outF.WriteLine("")

type DocstringOutputProvider (outPath) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
    override x.ArgSpec =
        {name = "docstring"; param = ["outfile"]; alias = ["docstrings"];
         desc = "log emitted documentation for each design to outfile"}
    override x.UseArg(arg) =
        DocstringOutputProvider(Some(arg.values.[0]))
        :> IOutputFormat
    override x.DoOutput(path, data) = dumpDocStrings path data.assemblies

let docstringOutputPlugin =
    outputPlugin
        "docstring_file"
        (Some "GSL docstring output format provider.  Enables dumping of assembly docstrings.")
        (DocstringOutputProvider(None))


type PrimerOutputProvider (outPath) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
    override x.ArgSpec =
        {name = "primers"; param = ["primerfile"]; alias = [];
         desc = "emit raw primer details (see also thumper output format)"}
    override x.UseArg(arg) =
        PrimerOutputProvider(Some(arg.values.[0]))
        :> IOutputFormat
    override x.DoOutput(path, data) =
        match data.primers with
        | Some(primers) -> primerDump.simplePrimerDump path primers data.assemblies
        | None -> failwithf "--primers was selected but no primers were produced.  Did you also pass --noprimers?"

let primerOutputPlugin =
    outputPlugin
        "primer_file"
        (Some "Primer description file format output provider.  Enables dumping of assembly primer data.")
        (PrimerOutputProvider(None))


let basicOutputPlugins = [
    flatFileOutputPlugin;
    cloneManagerOutputPlugin;
    apeOutputPlugin;
    docstringOutputPlugin;
    primerOutputPlugin;
]
