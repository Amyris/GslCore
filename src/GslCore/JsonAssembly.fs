module JsonAssembly

open System.IO
open System.Text
open System
open commonTypes
open constants
open Amyris.Bio.utils
open Newtonsoft.Json
open System.Collections.Generic
open PluginTypes
open CoreOutputProviders

/// A few key fields of a DNA slice for JSON representation.
type DNASliceJson =
   {id: string; 
    extId: string; 
    dna: string;  
    sourceChr: string; 
    sourceFr: string; 
    sourceTo: string; 
    sourceFwd: bool;
    destFr: string; 
    destTo: string;
    destFwd: bool; 
    amplified: bool; 
    sliceName: string;
    sliceType: string;
    breed: string;
    description: string ; 
}

/// A few key fields of Assembly output for JSON representation.
type AssemblyOutJson = 
    {id: string;
    name: string;
    dnaSlices: DNASliceJson list;
}

let formatST (s:SliceType) = 
    match s with 
    | REGULAR -> "REGULAR"
    | MARKER -> "MARKER"
    | LINKER -> "LINKER"
    | INLINEST -> "INLINE"
    | FUSIONST ->"FUSION"

let formatBreed (b:Breed) = 
    match b with 
    | B_PROMOTER -> "B_PROMOTER"
    | B_TERMINATOR -> "B_TERMINATOR"
    | B_MARKER -> "B_MARKER"
    | B_FUSABLEORF -> "B_FUSABLEORF"
    | B_UPSTREAM -> "B_UPSTREAM"
    | B_DOWNSTREAM -> "B_DOWNSTREAM"
    | B_GST -> "B_GST"
    | G_M -> "G_M"
    | G_STOP -> "G_STOP"
    | B_GS -> "B_GS"
    | B_INLINE -> "B_INLINE"
    | B_X -> "B_X"
    | B_VIRTUAL -> "B_VIRTUAL"
    | B_LINKER -> "B_LINKER" 

///  Write out a JSON file representing the output assembly list to a given path. 
let dumpJsonAssemblies (outFile:string) (assemblies : DnaAssembly list) =

    use outF = new StreamWriter(outFile)
    let assemblyHash =
        assemblies 
        |> List.map(fun a ->
            {id = a.id.Value.ToString(); name = a.name.ToString();
            dnaSlices = (a.dnaParts 
                |> List.map(fun d ->
                    {id = (match d.id with | None -> "0" | Some id -> id.ToString());
                    extId = "";
                    dna = String.Join("", d.dna);
                    sourceChr = d.sourceChr;
                    sourceFr = d.sourceFr.ToString();
                    sourceTo = d.sourceTo.ToString();
                    sourceFwd = d.sourceFwd;
                    destFr = d.destFr.ToString();
                    destTo = d.destTo.ToString();
                    destFwd = d.destFwd;
                    amplified = d.amplified;
                    sliceName = d.sliceName.ToString();
                    sliceType = (formatST d.sliceType);
                    breed = (formatBreed d.breed);
                    description = d.description.ToString()})
                );
            })
    
    let jsonFileString:string = Newtonsoft.Json.JsonConvert.SerializeObject(assemblyHash, Formatting.Indented)
    outF.WriteLine(jsonFileString)

type AutodeskJsonOutputProvider (outPath: (string) option) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
    override x.ArgSpec =
        {name = "json"; param = ["outfile"]; alias = [];
         desc = "write a json file format for the assembly results"}
    override x.UseArg(arg) =
        AutodeskJsonOutputProvider(Some(arg.values.[0]))
        :> IOutputFormat
    override x.DoOutput(path, data) = dumpJsonAssemblies path data.assemblies

let autodeskJsonOutputPlugin =
    outputPlugin
        "autodesk_json"
        (Some "Autodesk json output file format provider, for connecting to the Autodesk Genetic Constructor.")
        (AutodeskJsonOutputProvider(None))
