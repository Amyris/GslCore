/// Support for reusable part retrieval based on a FASTA file.
module LibraryPartProvider
open System.IO
open commonTypes
open PluginTypes
open Amyris.Dna
open Amyris.ErrorHandling

type LibraryPartProvider() =
    let mutable library: SequenceLibrary = Map.empty
    do
        ()
    with
    interface IPartProvider with
        member __.ProvidedArgs() = []
        member x.Configure(_) = x :> IPartProvider
        /// Load the sequence library from the GSLC lib directory, if it exists.
        member x.ConfigureFromOptions(opts) =
            let libFile = Path.Combine(opts.libDir, "lib.fa")
            if File.Exists libFile then
                let lib = 
                    Amyris.Bio.biolib.readReference libFile
                    |> Seq.map (fun kv -> (kv.Key.ToUpper(), Dna(kv.Value) ))
                    |> Map.ofSeq
                library <- lib

            x :> IPartProvider
        member x.Name = "library"
        member x.Accept(partId) = library |> Map.containsKey partId
        member x.Retrieve(partId) = ok {name = partId; dna = library.[partId]; linkers = None}
