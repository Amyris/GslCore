/// IO routines for loading the reference file format
module RefGenome
open Amyris.Bio
open Amyris.Bio.sgd
open Amyris.Bio.utils
open Amyris.Bio.SuffixTree
open utils
open constants
open pragmaTypes
open Amyris.ErrorHandling
open Amyris.Dna
(*
$ ls c\:/Amyris/data/REF/cenpk/cenpk
cenpk.fsa           cenpk_features.tab

*)
open System.IO
open System

let loadEnv (p:string) =
    if File.Exists(p) then
        eachLineIn p |> Seq.map (fun x -> x.Split([|"="|],StringSplitOptions.None))
                     |> Seq.choose (fun cols -> match cols with 
                                                    | [| a; b |] -> Some(a.Trim(),b.Trim())
                                                    | _ -> printf "WARNING: bad config entry '%A'" cols ; None
                                    )
                    |> Map.ofSeq
    else
        Map.empty

/// Init genome definition, genes etc
type GenomeDef(libDir: string, name: string) as this = class
    let mutable fasta = None
    let mutable feats = None
    let mutable featIndex = None
    let mutable suffixTreePath : string option = None
    let mutable suffixTree : SuffixTreeDisk option = None

    let mutable env : Map<string,string> = Map.empty
    let mutable envLoaded = false
    let refDir = opj libDir name

    let ensureConfigLoaded() =
        if not envLoaded then
            env <- opj refDir "config.txt" |> loadEnv
            envLoaded <- true

    let ensureLoaded() =
        match fasta with
            | None -> this.Load()
            | _ -> ()

    member x.Name = name
    member x.Env = 
        ensureConfigLoaded()
        env        

    member x.EnvLenLookup name defaultValue =
        if x.Env.ContainsKey(name) then
            (x.Env.[name] |> int ) * 1<OneOffset>
        else 1<OneOffset> * defaultValue

    member x.Load() =
        ensureConfigLoaded()
        let featsPath = opj refDir (sprintf "%s_features.tab" name)
        let fastaPath = opj refDir (sprintf "%s.fsa" name)
        suffixTreePath <- Some(opj refDir "suffixTree.st")
        
        fasta <- (let d = System.Collections.Generic.Dictionary<_,_>()
                  for kv in Amyris.Bio.biolib.readReference fastaPath do
                        d.Add(kv.Key,(Dna(kv.Value, true, AllowAmbiguousBases)))
                  Some d
                  )
        feats <-  Some(sgd.loadFeatures featsPath)
        
        let i1 = feats.Value |> Array.mapi (fun i f -> f.sysName,i)
        let i2 = feats.Value |> Array.mapi (fun i f -> f.gene,i)
        featIndex <- Array.concat [ i1 ; i2 ] |> Seq.filter (fun (x,_) -> x <> "") |> Map.ofSeq |> Some 
    
    member x.get(g:string) =
        ensureLoaded()
        match featIndex with
            | None -> failwith "Access to unloaded GenomeDef"
            | Some(fi) -> feats.Value.[fi.[g]]

    /// Return a list of codons to avoid, if this reference genome has them defined.
    member x.GetCodonAvoid() =
        match x.Env.TryFind("codonavoid") with
        | None -> []
        | Some(x) ->
            x.Split([| ' ' ; '\t' |])
            |> List.ofArray
            |> List.map (fun s -> s.Replace('U','T'))
            |> List.map Dna
    
    /// Return array of all genomic features we have loaded
    member x.GetAllFeats() =
        ensureLoaded()
        let distinctIndices = [ for kv in featIndex.Value -> kv.Value ] |> List.distinct
        [| for i in distinctIndices -> feats.Value.[i] |]

    member x.Dna(errorContext:string,chr:string,l':int<ZeroOffset>,r':int<ZeroOffset>) =
            ensureLoaded()
            let l,r = l'/1<ZeroOffset> , r'/1<ZeroOffset>
            if not (r>=l) then
                failwithf "ERROR: For %s Attempt to retrieve  DNA slice with reversed coordinates %s:%d-%d\n" errorContext chr l r
                
            match fasta with
                | None -> failwithf "For %s Access to unloaded GenomeDef Fasta" errorContext
                | Some(f) ->
                    if not (f.ContainsKey(chr)  ) then failwithf "ERROR: For %s unknown chromsome '%s'" errorContext chr
                    if l < 0 || l >= f.[chr].Length then failwithf "ERROR: For %s coordinate '%d' outside chromsome %s length = %d" errorContext l chr (f.[chr].Length)
                    if r < 0 || r >= f.[chr].Length then failwithf "ERROR: For %s coordinate '%d' outside chromsome %s length = %d" errorContext r chr (f.[chr].Length)
                    f.[chr].[l..r]
                    
    member x.SuffixTree =
                    ensureLoaded()
                    match suffixTree with 
                        | Some(st) -> st 
                        | None -> if (File.Exists(suffixTreePath.Value)) then 
                                        //FIXIFX: this next line occasionally throws an 'Cannot create a file when that file already exist error
                                        (*Cannot create a file when that file already exists.

ERROR: location:
   at System.IO.__Error.WinIOError(Int32 errorCode, String maybeFullPath)
   at System.IO.MemoryMappedFiles.MemoryMappedFile.CreateCore(SafeFileHandle fileHandle, String mapName, HandleInheritability inheritability, MemoryMappedFileSecurity memoryMappedFileSecurity, MemoryMappedFileAccess access, MemoryMappedFileOptions options, Int64 capacity)
   at System.IO.MemoryMappedFiles.MemoryMappedFile.CreateFromFile(String path, FileMode mode, String mapName, Int64 capacity, MemoryMappedFileAccess access)
   at System.IO.MemoryMappedFiles.MemoryMappedFile.CreateFromFile(String path, FileMode mode, String mapName, Int64 capacity)
   at Amyris.SuffixTree.SuffixTreeDisk.openMM(String path)
   at Amyris.SuffixTree.SuffixTreeDisk..ctor(String path)
   at sgdrefformat.GenomeDef.get_SuffixTree() in C:\seq\GSL\gslc\sgdrefformat.fs:line 99

                                        *)
                                        suffixTree <- Some(new SuffixTreeDisk(suffixTreePath.Value))  
                                        suffixTree.Value
                                  else failwithf "ERROR: loading suffix tree, unable to find file %s" suffixTreePath.Value           
    member x.IsValid(f:string) =
        ensureLoaded()
        
        match featIndex with
            | None -> failwith "Uninitialized GenomeDef instance\n"
            | Some(fi) -> fi.ContainsKey(f)   
    interface System.IDisposable with
        member x.Dispose() = match suffixTree with | None -> () | Some(std) -> (std :> System.IDisposable).Dispose()

    /// default or custom length for flanking regions
    member x.getFlank() = x.EnvLenLookup "flanklen" flanklenDefault
    /// default or custom length for stand alone terminator pieces
    member x.getTermLen() = x.EnvLenLookup "termlen" termLenDefault
    /// default or custom length for terminator part of an mRNA type part
    member x.getTermLenMRNA() = x.EnvLenLookup "termlenmrna" termLenMRNADefault
    /// default or custom length for promoter
    member x.getPromLen() = x.EnvLenLookup "promlen" promLenDefault

                 
end

type GenomeDefs = Map<string,GenomeDef>

/// Get a reference genome from an ordered set of pragma collections.
let getRGNew (rgs: GenomeDefs) (prags: PragmaCollection list) =
    let rgName = 
        match prags |> List.tryPick (fun pr -> pr.TryGetOne("refgenome")) with
        | Some(name) -> name
        | None -> defaultRefGenome
    match rgs.TryFind(rgName) with
    | Some(g) -> ok g
    | None when rgName = defaultRefGenome ->
        fail (sprintf "ERROR: unable to load default genome '%s' <currently loaded: %s>"
            defaultRefGenome
            (if rgs.Count=0 then "none"
             else String.Join(",", [ for k in rgs -> k.Key])))
    | _ ->
        fail (sprintf "ERROR: no such refgenome '%s', options are\n%s"
            rgName
            (String.Join("\n",seq { for k in rgs -> sprintf "    '%s'" k.Key })))


let refGenomeWarning() = 
    "" // DISABLED FOR NOW  ..  // sprintf "Warning, defaulting to %s codon usage, no #refgenome specified\n" defaultRefGenome

/// If a ref genome is specified in pragmas, return it.
/// Otherwise return the default ref genome.
// FIXME: warning should be swallowed up by ROP rather than printed.
let chooseRefGenome (p:PragmaCollection) =
    match p.TryGetOne("refgenome") with
    | Some(rg) -> rg
    | None ->
        printf "%s" (refGenomeWarning())
        defaultRefGenome  // Warning - defualts to yeast codon usage
