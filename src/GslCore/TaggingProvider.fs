/// Assembly transforming plugin that implements seamless part assembly.
module TaggingPlugin

open System
open LegacyParseTypes
open commonTypes
open commandConfig
open pragmaTypes
open PluginTypes
open Amyris.ErrorHandling

let taggingArg = 
   {name = "tag";
    param = ["namespace:value"];
    alias = [];
    desc = "Add default tag to every assembly."
   }

let parseTag (single:string) state =
    match single.IndexOf(":") with
        | -1 -> fail (sprintf "--tag value %s missing expected colon" single)
        | colonPosition ->
            ok  ({  nameSpace=single.[..colonPosition-1].Trim()
                    tag=single.[colonPosition+1..].Trim()
                }::state)

let parseTags (args:string list) =
    args |> 
        List.fold (
            fun (state:Result<_,_>) (arg:string) -> 
                state >>= (parseTag arg)
        ) (ok [])

/// do a trial parse and return ok unit if successful
let validateTag args =
    parseTags args 
    >>= (fun _ -> ok ())

let tagPragmaDef =
    { name = "tag"
      argShape = AtLeast 1
      scope = BlockOnly(TransientCumulative)
      desc = "tag assemblies with terms from a namespace."
      invertsTo = None
      validate = validateTag } 

let gTagPragmaDef =
    { name = "gtag"
      argShape = AtLeast 1
      scope = BlockOnly(PersistentCumulative);
      desc = "global tag assemblies with tags from a namespace."
      invertsTo = None
      validate = validateTag }  

/// Take previous #tag namespace:tagvalue  lines and fold into the assembly structure
let foldInTags (cmdlineTags : AssemblyTag list) (_at : ATContext) (a : DnaAssembly) =
    // gtag is global tag, tag is dna assembly tag
    match List.collect (fun pragma -> pragma.args) ([ a.pragmas.TryFind("tag") ; a.pragmas.TryFind("gtag") ] |> List.choose id) with
    | [] ->
        let newTags = cmdlineTags |> Set.ofList |> Set.union a.tags
        ok { a with tags = newTags }
    | args ->
        match parseTags args with
        | Ok(newTags,_) ->
            let newTags = (cmdlineTags @ newTags) |> Set.ofList |> Set.union a.tags 
            ok { a with tags = newTags }
        | Bad msg -> fail {msg = String.Join(";",msg) ; kind = ATError ; assembly = a ; stackTrace = None ; fromException = None}
        
type TaggingProvider = {
    cmdlineTags:AssemblyTag list
    /// Optionally attach a function to this plugin behavior to permit its operation to be
    /// configured by command line arguments injected by other plugins.  This is necessary because
    /// seamless assembly can alter a lot of expectations of downstream processing steps.
    processExtraArgs: ParsedCmdLineArg -> TaggingProvider -> TaggingProvider}
    with
    interface IAssemblyTransform with
        member __.ProvidedArgs() = [taggingArg]
        member x.Configure(arg) =
            if arg.spec = taggingArg then
                match parseTags arg.values with
                | Ok(v,_) ->
                    {x with cmdlineTags = v@x.cmdlineTags}
                | Result.Bad messages ->
                    failwithf "%s" (String.Join("; ",messages))

            else x
            |> x.processExtraArgs arg
            :> IAssemblyTransform
        member x.ConfigureFromOptions(_opts) =
            x :> IAssemblyTransform
        member x.TransformAssembly context assembly =
            foldInTags x.cmdlineTags context assembly

/// Produce an instance of the seamless assembly plugin with the provided extra argument processor.
let createTaggingPlugin extraArgProcessor =
   {name = "assembly tagging support"
    description = Some "Allow tagging of assemblies with #tag namespace:tag"
    behaviors =
      [{name = None;
        description = None;
        behavior = AssemblyTransform({cmdlineTags = []; processExtraArgs = extraArgProcessor})}]
    providesPragmas = [tagPragmaDef ; gTagPragmaDef];
    providesCapas = []}

let taggingPlugin = createTaggingPlugin (fun _ x -> x)
