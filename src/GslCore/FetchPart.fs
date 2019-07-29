/// Plugin-based retrieval of parts from external catalogs.
module FetchPart

open PluginTypes
open Amyris.ErrorHandling

// In an ideal world, this collection of plugins would be injected from the top, but at the time of this writing the
// compiler uses part retrieval functions in a large number of places, requiring a frustrating quantity of refactoring.
// Thus, this module-bound global structure.

let mutable private partProviders: IPartProvider array = Array.empty

/// Set the available part providers.
/// This should be called once at program initialization.
let setPartProviders providers = partProviders <- providers

/// Use the available part providers to try to fetch a part based on its ID.
let fetchPart partId =
    match partProviders |> Array.filter (fun p -> p.Accept(partId)) with
    | [||] -> fail <| sprintf "No external part provider found for part ID \"%s\"." partId
    | [|provider|] -> provider.Retrieve(partId)
    | tooMany ->
        let names = tooMany |> Array.map (fun p -> p.Name) |> String.concat ", "
        fail <| sprintf "More than one part provider service found for part ID \"%s\": %s" partId names