/// Plugin-based retrieval of parts from external catalogs.
module FetchPart

open PluginTypes
open Amyris.ErrorHandling
open Amyris.Dna

// In an ideal world, this collection of plugins would be injected from the top, but at the time of this writing the
// compiler uses part retrieval functions in a large number of places, requiring a frustrating quantity of refactoring.
// Thus, this module-bound global structure.

let mutable private partProviders: IPartProvider array = Array.empty

/// Set the available part providers.
/// This should be called once at program initialization.
let setPartProviders providers = partProviders <- providers

type ExternalPart = {
    /// Persistent identifier.
    id: string
    /// Human-readable name.
    name: string
    /// DNA sequence of this part.
    dna: Dna
    /// Optional RYSE linker specification (5' link code, 3' link code).
    linkers: (string*string) option
    /// Name of the part provider that provided this part.
    source: string
}

/// Use the available part providers to try to fetch a part based on its ID.
let fetchPart partId : Result<ExternalPart, string> =
    match partProviders |> Array.filter (fun p -> p.Accept(partId)) with
    | [||] -> fail <| sprintf "No external part provider found for part ID \"%s\"." partId
    | [|provider|] ->
        provider.Retrieve(partId)
        >>= (fun fetched ->
            ok {
                id = partId
                source = provider.Name
                name = fetched.name
                linkers = fetched.linkers
                dna = fetched.dna})
    | tooMany ->
        let names = tooMany |> Array.map (fun p -> p.Name) |> String.concat ", "
        fail <| sprintf "More than one part provider service found for part ID \"%s\": %s" partId names