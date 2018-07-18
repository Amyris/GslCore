/// Shared operations for compiling and extracting material from assemblies
module AssemblyTestSupport

open AstTypes
open AstAssertions
open AstExpansion
open constants
open Amyris.ErrorHandling

let rec extractAssemblies (n:AstNode) : AstNode list =
    [
        match n with
        | Block b -> 
            let result = b.x |> List.collect extractAssemblies
            yield! result
        | Splice s -> 
            let result = s |> List.ofArray |> List.collect extractAssemblies
            yield! result
        | Part p -> 
            match p.x.basePart with
            | Assembly a as x -> yield x
            | _ -> ()
        | Assembly a as x -> yield x
        | _ -> ()
    ]


/// compile one GSL source example and extract assemblies
let compileOne source =
    source 
    |> GslSourceCode
    |> compile (phase1 Set.empty) 
    |> returnOrFail
    |> fun x -> extractAssemblies x.wrappedNode
