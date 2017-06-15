/// Define AST validations that provide warnings and errors about the structure and content of code.
/// Useful for easing language transitions, these should be updated and removed as the language and
/// compiler abilities change.
module AstLinting

open AstTypes
open AstAlgorithms
open AstErrorHandling
open Amyris.ErrorHandling
open System.Text.RegularExpressions

let private rabitPartRegex = Regex("R\d+")

let private warnOnPartThatIsLikelyVariable node =
    match node with
    | PartId(pw) ->
        if rabitPartRegex.IsMatch(pw.x) then good
        else
            let msgText =
                sprintf
                    "The syntax for using a variable has changed to &myVar from @myVar.\n@%s looks like it should probably be &%s."
                    pw.x pw.x
            let warnMsg = warningMessage msgText node
            warn warnMsg ()
    | _ -> good

let private failOnPushAndPop node =
    match node with
    | ParsePragma(pp) ->
        if pp.x.name = "push" || pp.x.name = "pop" then
            error PragmaError "#push and #pop have been removed from GSL.  Please port your code to use do/end blocks." node
        else
            good
    | _ -> good

let private failOnAssemblyInL2Promoter node = 
    match node with
    |L2Element(e) ->
        // if you see an L2 element, check if the promoter looks like an Assembly
        match e.x.promoter with
        | Assembly(_) -> error L2ExpansionError "Unsupported use of an Assembly in an L2 promoter element. Please use only native or rabit parts." node
        | _ -> good
    | _ -> good

    

let private allLinters =
    warnOnPartThatIsLikelyVariable
    &&& failOnPushAndPop
    &&& failOnAssemblyInL2Promoter

/// Perform all linting passes on an AST.
let linters = validate allLinters
