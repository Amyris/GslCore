module DesignParams
open System
open Amyris.Bio.primercore
open System.Text.RegularExpressions
open Amyris.ErrorHandling
open pragmaTypes
open constants
open PcrParamParse

///<summary>
/// Given an initial set of PrimerParams, return a new set based on parsing arguments
/// passed to a #pcrparams pragma.
let revisePP (p: PrimerParams) (arguments: string list) =
    arguments
    |> List.fold
        (fun pp a -> pp >>= (parseArgUpdatePP a))
        (ok p)

type DesignParams =
   {targetTm: float<C>;
    seamlessTm: float<C>; 
    seamlessOverlapTm: float<C>;
    pp: PrimerParams;
    overlapParams: PrimerParams; 
    overlapMinLen: int}

/// Starting design parameters for construction
let initialDesignParams =
   {pp = defaultParams;
    targetTm = ryseLinkerTargetDefault;
    seamlessTm = seamlessTargetDefault;
    seamlessOverlapTm = seamlessTargetDefault;
    overlapParams = defaultParams;
    overlapMinLen = overlapMinLenDefault}

let updateDPFromPragma (p: Pragma) designParams =
    match p.name, p.args with
    | "pcrparams", args ->
        revisePP designParams.pp args
        >>= (fun pp -> ok {designParams with pp = pp})
    | "pcrassemblyparams", args ->
        revisePP designParams.overlapParams args
        >>= (fun overlapParams -> ok {designParams with overlapParams = overlapParams})
    | "targettm", v::_ -> ok {designParams with targetTm = strToTempC v}
    | "minoverlaplen", v::_ -> ok {designParams with overlapMinLen = int v}
    | "seamlesstm", v::_ -> ok {designParams with seamlessTm = strToTempC v}
    | "seamlessoverlaptm", v::_ -> ok {designParams with seamlessOverlapTm = strToTempC v}
    | "atpenalty", v::_ ->
        ok {designParams with pp = {designParams.pp with ATPenalty = float v * 1.0<C>}}
    | _ -> ok designParams

/// Given a pragma environment, compute assembly physical design parameters from an existing set.
let designParamsFromPragmas dp (pc: PragmaCollection) =

    pc.Values
    |> Seq.fold
        (fun dp p -> dp >>= (updateDPFromPragma p))
        (ok dp)
