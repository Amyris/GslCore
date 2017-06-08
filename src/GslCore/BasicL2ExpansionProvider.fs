module BasicL2ExpansionProvider

///
/// Implementation of GSL Level 2 Expression Lines
/// Modelled roughly on roughage syntax  e.g. gHO^ ; a> b ; c>d etc
///
open pragmaTypes
open LegacyParseTypes
open AstTypes
open AstAlgorithms
open AstProcess
open commonTypes
open Amyris.Bio
open constants
open RefGenome
open System.Text
open System
open PluginTypes

/// Take a list of expression elements and organize them in a balanced
/// way - e.g. splitting between two halves of a megastitch
let balance (elems: BuiltL2Element list) =
    let roundUp (x:float) =
        if x - (x|> int |> float) > Double.Epsilon then ((int x) + 1) else int x
    // Dumb implementation, doesn't handle bipromoters
    // or any clever avoidance of repeats etc, part reuse
    let countA = float(elems.Length) / 2.0 |> roundUp
    let partsA = Seq.take countA elems |> List.ofSeq
    let partsB = Seq.skip countA elems |> List.ofSeq
    partsA,partsB

/// Base implementation for level 2 knock out / promoter titration
/// give it lowest score in case someone has a preferred implementation
let l2JobScorer _ = Some 0.0<PluginScore>

/// Takes a level-2 line regarding explicit locus and returns a list.
///
/// E.g. transforms a level-2 line, gHO^ ; pA > gB ; pC > gD into
/// {"uHO"; "pA" ; "gB" ; "###" ; "!gD ; !pA" ; "dHO"}

let generateOutputsExplicitLocus (locus:L2Id) (args: L2DesignParams) =

    let locusWithPrefix = locus.id.x
    assert locus.prefix.IsNone

    let locusWithoutPrefix = locusWithPrefix.Substring(1)
    if not (locusWithPrefix.ToUpper().StartsWith("G")) then
        failwithf "ERROR: knockout target gene %s must start with g tag (e.g. gADH1)." locusWithPrefix
    let out = seq {
                    let partsA,partsB = balance args.line.parts

                    // Emit replacement DNA name for knockout
                    let replacementName =
                        match args.pragmas.TryFind(namePragmaDef) with
                        // if the user provided a pragma name, create the donor DNA name with .donor
                        // (we're not just using the name itself because it has to be distinguished
                        // from the gRNAs which will also be named a variant of the user provided name
                        | Some(p) -> 
                            let providedName = p.args.[0] // get the first argument to the name pragma
                            sprintf "#name %s" providedName

                        // if no name is provided, use this as the default donor name
                        | None -> sprintf "#name u%s__d%s" locusWithoutPrefix locusWithoutPrefix

                    yield replacementName
                    
                    // Emit upstream flanking region
                    yield sprintf "u%s" locusWithoutPrefix
                    // First half of the parts before the marker
                    for expItem in partsA do
                        yield decompile expItem.promoter
                        yield sprintf "%s" (expItem.target.String)
                    if args.megastitch then yield "###" // Marker
                    // Second half of the parts after the marker
                    for expItem in partsB do
                        yield (sprintf "!%s;!(%s)" expItem.target.String (decompile expItem.promoter))
                    // Emit downstream flanking region
                    yield sprintf "d%s" locusWithoutPrefix
                } |> List.ofSeq

    // results come back as a list of strings but we need to treat first name as a separate line and ; concat remainder
    match out with
        | name::rest ->
            let partsList = String.Join(";" , rest)
            [ name; partsList ]
        | _ -> failwithf "ERROR: L2 parsing failed"
    |> String.concat "\n"
    |> GslSourceCode


/// Takes a level-2 line regarding promoter titrations and returns a list.
///
/// E.g. transforms a level-2 line, pA>gB ; pc>gD into
/// {"uB"; "pC" ; "gD" ; "###" ; "pA" ; "gB[1:~500]"}
let generateOutputsTitrations (args: L2DesignParams) =

    // separates the expression pGene>gGene from the rest of the line
    let locusExp,otherExp =
        match args.line.parts with
            | [] -> failwithf "ERROR: unexpected empty L2 expression construct with no locus or parts\n"
            | hd::tl -> hd,tl
    /// the titrated gene
    let locusGene = locusExp.target.id.x.Substring(1) 
    if not (locusExp.target.id.x.ToUpper().StartsWith("G")) then
        failwithf "ERROR: titrating expression target %s must start with g tag (e.g. gADH1). Variables not supported for titrations." locusExp.target.String
    let partsA,partsB = balance otherExp
    /// the flank length
    let flank = args.rgs.[args.refGenome].getFlank()
    let out = seq{
                    
                    // Emit replacement DNA name for promoter swap
                    let replacementName =
                        match args.pragmas.TryFind(namePragmaDef) with
                        // if the user provided a pragma name, create the donor DNA name with .donor
                        // (we're not just using the name itself because it has to be distinguished
                        // from the gRNAs which will also be named a variant of the user provided name
                        | Some(p) -> 
                            let providedName = p.args.[0] // get the first argument to the name pragma
                            sprintf "#name %s" providedName

                        // if no name is provided, use this as the default donor name
                        | None -> sprintf "#name u%s_%s_d%s" locusGene (decompile locusExp.promoter |> cleanHashName ) locusGene
                    
                    // yield a new linker line because the default pattern will cause an A linker 
                    // to land on a marker (error: no A-9 markers)
                    yield "#linkers 0,2,A,3,9|0,A,3,9"
                    yield replacementName
                    // Yield upstream flanking region. 
                    yield (  sprintf "u%s" locusGene) // regular locus flanking seq
                    // First half of the parts before the marker
                    for expItem in partsA do
                        yield decompile expItem.promoter
                        yield expItem.target.String
                    if args.megastitch then yield "###" 
                    // Second half of the parts after the marker
                    for expItem in partsB do
                        yield (sprintf "!%s;!(%s)" expItem.target.String (decompile expItem.promoter))
                    // Finally the titrating promoter
                    yield decompile locusExp.promoter
                    // Emit downstream flanking region
                    yield sprintf "%s[1:~%A] {#breed DS_CDS}" locusExp.target.String flank
                }  |> List.ofSeq
    match out with
    | linkers::name::rest -> [linkers; name; String.Join(";" , rest)]
    | _ -> failwithf "ERROR: L2 parsing failed"
    |> String.concat "\n"
    |> GslSourceCode

let basicL2ExpansionPlugin =
   {name = "basic_L2_provider";
    description = Some "Basic implemention of L2 promoter titration.";
    behaviors =
       [{name = None;
         description = None;
         behavior =
            L2KOTitration(
               {jobScorer = l2JobScorer ;
                explicitLocusProvider = generateOutputsExplicitLocus
                implicitLocusProvider = generateOutputsTitrations})}
       ];
    providesPragmas = []
    providesCapas = []}