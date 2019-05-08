module pragmaTypes
open System
open Amyris.ErrorHandling
open utils

/// Accumulate named capabilities from #capa pragmas
type Capabilities = Set<string>

type PragmaArgShape =
    /// require zero args
    | Zero
    /// Require one arg
    | One
    /// Require exactly N args
    | Exactly of int
    /// Require at least N args
    | AtLeast of int
    /// Range is inclusive on both sides, so Range(1,5) accepts one to five parameters.
    | Range of int * int
    /// For the uncommon case where a pragma might accept on a set of exact
    /// numbers of arguments, possibly to dictate behavior.
    | ExactlySet of int list

type PragmaValidationResult = Result<unit,string>

type PragmaPersistence = | Persistent | Transient
///<summary>
/// Pragmas are scoped within a GSL document.  Some pragmas
/// are somewhat "scope-polymorphic" and have different
/// behavior depending on which scope they appear in.
/// This type indicates whether a pragma is allowed at the block level.
/// If it is, is specifies if the pragma is persistent or transient (only applies
/// to the next assembly).
/// It also specifies if the pragma is allowed in a part.
/// Part-level pragmas are intrinsically transient.
///</summary>
type PragmaScope = 
    | PartOnly
    | BlockOnly of PragmaPersistence
    | BlockOrPart of PragmaPersistence
    member x.ToString =
        match x with
        | BlockOrPart(b) ->
            sprintf "Block (%s); Part" (GetUnionCaseName b)
        | BlockOnly(b) ->
            sprintf "Block (%s)" (GetUnionCaseName b)
        | PartOnly -> "Part"

///<summary>
/// Formal declaration of a pragma.
/// A pragma is fully specified by its name and the shape of the arguments it accepts.
/// A validation function may be optionally provided to fail fast during parsing
/// rather than when the pragma is used.
///</summary>
[<CustomEquality;NoComparison>]
type PragmaDef = {
    name: string;
    argShape: PragmaArgShape;
    desc: string;
    scope: PragmaScope;
    invertsTo: string option;
    validate: (string list -> PragmaValidationResult);}
    with
    /// Since we always check for duplicate pragma defs, comparing names is sufficient for equality.
    override x.Equals(other) =
        match other with
        | :? PragmaDef as y -> x.name = y.name
        | _ -> false
    /// Hash pragma defs just by their name.
    override x.GetHashCode() = hash x.name

// Helper functions for generic validation of simple parameters.
let parseNumber parseFunc kind (args:string list) =
    match args with
    | [i] ->
        match parseFunc i with
        | true, _ -> ok ()
        | false, _ -> fail (sprintf "Could not parse %s as an %s." i kind)
    // We shouldn't ever hit this clause as we should have already blown up with
    // a different error.
    | x -> fail (sprintf "parse number expected one argument but got %d" x.Length)

let parseInt = parseNumber (Int64.TryParse) "int"
let parseDouble = parseNumber (Double.TryParse) "float"

let validatePcrParams (args: string list) =
    args
    |> Seq.map PcrParamParse.parseArg
    |> Seq.map (lift ignore)
    |> collectValidations

type Platform = | Stitch | Megastitch

let parsePlatform (args: string list) =
    match args with
    | [i] ->
        match i with
        | "stitch" -> ok Stitch
        | "megastitch" -> ok Megastitch
        | _ -> fail (sprintf "Invalid platform '%s'.  Options are 'stitch' and 'megastitch'." i)
    // We shouldn't ever hit this clause as we should have already blown up with
    // a different error.
    | x -> fail (sprintf "platform expected one argument but got %d" x.Length)

/// Represents topology information about contructs
type Topology =
    | Linear
    | Circular

module Topology =
    [<Literal>]
    let LinearValue = "linear"
    
    [<Literal>]
    let CircularValue = "circular"
    
    [<Literal>]
    let PragmaName = "topology"
    
    let parse : string list -> Result<Topology, string> =
        function
        | [arg] ->
            match arg with
            | LinearValue -> ok Linear
            | CircularValue -> ok Circular
            | _ -> fail (sprintf "Invalid topology '%s'.  Options are 'linear' and 'circular'." arg)
        | x -> fail (sprintf "topology expected one argument but got %d" x.Length)

/// Pass-through placeholder validator.
let noValidate _ = ok ()

// Pragma defs that are used internally by the compiler.

let warningPragmaDef =
    { name = "warn"
      argShape = AtLeast(1)
      scope = BlockOrPart(Transient)
      desc = "Print a warning message."
      invertsTo = None
      validate = noValidate }

let warnoffPragmaDef =
    { name = "warnoff"
      argShape = One
      scope = BlockOnly(Persistent)
      desc = "Turn off specific warnings."
      invertsTo = None
      validate = noValidate } // TODO parse function here

let capaPragmaDef =
    { name = "capa"
      argShape = One
      scope = BlockOnly(Persistent)
      desc = "Enables particular compiler capabilities."
      invertsTo = None
      validate = noValidate }

let platformPragmaDef =
    { name = "platform"
      argShape = One
      scope = BlockOnly(Persistent)
      desc = "Specify an assembly platform to target, current options: 'stitch', 'megastitch'."
      invertsTo = None
      validate = parsePlatform >> (lift ignore) }

let markersetPragmaDef =
    { name = "markerset"
      argShape = One
      scope = BlockOnly(Persistent)
      desc = "Set the default marker set for a ### part."
      invertsTo = None
      validate = noValidate }

let namePragmaDef =
    { name = "name"
      argShape = One
      scope = BlockOrPart(Transient)
      desc = "Override name for a assembly or part."
      invertsTo = None
      validate = noValidate }

let fusePragmaDef =
    { name = "fuse"
      argShape = Zero
      scope = PartOnly
      desc = "Create a seamless junction with the next part."
      invertsTo = None
      validate = noValidate }

let topologyPragmaDef =
    { name = Topology.PragmaName
      argShape = One
      scope = BlockOnly(Persistent)
      desc = "The design has either linear or circular topology"
      invertsTo = None
      validate = Topology.parse >> (lift ignore) }

/// Base set of hard coded pragmas.  Plugins might augment this list
let pragmaDefsStatic : PragmaDef list =
   [ warningPragmaDef
     warnoffPragmaDef
     capaPragmaDef
     platformPragmaDef
     markersetPragmaDef
     namePragmaDef
     fusePragmaDef
     { name = "linkers"
       argShape = AtLeast(1)
       scope = BlockOnly(Persistent)
       desc = "Override the default set of RYSE linkers."
       invertsTo = None
       validate = noValidate } // TODO: import linker parse function here
     { name = "refgenome"
       argShape = One
       scope = BlockOrPart(Persistent)
       desc = "Specify a reference genome."
       invertsTo = None
       validate = noValidate }
     { name = "dnasrc"
       argShape = One
       scope = PartOnly
       desc = "Specify a DNA source for a part."
       invertsTo = None
       validate = noValidate }
     { name = "stitch"
       argShape = Zero
       scope = BlockOnly(Persistent)
       desc = "TODO description"
       invertsTo = None
       validate = noValidate }
     { name = "megastitch"
       argShape = Zero
       scope = BlockOnly(Persistent)
       desc = "TODO description"
       invertsTo = None
       validate = noValidate }
     { name = "rabitstart"
       argShape = Zero
       scope = PartOnly
       desc = "Designate part as the start of a RYSE rabit."
       invertsTo = Some("rabitend")
       validate = noValidate }
     { name = "rabitend"
       argShape = Zero
       scope = PartOnly
       desc = "Designate part as the end of a RYSE rabit."
       invertsTo = Some("rabitstart")
       validate = noValidate }
     { name = "primerpos"
       argShape = Exactly(2)
       scope = PartOnly;
       desc = "Dictate forward FWD or reverse REV primer position relative to first base of a short inline slice";
       invertsTo = None
       validate = noValidate } // TODO parse function here
     { name = "primermax"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Max length of primer that can be designed."
       invertsTo = None
       validate = parseInt }
     { name = "primermin"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Max length of primer that can be designed."
       invertsTo = None
       validate = parseInt }
     { name = "pcrparams"
       argShape = Range(1,5)
       scope = BlockOnly(Persistent)
       desc = "Set various parts of PCR conditions."
       invertsTo = None
       validate = validatePcrParams }
     { name = "targettm"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Set target melting temperature for pcr designs."
       invertsTo = None
       validate = parseDouble }
     { name = "seamlesstm"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Set target melting temperature for seamless designs (body of primer that amplifies the two pieces adjacent to the junction)."
       invertsTo = None
       validate = parseDouble }
     { name = "seamlessoverlaptm"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Set target melting temperature for the tail of the seamless primers that overlap in the middle to form the junction."
       invertsTo = None
       validate = parseDouble }
     { name = "atpenalty"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Set degree of tm flexibility to get a terminal G or C and unstable 3' end of an oligo."
       invertsTo = None
       validate = parseDouble }
     { name = "pcrassemblyparams"
       argShape = Range(1,5)
       scope = BlockOnly(Persistent)
       desc = "Set melting conditions for the overlap junction in a seamless design."
       invertsTo = None
       validate = validatePcrParams }
     { name = "minoverlaplen"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Sets the minimum overlap length for junction in a seamless design."
       invertsTo = None
       validate = parseInt }
     { name = "len"
       argShape = One
       scope = PartOnly
       desc = "Set specific heterology block length for a ~ part."
       invertsTo = None
       validate = parseInt }
     { name = "user"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "Set owner for any output fields with a creator field"
       invertsTo = None
       validate = noValidate }
     { name = "style"
       argShape = One
       scope = PartOnly
       desc = "Set allele swap style."
       invertsTo = None
       validate = noValidate }
     { name = "breed"
       argShape = One
       scope = PartOnly
       desc = "Specify RYSE breed for a specific rabit, overriding any breed inference."
       invertsTo = None
       validate = noValidate }
     { name = "inline"
       argShape = Zero
       scope = PartOnly
       desc = "Force long inline sequences to be created inline as part of a 2 piece rabit regardless of length."
       invertsTo = None
       validate = noValidate }
     { name = "seed"
       argShape = One
       scope = BlockOrPart(Persistent)
       desc = "Sets the seed for the random number generator for things like codon optimization."
       invertsTo = None
       validate = parseInt }
     { name = "codonopt"
       argShape = AtLeast 1
       scope = BlockOnly(Persistent)
       desc = "Set codon optimization parameters."
       invertsTo = None
       validate = noValidate }
     { name = "uri"
       argShape = One
       scope = BlockOrPart(Transient)
       desc = "Tag a part or assembly with a URI."
       invertsTo = None
       validate = noValidate }
     { name = "swapend"
       argShape = One
       scope = PartOnly
       desc = "State an end preference for an allele swap. Arg should be '3' or '5'."
       invertsTo = None
       validate = parseInt }
     { name = "promlen"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "preferred promoter length - overrides genome or system default."
       invertsTo = None
       validate = parseInt }
     { name = "termlen"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "preferred terminator length - overrides genome or system default."
       invertsTo = None
       validate = parseInt }
     { name = "termlenmrna"
       argShape = One
       scope = BlockOnly(Persistent)
       desc = "preferred terminator region length when part of mRNA part- overrides genome or system default."
       invertsTo = None
       validate = parseInt }
     topologyPragmaDef
    ]
/// Legal/Valid pragma names and defintions for lookup by name
let mutable private globalLegalPragmas : Map<_,_> option = None

let getLegalPragmas() =
    match globalLegalPragmas with
    | Some(p) -> p
    | None -> failwithf "Global pragma collection is not initialized."

/// Check that a pragma inverts to a legal pragma.  Returns the pragma it inverts to or raises an exception.
let validatePragmaInversion (declaredPrags: Map<string,PragmaDef>) p =
    match p.invertsTo with
    | None -> None
    | Some(name) ->
        match declaredPrags.TryFind name with
        | None ->
            failwithf "Pragma %s inverts to an unknown pragma %s" (p.name) name
        | Some(invTo) -> // inverts to a known pragma, make sure they have the same shape
            if p.argShape <> invTo.argShape then
                failwithf "Pragma %s inverts to %s but they have differing argShapes."
                    (p.name) (invTo.name)
            Some(invTo)

/// Initialize the global collection of valid pragmas, merging in definitions from plugins
/// to the built-in pragmas.  Performs some validation as well.
/// Raises an exception if something fails validation.
let finalizePragmas pluginPragmas =
    let pragmaDefs =
        pluginPragmas@pragmaDefsStatic
        |> List.distinctBy LanguagePrimitives.PhysicalHash

    let pragsByName =
        pragmaDefs
        |> List.map (fun p -> p.name, p)
        |> Map.ofList

    // Idiot check that we don't have any duplicate pragmas.
    if pragsByName.Count <> pragmaDefs.Length then
        failwithf "%d pragmas were defined but size of legalPragmas map is only %d. Name aliases?"
            (pragmaDefs.Length) (pragsByName.Count)

    // Make sure any pragmas that invert do it sensibly.
    // Raises an exception if any one doesn't validate.
    for pd in pragmaDefs do
        validatePragmaInversion pragsByName pd |> ignore

    // Initialize the global collection.
    globalLegalPragmas <- Some (pragsByName)

/// Instance of a pragma directive.
[<CustomEquality; CustomComparison>]
type Pragma = {
    definition: PragmaDef;
    args: string list}
    with
    member x.name = x.definition.name
    member x.isTransient =
        match x.definition.scope with
        | BlockOnly(Persistent) | BlockOrPart(Persistent) -> false
        | _ -> true
    /// Does this pragma announce the availability of an extension capability?
    member x.SetsCapability =
        if x.definition = capaPragmaDef then Some(x.args.[0].ToLower())
        else None
    /// Is this pragma a warning message?
    member x.IsWarning = x.definition = warningPragmaDef
    /// Is this pragma a flag to deactivate a warning?
    member x.IgnoresWarning =
        if x.definition = warnoffPragmaDef then Some(x.args.[0])
        else None
    /// Is this pragma a #capa directive?
    member x.IsCapa = x.definition = capaPragmaDef
    /// Helper function to check the list of args for a particular value.
    member x.hasVal(value:string) =
        List.contains value x.args
    /// Only consider pragma name and args in comparions.
    override x.Equals(obj) =
        match obj with
        | :? Pragma as p -> (x.name = p.name && x.args = p.args)
        | _ -> false
    /// Hash a Pragma as a combination of pName and args.
    override x.GetHashCode() = hash (x.name, x.args)
    interface System.IComparable with
        member x.CompareTo obj =
            match obj with
            | :? Pragma as p -> compare (x.name, x.args) (p.name, p.args)
            | _ -> invalidArg "obj" "cannot compare values of different types"
    override x.ToString() =
        sprintf "#%s %s" x.name (String.concat " " x.args)

/// Determine if a pragma inverts.  Return the definition of the pragma it inverts to.
/// This function will only fail if finalization hasn't occurred properly or if there are
/// invalid pragmas that somehow escaped validation.
let pragmaInverts p = validatePragmaInversion (getLegalPragmas()) p.definition

/// Format a pragma definition.
let formatPragma p =
    let argDescFormat v = sprintf "<a%d>" v
    let makeArgDesc (n:int) = [0..n-1] |> Seq.map argDescFormat |> String.concat " "
    let argDesc =
        match p.argShape with
        | Zero -> ""
        | One -> makeArgDesc 1
        | Exactly(n) -> makeArgDesc n
        | AtLeast(n) -> (makeArgDesc n) + " ..."
        | Range(n, m) -> (makeArgDesc n) + " (..." + (argDescFormat (m-1)) + ")"
        | ExactlySet(v) -> sprintf " <arg shapes: %A>" v
    let firstLine = sprintf "#%s %s" p.name argDesc
    let descLines = p.desc.Split [|'\n'|] |> List.ofArray |> List.map (fun d -> "    " + d)
    let scopeLine = sprintf "    Scoping: %s" p.scope.ToString
    (firstLine::scopeLine::descLines) |> String.concat "\n"

/// Print all available pragmas.
let pragmaUsage () =
    let orderedPragmas =
        getLegalPragmas()
        |> Map.toList
        |> List.sortBy fst // sort the pairs by name
        |> List.map snd // pull out just the sorted pragmas
    for p in orderedPragmas do printfn "%s" (formatPragma p)

/// Raise an exception if pName is not among the registered pragmas.
let validatePragmaName pName =
    if not (getLegalPragmas().ContainsKey pName) then
        failwithf "Requested unknown pragma '#%s'."
            pName

/// Validated pragma construction during parsing
let buildPragmaFromDef (pDef:PragmaDef) (values:string list) =
    let name = pDef.name

    // check that the right number of arguments were supplied
    let nArg = values.Length
    let checkNArgs n =
        if nArg <> n then
            fail (sprintf "Pragma #%s expected %d argument(s) but got %d: %A"
                name n nArg values)
        else ok ()
    let checkMinArgs min =
        if nArg < min then
            fail (sprintf "Pragma #%s expected at least %d argument(s) but got %d: %A"
                name min nArg values)
        else ok ()
    let checkMaxArgs max _ =
        if nArg > max then
            fail (sprintf "Pragma #%s expected at most %d argument(s) but got %d: %A"
                name max nArg values)
        else ok ()

    let checkArgShape() =
        match pDef.argShape with
        | Zero -> checkNArgs 0
        | One -> checkNArgs 1
        | Exactly(n) -> checkNArgs n
        | AtLeast(n) -> checkMinArgs n
        | Range(min,max) ->
            checkMinArgs min
            >>= checkMaxArgs max
        | ExactlySet(vals) ->
            if not (vals |> List.contains nArg) then
                fail (sprintf "Pragma %s expected any number of arguments in the set %A but got %d: %A"
                    name vals nArg values)
            else ok ()

    let validateArgs() = pDef.validate values

    // validation pipeline
    checkArgShape()
    >>= validateArgs
    >>= (fun () -> ok {definition = pDef; args = values})

/// Try to build a pragma from a name and values.
let buildPragma (name: string) (values: string list) =
    // try to get the pragma defintion
    match getLegalPragmas().TryFind name with
    | Some(pDef) -> buildPragmaFromDef pDef values
    | None -> fail (sprintf "Unknown or invalid pragma: '#%s'" name)


// ===========================
// PragmaCollection domain type
// ===========================
   
///<summary>
/// A PragmaCollection is a mapping between pragma name and the actual value
/// set for that pragma.  This is the main data structure in which pragmas
/// are passed around.  It is a helpful and safe wrapping of an immutable map.
/// It should be impossible to add invalid pragmas to this structure without
/// doing it manually through the underlying map.</summary>
type PragmaCollection = PragmaCollection of Map<string,Pragma>
    with
    member x.pmap = match x with PragmaCollection(pc) -> pc
    /// Add a Pragma to this collection.
    member x.Add(p:Pragma) = PragmaCollection (x.pmap.Add(p.name, p))
    /// Add a pragma to this collection using string name.
    member x.Add(pName:string) = x.Add(pName, [])
    /// Add a pragma to this collection using string name and single value.
    member x.Add(pName:string, value:string) = x.Add(pName, [value])
    /// Add a pragma to this collection using string name and values.
    member x.Add(pName:string, values:string list) =
        buildPragma pName values
        >>= (fun p -> ok (x.Add(p)))
    /// Remove a pragma from this collection.
    member x.Remove(name:string) =
        PragmaCollection (x.pmap.Remove(name))
    /// Remove a pragma from this collection.
    member x.Remove(pDef:PragmaDef) = x.Remove(pDef.name)
    /// Remove a pragma from this collection.
    member x.Remove(p:Pragma) = x.Remove(p.name)
    /// Merge a list of Pragmas into this collection.
    /// The incoming pragmas will clobber any pragmas set in this collection.
    member x.MergeIn(incoming:Pragma list) =
        incoming
        |> List.fold (fun (pc:Map<string,Pragma>) prag ->
                pc.Add(prag.name, prag))
            x.pmap
        |> PragmaCollection
    /// Merge another PragmaCollection into this one.
    /// The incoming pragmas will clobber any pragmas set in this collection.
    member x.MergeIn(incoming:PragmaCollection) =
        incoming.pmap
        |> Map.fold (fun (pc:Map<string,Pragma>) name prag ->
                pc.Add(name, prag))
            x.pmap
        |> PragmaCollection
    /// Has a pragma been set?
    /// Raises an exception if pName is not a registered pragma.
    member x.ContainsKey(pName:string) =
        validatePragmaName pName
        x.pmap.ContainsKey pName
    /// Has a pragma been set?
    member x.ContainsKey(pDef:PragmaDef) = x.pmap.ContainsKey pDef.name
    /// Has a pragma been set?
    member x.ContainsKey(p:Pragma) = x.pmap.ContainsKey p.name
    /// Get the values associated with a pragma.
    /// Raises an exception is pName is not a registered pragma.
    member x.TryGetValues(pName:string) =
        validatePragmaName pName
        match x.pmap.TryFind pName with
        | Some(p) -> Some(p.args)
        | None -> None
    /// Get a single value associated with a pragma, ignoring any extras.
    /// Raises an exception is pName is not a registered pragma.
    member x.TryGetOne(pName:string) =
        validatePragmaName pName
        match x.TryGetValues pName with
        | Some(v::_) -> Some(v)
        | None | Some([]) -> None
    /// Get a pragma.
    /// Raises an exception is pName is not a registered pragma.
    member x.TryFind(pName:string) =
        validatePragmaName pName
        x.pmap.TryFind pName
    /// Get a pragma by definition.
    member x.TryFind(pDef:PragmaDef) =
        x.pmap.TryFind pDef.name
    member x.Names =
        x.pmap |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    member x.Values =
        x.pmap |> Map.toSeq |> Seq.map snd
    member x.IsEmpty = x.pmap.IsEmpty
    /// Pretty-print a collection of pragmas.
    override x.ToString() =
        let ordered = x.pmap |> Map.toList |> List.sortBy fst |> List.map snd
        let entries = String.concat " " (ordered |> Seq.map (fun p -> p.ToString()))
        sprintf "PragmaCollection: %s" entries

let createPragmaCollection (pragmas: seq<Pragma>) =
    pragmas
    |> Seq.map (fun p -> p.name, p)
    |> Map.ofSeq
    |> PragmaCollection

let EmptyPragmas = PragmaCollection(Map.empty)

/// Determine the current assembly mode from pragma collection.
let assemblyMode (pc: PragmaCollection) =
    match pc.TryFind("platform") with
    | Some(p) -> parsePlatform p.args |> returnOrFail
    | None -> Megastitch

// ======================
// pragma deprecations and deprecation machinery
// ======================

type PragmaDeprecation =
    {name: string; replacement: string; replace: Pragma -> Pragma; extraMessage: string option}
    with
    member x.WarningMessage =
        let msg = sprintf "The pragma #%s is deprecated; please use #%s instead." x.name x.replacement
        match x.extraMessage with
        | Some(m) ->
            sprintf "%s\n%s" msg m
        | None -> msg

let private replaceStaticPlatformPragma which _ = {definition = platformPragmaDef; args = [which]}

let private stitchPragmaDeprecation =
    {name = "stitch";
     replacement = "platform";
     replace = replaceStaticPlatformPragma "stitch";
     extraMessage = Some("This pragma will be interpreted as '#platform stitch'.")}

let private megastitchPragmaDeprecation =
    {name = "megastitch";
     replacement = "platform";
     replace = replaceStaticPlatformPragma "megastitch";
     extraMessage = Some("This pragma will be interpreted as '#platform megastitch'.")}

let DeprecatedPragmas =
    [stitchPragmaDeprecation; megastitchPragmaDeprecation]
    |> Seq.map (fun pd -> pd.name, pd)
    |> Map.ofSeq



