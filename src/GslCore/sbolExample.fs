module sbolExample
open Amyris.Bio.utils
open FSharp.Data
open uri

// NOTE: see helper functions below.  Dammit SBOL, why did you decide to wrap
// every XML tag inside another XML tag...

type SBOLProvider =
    FSharp.Data.XmlProvider<"""
<gbom:gbom xmlns:gbom="http://www.amyris.com/xmlns/gbom/v0.2.0" xmlns:sbol="http://sbols.org/v2#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:prov="http://www.w3.org/ns/prov#">
  <sbol:ComponentDefinition rdf:about="http://amyris.com/GBoM/Component#156">
    <dcterms:title>parent</dcterms:title>
    <dcterms:description>a description</dcterms:description>
    <sbol:sequence rdf:resource="http://amyris.com/GBoM/ComponentSequence#120"/>
    <sbol:type rdf:resource="http://www.biopax.org/release/biopax-level3.owl#DnaRegion"/>
    <sbol:role rdf:resource="http://amyris.com/GBoM/Role#Rabit"/>
    <sbol:role rdf:resource="http://amyris.com/GBoM/Role#Stitch"/>
    <sbol:role rdf:resource="http://amyris.com/GBoM/Role#Megastitch"/>
    <sbol:component>
      <sbol:Component rdf:about="http://amyris.com/GBoM/SubcomponentLink#158">
        <sbol:access rdf:resource="http://sbols.org/v2#private"/>
        <sbol:definition rdf:resource="http://amyris.com/GBoM/Component#157"/>
      </sbol:Component>
    </sbol:component>
    <sbol:component>
      <sbol:Component rdf:about="http://amyris.com/GBoM/SubcomponentLink#159">
        <sbol:access rdf:resource="http://sbols.org/v2#private"/>
        <sbol:definition rdf:resource="http://amyris.com/GBoM/Component#158"/>
      </sbol:Component>
    </sbol:component>
    <sbol:component>
      <sbol:Component rdf:about="http://amyris.com/GBoM/SubcomponentLink#160">
        <sbol:access rdf:resource="http://sbols.org/v2#private"/>
        <sbol:definition rdf:resource="http://amyris.com/GBoM/Component#158"/>
      </sbol:Component>
    </sbol:component>
    <sbol:component>
      <sbol:Component rdf:about="http://amyris.com/GBoM/SubcomponentLink#161">
        <sbol:access rdf:resource="http://sbols.org/v2#private"/>
        <sbol:definition rdf:resource="http://amyris.com/GBoM/Component#158"/>
      </sbol:Component>
    </sbol:component>
    <sbol:component>
      <sbol:Component rdf:about="http://amyris.com/GBoM/SubcomponentLink#162">
        <sbol:access rdf:resource="http://sbols.org/v2#private"/>
        <sbol:definition rdf:resource="http://amyris.com/GBoM/Component#158"/>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Rabit"/>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Stitch"/>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Primer_5prime"/>
      </sbol:Component>
    </sbol:component>
    <sbol:sequenceAnnotation>
      <sbol:SequenceAnnotation rdf:about="http://amyris.com/GBoM/SequenceAnnotation#159">
        <sbol:component rdf:resource="http://amyris.com/GBoM/SubcomponentLink#159"/>
        <sbol:location>
          <sbol:Range rdf:about="http://amyris.com/GBoM/Range#159">
            <sbol:start>1</sbol:start>
            <sbol:end>2</sbol:end>
            <sbol:orientation rdf:resource="http://sbols.org/v2#inline"/>
          </sbol:Range>
        </sbol:location>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Rabit"/>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Stitch"/>
      </sbol:SequenceAnnotation>
    </sbol:sequenceAnnotation>
    <sbol:sequenceAnnotation>
      <sbol:SequenceAnnotation rdf:about="http://amyris.com/GBoM/SequenceAnnotation#160">
        <sbol:component rdf:resource="http://amyris.com/GBoM/SubcomponentLink#160"/>
        <sbol:location>
          <sbol:Range rdf:about="http://amyris.com/GBoM/Range#160">
            <sbol:start>2</sbol:start>
            <sbol:end>3</sbol:end>
            <sbol:orientation rdf:resource="http://sbols.org/v2#inline"/>
          </sbol:Range>
        </sbol:location>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Rabit"/>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Stitch"/>
      </sbol:SequenceAnnotation>
    </sbol:sequenceAnnotation>
    <sbol:sequenceAnnotation>
      <sbol:SequenceAnnotation rdf:about="http://amyris.com/GBoM/SequenceAnnotation#161">
        <sbol:component rdf:resource="http://amyris.com/GBoM/SubcomponentLink#161"/>
        <sbol:location>
          <sbol:Range rdf:about="http://amyris.com/GBoM/Range#161">
            <sbol:start>3</sbol:start>
            <sbol:end>4</sbol:end>
            <sbol:orientation rdf:resource="http://sbols.org/v2#inline"/>
          </sbol:Range>
        </sbol:location>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Rabit"/>
        <gbom:role rdf:resource="http://amyris.com/GBoM/Role#Stitch"/>
      </sbol:SequenceAnnotation>
    </sbol:sequenceAnnotation>
    <sbol:sequenceConstraint>
      <sbol:SequenceConstraint rdf:about="http://amyris.com/GBoM/SequenceConstraint#159">
        <sbol:restriction rdf:resource="http://sbols.org/v2#precedes"/>
        <sbol:subject rdf:resource="http://amyris.com/GBoM/SubcomponentLink#159"/>
        <sbol:object rdf:resource="http://amyris.com/GBoM/SubcomponentLink#158"/>
      </sbol:SequenceConstraint>
    </sbol:sequenceConstraint>
    <sbol:sequenceConstraint>
      <sbol:SequenceConstraint rdf:about="http://amyris.com/GBoM/SequenceConstraint#160">
        <sbol:restriction rdf:resource="http://sbols.org/v2#precedes"/>
        <sbol:subject rdf:resource="http://amyris.com/GBoM/SubcomponentLink#160"/>
        <sbol:object rdf:resource="http://amyris.com/GBoM/SubcomponentLink#161"/>
      </sbol:SequenceConstraint>
    </sbol:sequenceConstraint>
    <sbol:sequenceConstraint>
      <sbol:SequenceConstraint rdf:about="http://amyris.com/GBoM/SequenceConstraint#161">
        <sbol:restriction rdf:resource="http://sbols.org/v2#precedes"/>
        <sbol:subject rdf:resource="http://amyris.com/GBoM/SubcomponentLink#161"/>
        <sbol:object rdf:resource="http://amyris.com/GBoM/SubcomponentLink#162"/>
      </sbol:SequenceConstraint>
    </sbol:sequenceConstraint>
    <gbom:gslProgram>
      <gbom:gslSource>GSL ; GSL; GSL</gbom:gslSource>
      <gbom:gslVersion>1.0.0</gbom:gslVersion>
    </gbom:gslProgram>
  </sbol:ComponentDefinition>
  <sbol:ComponentDefinition rdf:about="http://amyris.com/GBoM/Component#157">
    <sbol:type rdf:resource="http://www.biopax.org/release/biopax-level3.owl#DnaRegion"/>
    <sbol:role rdf:resource="http://amyris.com/GBoM/Role#Rabit"/>
  </sbol:ComponentDefinition>
  <sbol:ComponentDefinition rdf:about="http://amyris.com/GBoM/Component#158">
    <dcterms:title>child1</dcterms:title>
    <sbol:sequence rdf:resource="http://amyris.com/GBoM/ComponentSequence#121"/>
    <sbol:type rdf:resource="http://www.biopax.org/release/biopax-level3.owl#DnaRegion"/>
    <sbol:role rdf:resource="http://amyris.com/GBoM/Role#Rabit"/>
    <gbom:gslProgram>
      <gbom:gslSource>VERYSMALLPROGRAM</gbom:gslSource>
      <gbom:gslVersion>1.0.0</gbom:gslVersion>
    </gbom:gslProgram>
  </sbol:ComponentDefinition>
  <sbol:Sequence rdf:about="http://amyris.com/GBoM/ComponentSequence#120">
    <sbol:elements>atcgatcgatcgatcgatcgatcgatcg</sbol:elements>
    <sbol:encoding rdf:resource="http://www.chem.qmul.ac.uk/iubmb/misc/naseq.html"/>
  </sbol:Sequence>
  <sbol:Sequence rdf:about="http://amyris.com/GBoM/ComponentSequence#121">
    <sbol:elements>atcg</sbol:elements>
    <sbol:encoding rdf:resource="http://www.chem.qmul.ac.uk/iubmb/misc/naseq.html"/>
  </sbol:Sequence>
</gbom:gbom>
""">
    //, Global=true>

// --- Helper items for generating XML objects ---

let defaultEncoding = "http://www.chem.qmul.ac.uk/iubmb/misc/naseq.html"

type Restriction = | Precedes | SameOrientationAs | OppositeOrientationAs

let restrictionObj restriction =
    match restriction with 
    | Precedes -> SBOLProvider.Restriction("http://sbols.org/v2#precedes")
    | SameOrientationAs -> SBOLProvider.Restriction("http://sbols.org/v2#sameOrientationAs")
    | OppositeOrientationAs -> SBOLProvider.Restriction("http://sbols.org/v2#oppositeOrientationAs")

let SequenceConstraint (uri:Uri) restriction (subj_uri:Uri) (obj_uri:Uri) =
    let innerSC =
        SBOLProvider.SequenceConstraint2(
            uri,
            (restrictionObj restriction),
            SBOLProvider.Subject(subj_uri),
            SBOLProvider.Object(obj_uri))
    SBOLProvider.SequenceConstraint(innerSC)

let SequenceAnnotation (uri:Uri) (componentUri:Uri) location (roleUris:seq<Uri>) =
    let roles = [|for ruri in roleUris -> SBOLProvider.Role3(ruri)|]
    let innerSA =
        SBOLProvider.SequenceAnnotation2(
            uri,
            (SBOLProvider.Component3(componentUri)),
            location,
            roles)
    SBOLProvider.SequenceAnnotation(innerSA)

let dnaType = "http://www.biopax.org/release/biopax-level3.owl#DnaRegion"
let rnaType = "http://www.biopax.org/release/biopax-level3.owl#RnaRegion"

/// Create a properly-wrapped Component reference
let Component (uri:Uri) (refCompUri:Uri) (roles:Uri list) =
    SBOLProvider.Component(
        SBOLProvider.Component2(
            uri,
            SBOLProvider.Access("http://sbols.org/v2#private"),
            SBOLProvider.Definition(refCompUri),
            roles |> List.map SBOLProvider.Role2 |> Array.ofList))

type TopLevel =
    | CompDef of SBOLProvider.ComponentDefinition
    | Seq of SBOLProvider.Sequence2

// --- Helper types for working at a higher level ---
// Ideally, nothing outside this module should actually touch SBOLProvider,
// as its generated types will change if we expand the trainer example.
// The types below represent a higher-level interface which can be expaneded
// into SBOL.

type Identity =
   {identity:Uri;
    name:string option;
    description:string option;}

type Orientation = | FWD | REV
    with
    member x.toString() = match x with | FWD -> "FWD" | REV -> "REV"
    member x.toChar() = match x with | FWD -> '0' | REV -> '1' 

type RangeLocation = {start:int; stop:int; orient:Orientation}
    with
    member x.emit() =
        SBOLProvider.Location(
            SBOLProvider.Range(
                uri.createTempUri(),
                x.start,
                x.stop,
                SBOLProvider.Orientation(
                    match x.orient with
                    | FWD -> "http://sbols.org/v2#inline"
                    | REV -> "http://sbols.org/v2#reverseComplement")))


type Sequence = {id:Identity; elements:string}
    with
    member x.emit() =
        SBOLProvider.Sequence2(x.id.identity, x.elements, SBOLProvider.Encoding(defaultEncoding))

/// Create a new Sequence type from dna.
let seqFromDna (dna:string) =
    {id = {identity = uri.createTempUri(); name = None; description = None};
     elements = dna}

type Location =
    | Range of RangeLocation
    | Precede of SubcomponentIntegration

and ComponentDefinition =
   {id:Identity;
    roles:Uri list;
    sequence:Sequence option;
    subcomponents:SubcomponentIntegration list;
    gslProg:string option}
    with
    /// Emit all top-level SBOL XML objects representing this entity.
    member x.emit() =
        // create all of the references and annotations
        let (compRefs, seqAnns, seqCons) =
            x.subcomponents |> List.fold (fun (cr, sa, sc) subcomp ->
                let cR, sAs, sCs = subcomp.emit()
                cR::cr, sAs@sa, sCs@sc)
                ([], [], [])

        let maybeCompSeq =
            match x.sequence with
            | Some(s) -> Some(s.emit())
            | None -> None
             
        let comp =
            SBOLProvider.ComponentDefinition(
                x.id.identity,
                x.id.name,
                x.id.description,
                (match maybeCompSeq with
                 | Some(s)-> Some(SBOLProvider.Sequence(s.About))
                 | None -> None),
                SBOLProvider.Type(dnaType),
                (x.roles |> Seq.map SBOLProvider.Role |> Array.ofSeq),
                compRefs |> Array.ofList,
                seqAnns |> Array.ofList,
                seqCons |> Array.ofList,
                // TODO: actually retrieve a GSL language version string!
                (match x.gslProg with
                 | Some(p) -> Some(SBOLProvider.GslProgram(p, "TODO VERSION STRING"))
                 | None -> None))
        match maybeCompSeq with
        | Some(s)-> [Seq(s); CompDef(comp)]
        | None -> [CompDef(comp)]
        

    /// Helper function to create a subcomponent reference to this CD.
    member x.asSubcomponent(locations, roles) =
        {identity = uri.createTempUri();
         compRef = x;
         locations = locations;
         roles = roles}

     
and SubcomponentIntegration =
   {identity:Uri;
    compRef:ComponentDefinition;
    locations:Location list;
    roles:Uri list}
    with
    /// Emit all SBOL objects needed to represent this subcomponent integration.
    member x.emit() =
        // build up lists of sequence annotations and constraints as necessary
        let mutable seqAnns, seqCons = [], []
        for location in x.locations do
            match location with
            | Range(rl) ->
                seqAnns <-
                    (SequenceAnnotation (uri.createTempUri()) x.identity (rl.emit()) (x.roles))
                    ::seqAnns
            | Precede(si) ->
                seqCons <-
                    (SequenceConstraint (uri.createTempUri()) Precedes (x.identity) (si.identity))
                    ::seqCons
        // only put roles on the Component if there are no SequenceAnnotations
        let compRoles = if seqAnns.IsEmpty then x.roles else []
        let comp = Component x.identity (x.compRef.id.identity) compRoles
        (comp, seqAnns, seqCons)

/// Convert a pile o' ComponentDefinitions into a full-blown GBoM.
let compileGbom (compDefs:seq<ComponentDefinition>) =
    let topLevels = compDefs |> Seq.collect (fun cd -> cd.emit()) |> List.ofSeq
    let rec collectTopLevels (tls:TopLevel list) seqs cds =
        match tls with
        | tl::tail ->
            match tl with
            | CompDef(cd) -> collectTopLevels tail seqs (cd::cds)
            | Seq(s)-> collectTopLevels tail (s::seqs) cds
        | [] -> seqs, cds
    let seqs, cds = collectTopLevels topLevels [] []
    SBOLProvider.Gbom(cds |> Array.ofList, seqs |> Array.ofList)



// --- Roles ---

let roleUriBase = unwrap (addNamespaces uri.amyrisUriBase ["Role"])

let addTermToNamespaceStatic ns term = unwrap (addTermToNamespace ns term)

// --- static URI definitions ---
let ryseLinkerRoleUri = addTermToNamespaceStatic roleUriBase "RYSELinker"
let fivePrimeLinkerRoleUri = addTermToNamespaceStatic roleUriBase "RYSELinker_5prime"
let threePrimeLinkerRoleUri = addTermToNamespaceStatic roleUriBase "RYSELinker_3prime"

let primerTailRoleUri = addTermToNamespaceStatic roleUriBase "PrimerTail"
let primerBodyRoleUri = addTermToNamespaceStatic roleUriBase "PrimerBody"
let ampPrimerRoleUri = addTermToNamespaceStatic roleUriBase "AmplificationPrimer"

let quickchangePrimerRoleUri = addTermToNamespaceStatic roleUriBase "QuickchangePrimer"

let fivePrimePrimerRoleUri = addTermToNamespaceStatic roleUriBase "Primer_5prime"
let threePrimePrimerRoleUri = addTermToNamespaceStatic roleUriBase "Primer_3prime"


// --- legacy top-level-component-related roles

let rabitRoleUri = addTermToNamespaceStatic roleUriBase "Rabit"

let rabitDnaRoleUri = addTermToNamespaceStatic roleUriBase "RabitDNAElement"

let stitchRoleUri = addTermToNamespaceStatic roleUriBase "Stitch"

let stitchRabitRoleUri = addTermToNamespaceStatic roleUriBase "StitchRabit"

let megastitchRoleUri = addTermToNamespaceStatic roleUriBase "Megastitch"

let rabitBreedUriBase = unwrap (addNamespaces roleUriBase ["RabitBreed"])

let rabitBreedRole breed = unwrap (addTermToNamespace rabitBreedUriBase breed)
