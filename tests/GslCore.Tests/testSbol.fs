namespace testGslc
open System
open System.IO
open System.Xml
open NUnit.Framework

open sbolExample

[<TestFixture>]
type TestSbolProvider() = 

    [<Test>]
    member x.TestSbolParsing() =
        let testSbolInput = """
<gbom:gbom xmlns:gbom="http://www.amyris.com/xmlns/gbom/v0.2.0" xmlns:sbol="http://sbols.org/v2#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:prov="http://www.w3.org/ns/prov#">
  <sbol:ComponentDefinition rdf:about="http://amyris.com/GBoM/Component/149">
    <dcterms:title>child1</dcterms:title>
    <sbol:type rdf:resource="http://www.biopax.org/release/biopax-level3.owl#DnaRegion"/>
    <sbol:role rdf:resource="http://amyris.com/GBoM/Role/Rabit"/>
    <gbom:gslProgram>
      <gbom:gslSource>VERYSMALLPROGRAM</gbom:gslSource>
      <gbom:gslVersion>1.0.0</gbom:gslVersion>
    </gbom:gslProgram>
  </sbol:ComponentDefinition>
</gbom:gbom>"""
        let parsedInput = sbolExample.SBOLProvider.Parse(testSbolInput)
        Assert.AreEqual (parsedInput.ComponentDefinitions.Length, 1)
        let cd = parsedInput.ComponentDefinitions.[0]
        Assert.AreEqual (cd.Title, Some("child1"))
        Assert.IsEmpty parsedInput.Sequences

    [<Test>]
    member x.TestSbolEmitting() =
        let seq = seqFromDna "ATCG"
        let subseq = seqFromDna "AAAA"
        let subcd =
           {id={identity="test subcomp URI"; name=None; description=None};
            roles=["test sc role URI"];
            sequence=Some(subseq);
            subcomponents=[];
            gslProg=None}
        let cd =
           //{id={identity="test URI"; name=Some("test name"); description=Some("test desc")};
           {id={identity="test URI"; name=None; description=None};
            roles=["test role URI"];
            sequence=Some(seq);
            subcomponents=[subcd.asSubcomponent([], ["test role"])];
            gslProg=None}
        let gbom = compileGbom [cd]
        let gbomSub = compileGbom [cd; subcd]
        use x = new XmlTextWriter(stdout)
                    
        x.Formatting <- Formatting.Indented
        gbom.XElement.WriteTo(x)
        gbomSub.XElement.WriteTo(x)
