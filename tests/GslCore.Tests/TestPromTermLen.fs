/// Test #promoterlen #terminatorlen work
module testPromTermLen

open NUnit.Framework
open constants
open commonTypes
open pragmaTypes
open Amyris.ErrorHandling
open TestGslcLib

[<TestFixture>]
type TestPromTermLen() = 
    do
        // initialize pragmas
        pragmaTypes.finalizePragmas []

    let checkOneGenome pragmas name promLen termLen termLenMRNA =
        let gd = new RefGenome.GenomeDef(testLibDir,name)
        gd.Load()
        let part = DnaCreation.translateGenePrefix pragmas gd TERMINATOR
        Assert.AreEqual(
            termLen,
            ((part.right.x-part.left.x+1<OneOffset>)/1<OneOffset>)) // +1 since ends are inclusive

        let part = DnaCreation.translateGenePrefix pragmas gd PROMOTER
        Assert.AreEqual(
            promLen,
            ((part.right.x-part.left.x+1<OneOffset>)/1<OneOffset>))  // +1 since ends are inclusive

        let mRNA = DnaCreation.translateGenePrefix pragmas gd MRNA
        Assert.AreEqual(
            termLenMRNA,
            (((mRNA.right.x-1<OneOffset>)+1<OneOffset>)/1<OneOffset>)) // Use 1 (rel to 3' end as the start of the terminator region

    let testPragma name value refGenome expProm expTerm expTermMRNA =
        let p = buildPragma name [value] |> returnOrFail
        let map = EmptyPragmas.Add(p)
        checkOneGenome map refGenome expProm expTerm expTermMRNA 
                
    [<Test>]
    member __.TestGenomesLoadable() =
        let gd = new RefGenome.GenomeDef(testLibDir,"TestGenome")
        gd.Load()
        ()

    [<Test>]
    member __.TestPragmasExist() =
        let checkPragmaExists name =
            Assert.DoesNotThrow (fun () -> returnOrFail (buildPragma name ["250"]) |> ignore) |> ignore

        checkPragmaExists "promlen"
        checkPragmaExists "termlen"
        checkPragmaExists "termlenmrna"

    [<Test>]
    member __.TestDefaultTerminatorLen() =
        checkOneGenome EmptyPragmas "TestGenome" promLenDefault termLenDefault termLenMRNADefault

    [<Test>]
    member __.TestCustomTerminatorLen() =
        checkOneGenome EmptyPragmas "TestGenome2" 750 250 300

    [<Test>]
    member __.TestPromLenPragma() =
        testPragma "promlen" "123" "TestGenome" 123 termLenDefault termLenMRNADefault 

    [<Test>]
    member __.TestTermLenPragma() =
        testPragma "termlen" "123" "TestGenome" promLenDefault 123 termLenMRNADefault 

    [<Test>]
    member __.TestTermLenMRNAPragma() =
        testPragma "termlenmrna" "123" "TestGenome" promLenDefault termLenDefault 123
