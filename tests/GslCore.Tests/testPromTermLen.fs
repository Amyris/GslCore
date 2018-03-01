﻿module testPromTermLen
/// Test #promoterlen #terminatorlen work

open NUnit.Framework
open LegacyParseTypes
open constants
open commonTypes
open pragmaTypes
open Amyris.ErrorHandling

/// location of test gslc_lib fixtures
let testLibDir = @"../../../../TestGslcLib"

[<TestFixture>]
type TestPromTermLen() = 
    let emptyPragmas = PragmaCollection(Map.empty)
    do
        // initialize pragmas
        pragmaTypes.finalizePragmas []

    let same context expected actual =
        if expected<>actual then 
            failwithf "%s: expected= %d and actual=%d not equal" context expected actual

    let checkOneGenome pragmas name promLen termLen termLenMRNA =
        let gd = new RefGenome.GenomeDef(testLibDir,name)

        let part = DnaCreation.translateGenePrefix pragmas gd TERMINATOR
        same "terminator length test" termLen ((part.right.x-part.left.x+1<OneOffset>)/1<OneOffset>) // +1 since ends are inclusive

        let part = DnaCreation.translateGenePrefix pragmas gd PROMOTER
        same "promoter length test" promLen ((part.right.x-part.left.x+1<OneOffset>)/1<OneOffset>)  // +1 since ends are inclusive

        let mRNA = DnaCreation.translateGenePrefix pragmas gd MRNA
        same "termmrna length test" termLenMRNA (((mRNA.right.x-1<OneOffset>)+1<OneOffset>)/1<OneOffset>) // Use 1 (rel to 3' end as the start of the terminator region

    let testPragma name value refGenome expProm expTerm expTermMRNA =
        match buildPragma name [value] with
        | Ok (p,[]) ->
            let map = [ p.name,p] |> Map.ofList |> PragmaCollection
            checkOneGenome map refGenome expProm expTerm expTermMRNA 
        | _ -> failwith "building promlen pragma"
        
    [<Test>]
    member __.TestPragmasExist() =
        let checkPragmaExists name =
            Assert.DoesNotThrow (fun () -> returnOrFail (buildPragma name ["250"]) |> ignore) |> ignore

        checkPragmaExists "promlen"
        checkPragmaExists "termlen"
        checkPragmaExists "termlenmrna"

    [<Test>]
    member __.TestDefaultTerminatorLen() =
        checkOneGenome emptyPragmas "TestGenome" promLenDefault termLenDefault termLenMRNADefault

    [<Test>]
    member __.TestCustomTerminatorLen() =
        checkOneGenome emptyPragmas "TestGenome2" 750 250 300

    [<Test>]
    member __.TestPromLenPragma() =
        testPragma "promlen" "123" "TestGenome" 123 termLenDefault termLenMRNADefault 

    [<Test>]
    member __.TestTermLenPragma() =
        testPragma "termlen" "123" "TestGenome" promLenDefault 123 termLenMRNADefault 

    [<Test>]
    member __.TestTermLenMRNAPragma() =
        testPragma "termlenmrna" "123" "TestGenome" promLenDefault termLenDefault 123