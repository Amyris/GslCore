module TestSeamlessPrimers

open System
open NUnit.Framework
open LegacyParseTypes
open constants
open commonTypes
open Amyris.Dna
open pragmaTypes
let map = Map.ofSeq

let uHO = { id= Some(2)
            amplified= true
            annotations = []
            breed= B_UPSTREAM
            description= "uHO"
            destFr= 0<ZeroOffset>
            destFwd= true
            destTo= 549<ZeroOffset>
            dna = Dna("ACCTTTTTTGTGCGTGTATTGAAATATTATGACATATTACAGAAAGGGTTCGCAAGTCCTGTTTCTATGCCTTTCTCTTAGTAATTCACGAAATAAACCTATGGTTTACGAAATGATCCACGAAAATCATGTTATTATTTACATCAACATATCGCGAAAATTCATGTCATGTCCACATTAACATCATTGCAGAGCAACAATTCATTTTCATAGAGAAATTTGCTACTATCACCCACTAGTACTACCATTGGTACCTACTACTTTGAATTGTACTACCGCTGGGCGTTATTAGGTGTGAAACCACGAAAAGTTCACCATAACTTCGAATAAAGTCGCGGAAAAAAGTAAACAGCTATTGCTACTCAAATGAGGTTTGCAGAAGCTTGTTGAAGCATGATGAAGCGTTCTAAACGCACTATTCATCATTAAATATTTAAAGCTCATAAAATTGTATTCAATTCCTATTCTAAATGGCTTTTATTTCTATTACAACTATTAGCTCTAAATCCATATCCTCATAAGCAGCAATCAATTCTATCTATACTTTAAA")
            dnaSource= "S288C"
            extId= None
            materializedFrom= None // Some({LegacyParseTypes.PPP})
            pragmas= PragmaCollection Map.empty
            sliceName= ""
            sliceType= REGULAR
            sourceChr= "4"
            sourceFr= 48031<ZeroOffset>
            sourceFrApprox= true
            sourceFwd= false
            sourceTo= 48580<ZeroOffset>
            sourceToApprox= false
            template= Some(Dna("ACCTTTTTTGTGCGTGTATTGAAATATTATGACATATTACAGAAAGGGTTCGCAAGTCCTGTTTCTATGCCTTTCTCTTAGTAATTCACGAAATAAACCTATGGTTTACGAAATGATCCACGAAAATCATGTTATTATTTACATCAACATATCGCGAAAATTCATGTCATGTCCACATTAACATCATTGCAGAGCAACAATTCATTTTCATAGAGAAATTTGCTACTATCACCCACTAGTACTACCATTGGTACCTACTACTTTGAATTGTACTACCGCTGGGCGTTATTAGGTGTGAAACCACGAAAAGTTCACCATAACTTCGAATAAAGTCGCGGAAAAAAGTAAACAGCTATTGCTACTCAAATGAGGTTTGCAGAAGCTTGTTGAAGCATGATGAAGCGTTCTAAACGCACTATTCATCATTAAATATTTAAAGCTCATAAAATTGTATTCAATTCCTATTCTAAATGGCTTTTATTTCTATTACAACTATTAGCTCTAAATCCATATCCTCATAAGCAGCAATCAATTCTATCTATACTTTAAA"))
            uri= None
           }:DNASlice

let inlineSlice = { amplified= false
                    annotations = []
                    breed= B_INLINE
                    description= "ATGTGAC"
                    destFr= 550<ZeroOffset>
                    destFwd= true
                    destTo= 556<ZeroOffset>
                    dna= Dna("ATGTGAC")
                    dnaSource= "S288C"
                    extId = None
                    id= Some(0)
                    materializedFrom = None
                    pragmas= PragmaCollection Map.empty
                    sliceName= ""
                    sliceType= INLINEST
                    sourceChr= "inline"
                    sourceFr= 0<ZeroOffset>
                    sourceFrApprox= false
                    sourceFwd= true
                    sourceTo= 6<ZeroOffset>
                    sourceToApprox= false
                    template= Some(Dna("ATGTGAC"))
                    uri= None
                    }
let dHO = { amplified= true
            annotations= []
            breed= B_DOWNSTREAM
            description= "dHO"
            destFr= 557<ZeroOffset>
            destFwd= true
            destTo= 1106<ZeroOffset>
            dna= Dna("AATGTGTATATTAGTTTAAAAAGTTGTATGTAATAAAAGTAAAATTTAATATTTTGGATGAAAAAAACCATTTTTAGACTTTTTCTTAACTAGAATGCTGGAGTAGAAATACGCCATCTCAAGATACAAAAAGCGTTACCGGCACTGATTTGTTTCAACCAGTATATAGATTATTATTGGGTCTTGATCAACTTTCCTCAGACATATCAGTAACAGTTATCAAGCTAAATATTTACGCGAAAGAAAAACAAATATTTTAATTGTGATACTTGTGAATTTTATTTTATTAAGGATACAAAGTTAAGAGAAAACAAAATTTATATACAATATAAGTAATATTCATATATATGTGATGAATGCAGTCTTAACGAGAAGACATGGCCTTGGTGACAACTCTCTTCAAACCAACTTCAGCCTTTCTCAATTCATCAGCAGATGGGTCTTCGATTTGCAAAGCAGCCAAAGCATCGGACAAAGCAGCTTCAATCTTGGACTTGGAACCTCTCTTCAATTTAGAAGACAAGACTGGGTCAGTGACAGTTTGTTCGAT")
            dnaSource= "S288C"
            extId= None
            id= Some(1)
            materializedFrom= None
            pragmas= PragmaCollection Map.empty
            sliceName= ""
            sliceType= REGULAR
            sourceChr= "4"
            sourceFr= 45720<ZeroOffset>
            sourceFrApprox= false
            sourceFwd= false
            sourceTo= 46269<ZeroOffset>
            sourceToApprox= true
            template= Some(Dna("AATGTGTATATTAGTTTAAAAAGTTGTATGTAATAAAAGTAAAATTTAATATTTTGGATGAAAAAAACCATTTTTAGACTTTTTCTTAACTAGAATGCTGGAGTAGAAATACGCCATCTCAAGATACAAAAAGCGTTACCGGCACTGATTTGTTTCAACCAGTATATAGATTATTATTGGGTCTTGATCAACTTTCCTCAGACATATCAGTAACAGTTATCAAGCTAAATATTTACGCGAAAGAAAAACAAATATTTTAATTGTGATACTTGTGAATTTTATTTTATTAAGGATACAAAGTTAAGAGAAAACAAAATTTATATACAATATAAGTAATATTCATATATATGTGATGAATGCAGTCTTAACGAGAAGACATGGCCTTGGTGACAACTCTCTTCAAACCAACTTCAGCCTTTCTCAATTCATCAGCAGATGGGTCTTCGATTTGCAAAGCAGCCAAAGCATCGGACAAAGCAGCTTCAATCTTGGACTTGGAACCTCTCTTCAATTTAGAAGACAAGACTGGGTCAGTGACAGTTTGTTCGAT"))
            uri=None
        }

[<TestFixture>]
type TestSeamlessPrimers() = 
    let checkSliceTypes (exp:SliceType list) (actual:SliceType list) =
        if exp <> actual then
            printfn "ERROR: matching slice types in seamless check"
            printfn "Expected: %s" (String.Join(",",exp |> List.map (formatST)))
            printfn "Actual  : %s" (String.Join(",",actual |> List.map (formatST)))
        Assert.AreEqual(exp,actual)

    [<Test>]
    member x.testShortInline() =
        // No need for fuse directive for small inline sequence 
        //                                    <----------
        //    part / inline / part  =>   L part inline part L
        //                                    ---------->
        let dnaParts = [ uHO ; inlineSlice ; dHO]
        let fused = SeamlessPlugin.procInsertFuse false dnaParts
        let pTypes = fused |> List.map (fun p -> p.sliceType)
        let expected = [    SliceType.LINKER ; 
                            SliceType.REGULAR ; 
                            SliceType.INLINEST ; 
                            SliceType.REGULAR ; 
                            SliceType.LINKER 
                       ]
        // bug   LINKER,REG,FUSION,INLINE,REG,LINKER
        checkSliceTypes expected pTypes

        ()



