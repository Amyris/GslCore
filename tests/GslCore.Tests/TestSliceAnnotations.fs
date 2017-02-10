namespace GslCore.Tests
open System
open NUnit.Framework
open LegacyParseTypes
open constants
open commonTypes

[<TestFixture>]
type TestOrfAnnotation() = 

    let checkIndices (annotation:OrfAnnotation) (correct: int list) =
        Assert.AreEqual(
            correct |> List.map (fun x -> x*1<ZeroOffset>),
            annotation.CompleteCodonIndices() |> List.ofSeq)

    [<Test>]
    member x.TestCodonIndexGeneration() =
        // test really basic functionality
        let simpleOrf =
           {left = 0<ZeroOffset>;
            right = 7<ZeroOffset>;
            frameOffset = Zero;
            fwd = true}
        checkIndices simpleOrf [0; 3]

        let backwardsOrf = {simpleOrf with fwd = false}
        checkIndices backwardsOrf [7; 4]

        let offsetOrf = {simpleOrf with frameOffset = Two}
        checkIndices offsetOrf [1; 4]

        let backwardsOffsetOrf = {backwardsOrf with frameOffset = Two}
        checkIndices backwardsOffsetOrf [6; 3]

        let tinyOrf = {backwardsOrf with right = 1<ZeroOffset>}
        checkIndices tinyOrf []

    [<Test>]
    member x.TestAnnotationFromSlice() =
        // default range for gene
        let basicOrfSlice =
            {left = {x = 1<OneOffset>; relTo = FivePrime};
             lApprox = false;
             rApprox = false;
             right = {x = -1<OneOffset>; relTo = ThreePrime } }

        let featLen = 100
        let orfAnnotationFwd = orfAnnotationFromSlice basicOrfSlice featLen true Genomic
        Assert.AreEqual(0<ZeroOffset>, orfAnnotationFwd.left)
        Assert.AreEqual(99<ZeroOffset>, orfAnnotationFwd.right)
        checkIndices orfAnnotationFwd [0..3..97]
        Assert.That(orfAnnotationFwd.fwd)

        let orfAnnotationRev = orfAnnotationFromSlice basicOrfSlice featLen false Genomic
        Assert.AreEqual(0<ZeroOffset>, orfAnnotationRev.left)
        Assert.AreEqual(99<ZeroOffset>, orfAnnotationRev.right)
        checkIndices orfAnnotationRev [99..-3..2]
        Assert.That(not orfAnnotationRev.fwd)