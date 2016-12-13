namespace GslCore.Tests
open System
open NUnit.Framework
open constants
open commonTypes

[<TestFixture>]
type TestOrfAnnotation() = 

    let checkIndices (annotation:OrfAnnotation) correct =
        Assert.AreEqual(
            correct,
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