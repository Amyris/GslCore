namespace testGslc
open System
open NUnit.Framework
open uri

[<TestFixture>]
type TestUri() = 

    [<Test>]
    member x.TestUriConstruction() =

        match uri.buildUri [] "test" with
        | Ok(u) -> Assert.AreEqual("http://amyris.com/GBoM/test", u)
        | Err(e) -> failwith e

        Assert.AreEqual(uri.linkerUri "0", "http://amyris.com/GBoM/Component/Linker/0")
        Assert.AreEqual(uri.linkerUri "A", "http://amyris.com/GBoM/Component/Linker/A")
