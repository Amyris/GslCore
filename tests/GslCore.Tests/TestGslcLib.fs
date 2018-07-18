/// Locate the GSLC test library on the filesystem and ensure it is present for unit tests.
module TestGslcLib

open System
open System.IO

/// Path to the GSLC test library, verified to exist before tests are run.
let testLibDir = 
   [Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "..", "..", "..", "..", "TestGslcLib"); // mono
    Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "..", "..", "..", "..", "..", "TestGslcLib")] // dotnetcore
    |> List.filter System.IO.Directory.Exists
    |> function
        | [testDir] -> testDir
        | [] -> failwith "Test directory not found."
        | x -> failwithf "Too many test directories found: %O" x