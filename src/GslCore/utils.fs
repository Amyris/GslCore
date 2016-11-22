/// Functions used throughout GSLc that have no internal dependencies.
module utils
open System
open System.Reflection
open System.Text
open Microsoft.FSharp.Core.Printf
open constants
open Microsoft.FSharp.Reflection

/// Print an integer id that might not be assigned yet 
let ambId (i :int option) = match i with None -> "?" | Some(i) -> string(i)

/// Produce a padding string of spaces n long
let pad n = String.replicate n " "

/// upper case a DNA array of chars
let basesUpper (c:char []) =
    c |> Array.map (fun b -> match b with
                                | 'a' -> 'A'
                                | 't' -> 'T'
                                | 'c' -> 'C'
                                | 'a' -> 'G'
                                | 'n' -> 'N'
                                | _ as x when x>='a' && x<='z' -> char(int(x)-int('a') + int('A'))
                                | _ as x -> x
                    )

let limitTo n (s:string) = if s.Length > n then s.Substring(0,n) else s

///Returns the case name of the object with union type 'ty.
let GetUnionCaseName (x:'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

/// Boilerplate error to throw a meaningful exception if we screwed up a match expression that uses
/// a non-exhaustive combination of active patterns.
let nonExhaustiveError x =
    failwithf "A match expression with an active pattern had a non-exhaustive case: %A" x

/// Pretty print an exception chain, including all inner exceptions.
/// Uses reflection to print the type of each exception as well.
/// Taken from https://sergeytihon.wordpress.com/2013/04/08/f-exception-formatter/
// TODO: promote to Amyris.ErrorHandling
let prettyPrintException (e:Exception) =
    let sb = StringBuilder()
    let delimeter = String.replicate 50 "*"
    let nl = Environment.NewLine
    let rec printException (e:Exception) count =
        if (e :? TargetException && e.InnerException <> null)
        then printException (e.InnerException) count
        else
            if (count = 1) then bprintf sb "%s%s%s" e.Message nl delimeter
            else bprintf sb "%s%s%d)%s%s%s" nl nl count e.Message nl delimeter
            bprintf sb "%sType: %s" nl (e.GetType().FullName)
            // Loop through the public properties of the exception object
            // and record their values.
            e.GetType().GetProperties()
            |> Array.iter (fun p ->
                // Do not log information for the InnerException or StackTrace.
                // This information is captured later in the process.
                if (p.Name <> "InnerException" && p.Name <> "StackTrace" &&
                    p.Name <> "Message" && p.Name <> "Data") then
                    try
                        let value = p.GetValue(e, null)
                        if (value <> null)
                        then bprintf sb "%s%s: %s" nl p.Name (value.ToString())
                    with
                    | e2 -> bprintf sb "%s%s: %s" nl p.Name e2.Message
            )
            if (e.StackTrace <> null) then
                bprintf sb "%s%sStackTrace%s%s%s" nl nl nl delimeter nl
                bprintf sb "%s%s" nl e.StackTrace
            if (e.InnerException <> null)
            then printException e.InnerException (count+1)
    printException e 1
    sb.ToString()
