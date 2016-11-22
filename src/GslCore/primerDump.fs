/// IO routine for emitting simple primer details
module primerDump
open System.IO
open commonTypes
open Amyris.Bio.utils
open Amyris.Bio.biolib
open System

/// Dump out all the primers/primerparts to define the construct
let simplePrimerDump (file:string) (primers:DivergedPrimerPair list list) (assemblies:AssemblyOut list) =
    // User wants primers now
    if file <> "-" then printfn "Writing primers to %s" file
    let outF = if file = "-" then None else Some(new StreamWriter(file))
    // use outF = new StreamWriter(file)

    let w (s:string) = match outF with
                            | None -> stdout.WriteLine(s)
                            | Some(x) -> x.WriteLine(s)

    let dumpOne i (primerList:DivergedPrimerPair list,assembly:AssemblyOut) =
        let name = assembly.name
        for dpp in primerList do
            match dpp with
                | GAP -> () // Don't need to emit these
                | DPP(dpp) -> 
                    let cols = seq {
                                    yield name
                                    yield (sprintf "%d" i)
                                    yield (arr2seq dpp.fwd.Primer)
                                    yield (arr2seq dpp.rev.Primer) 
                                    yield (arr2seq dpp.fwd.tail)
                                    yield (arr2seq dpp.fwd.body)
                                    yield (arr2seq dpp.rev.tail)
                                    yield (arr2seq dpp.rev.body)
                                    //
                                    // These two designs are the "medieval" versions (Credit to Max for the term)
                                    // which have complete overlap between the two primers, ensuring an insanely high TM
                                    // They also function as bridge primers
                                    //
                                    yield ([| dpp.rev.body |> revComp ; dpp.fwd.body |] |> Array.concat |> arr2seq)
                                    yield ([| dpp.fwd.body |> revComp ; dpp.rev.body |] |> Array.concat |> arr2seq)

                                    // Emit primer pieces individually and Tms
                                    let tm (a:char array) = Amyris.Bio.primercore.temp assembly.designParams.pp a a.Length |> fun t -> sprintf "%3.1f" (t*1.0/(1.0<Amyris.Bio.primercore.C>))

                                    yield ( match dpp.fwd.Interval DNAIntervalType.ANNEAL with 
                                                        | Some(i) -> dpp.fwd.Primer.[i.il..i.ir] |> arr2seq
                                                        | None -> ""
                                          )
                                    yield ( match dpp.fwd.Interval DNAIntervalType.SANDWICH with 
                                                        | Some(i) -> dpp.fwd.Primer.[i.il..i.ir] |> arr2seq
                                                        | None -> ""
                                          )
                                    yield ( match dpp.fwd.Interval DNAIntervalType.AMP with 
                                                        | Some(i) -> dpp.fwd.Primer.[i.il..i.ir] |> arr2seq
                                                        | None -> ""
                                          )
                                    yield ( match dpp.rev.Interval DNAIntervalType.ANNEAL with 
                                                        | Some(i) -> dpp.rev.Primer.[i.il..i.ir] |> arr2seq
                                                        | None -> ""
                                          )
                                    yield ( match dpp.rev.Interval DNAIntervalType.SANDWICH with 
                                                        | Some(i) -> dpp.rev.Primer.[i.il..i.ir] |> arr2seq
                                                        | None -> ""
                                          )
                                    yield ( match dpp.rev.Interval DNAIntervalType.AMP with 
                                                        | Some(i) -> dpp.rev.Primer.[i.il..i.ir] |> arr2seq
                                                        | None -> ""
                                          )
                                    yield ( match dpp.fwd.Interval DNAIntervalType.ANNEAL with 
                                                        | Some(i) -> dpp.fwd.Primer.[i.il..i.ir] |> tm
                                                        | None -> ""
                                          ) 
                                    yield ( match dpp.fwd.Interval DNAIntervalType.AMP with 
                                                | Some(i) -> dpp.fwd.Primer.[i.il..i.ir] |> tm
                                                | None -> ""
                                        )
                                    yield ( match dpp.rev.Interval DNAIntervalType.ANNEAL with 
                                                        | Some(i) -> dpp.rev.Primer.[i.il..i.ir] |> tm
                                                        | None -> ""
                                          )
                                    yield ( match dpp.rev.Interval DNAIntervalType.AMP with 
                                                        | Some(i) -> dpp.rev.Primer.[i.il..i.ir] |> tm
                                                        | None -> ""
                                          )

                                    //yield dpp.f
                        }
                    String.Join("\t",cols) |> w
       
    w(String.Join("\t",
                                seq {
                                        yield "Name"
                                        yield "Id"
                                        yield "fwd"
                                        yield "rev"
                                        yield "fwdtail"
                                        yield "fwdbody"
                                        yield "revtail"
                                        yield "revbody"
                                        yield "fwdbridge"
                                        yield "revbridge"
                                        yield "fwdanneal"
                                        yield "fwdsandwich"
                                        yield "fwdamp"
                                        yield "revanneal"
                                        yield "revsandwich"
                                        yield "revamp"
                                        yield "fwdannealTm"
                                        yield "fwdampTm"
                                        yield "revannealTm"
                                        yield "revampTm"
                                        //yield "sequence"
                                }
    ))

    List.zip primers assemblies |> List.iteri (dumpOne)
    match outF with
        | Some(outF) -> outF.Close()
        | None -> ()


