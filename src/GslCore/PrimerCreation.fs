module PrimerCreation
/// Support routines for primer design scenarios and primer generation for stitches
open commonTypes
open System
open constants // primer parameters
open Amyris.Bio.primercore
open Amyris.Bio
open Amyris.Bio.utils
open Amyris.Bio.biolib
open Amyris.Dna
open DesignParams
open pragmaTypes

/// Check if tail of A overlaps head of B
let checkTailAOverlapsHeadB (a: Dna) (b: Dna) =
    let rec maxOverlap i =
        if i >= a.Length-12 then
            false
        else
            if a.[i..] = (b.[..a.Length-i-1]) then
                true
            else maxOverlap (i+1)

    maxOverlap (max (a.Length-b.Length) 0)

let checkBContainedInA (a: Dna) (b: Dna) =
    let rec checkAt i =
        if i > a.Length-b.Length then
            false
        elif a.[i..i+b.Length-1] = b then
            true
        else
            checkAt (i+1)
    checkAt 0

let checkParallelOverlap (a: Dna) (b: Dna) =
    if (checkTailAOverlapsHeadB a b) ||
       (checkTailAOverlapsHeadB b a) ||
       (checkBContainedInA a b)  ||
       (checkBContainedInA b a)
    then
        () // test passes
    else
        failwithf "insufficient overlap in checkParallelOverlap \na=%O \nb=%O" a b

let checkAntiParallelOverlap (a: Dna) (b: Dna) =
    let rec maxOverlap i =
        if i < 11 then
            failwithf "insufficient overlap in checkAntiparallelOvelap a=%O b=%O bRC=%O"
                a b (b.RevComp())

        if a.[..i] = (b.[..i].RevComp()) then () else maxOverlap (i-1)

    maxOverlap ((min a.Length b.Length)-1)

//type private TuneDirection = T_INCREASE | T_DECREASE | T_NONE
type TuneStep =
    | CHOP_F_AMP
    | CHOP_F_ANNEAL
    | CHOP_R_AMP
    | CHOP_R_ANNEAL
    | SLIDE_F_LEFT
    | SLIDE_F_RIGHT
    | SLIDE_R_LEFT
    | SLIDE_R_RIGHT
    | EXT_F_AMP
    | EXT_F_ANNEAL
    | EXT_R_AMP
    | EXT_R_ANNEAL

let nonSlide s =
    match s with
    | CHOP_F_AMP
    | CHOP_F_ANNEAL
    | CHOP_R_AMP
    | CHOP_R_ANNEAL
        -> true
    | SLIDE_F_LEFT // Support routines for primer design scenarios and primer generation for stitches
    | SLIDE_F_RIGHT
    | SLIDE_R_LEFT
    | SLIDE_R_RIGHT
        -> false
    | EXT_F_AMP
    | EXT_F_ANNEAL
    | EXT_R_AMP
    | EXT_R_ANNEAL
        -> true

///  Current state of optimization during tail tuning.  Settings and best results so far.
/// We need to tune fwdTail length (ft)  fwd body (fb) as well as reverse equivalents.
/// There are 3 deltas to optimize, for the anneal delta, fwd amp delta and rev amp delta.
type private TuneState =
   {bestAnnealDelta: float<C>;
    bestFwdDelta: float<C>;
    bestRevDelta: float<C>;
    (*bestFt : int ; bestRt : int ; bestFb : int ; bestRb : int ; *)
    ft: int;
    fb: int;
    rt: int;
    rb: int}
    with
        override x.ToString() =
            sprintf "bestA=%A bestF=%A bestR=%A bestT=%A"
                x.bestAnnealDelta
                x.bestFwdDelta
                x.bestRevDelta
                (x.bestAnnealDelta+x.bestFwdDelta+x.bestRevDelta)

[<Struct>]
type TuneVector(a:int,b:int,c:int,d:int) =
    member x.A = a
    member x.B = b
    member x.C = c
    member x.D = d

let maxTuneTailsIters = 500

/// Extend tails of primers (or truncate) to optimize annealing Tm
/// after left/right design off a linker/inline
let tuneTails
        verbose
        (dp:DesignParams)
        (fwdTailLenFixed:int option)
        fwdTailLenMin
        fwdTailLenMax
        firmMiddle
        (revTailLenFixed:int option)
        revTailLenMin
        revTailLenMax
        (fwd:Primer)
        (rev:Primer)
        (middleDNA : Dna) =
    // Input is two sets of primers like this where the region between | symbols is an inline sequence.
    // Output is adjust primer tails that have a better annealing length

    //    rev.body        middleDNA fwd.body
    //                   .....v   possible revTailFixed constraint
    //    <-------------|-------o
    //                  o-------|----------->
    //                     ^.. possible fwdTailLenFixed constraint

    // The basic idea behind that code is that in the general case you have something like this:
    // upstreamRabit ; sandwich1 ; linker ; sandwich2 ; downstreamRabit
    // The (up to ) two primers overlapping have to encode amplification for upstream and downstream rabits,
    // and overlap enough in the middle recreate the sandwich sequence and linker sequence.  Each primer/oligo
    // has to cover the linker by a minimum amount so it looks like this:
    //                               >>>>>>>>>>>>>>>>>>>>>>>>>>>
    // upstreamRabit ; sandwich1 ; linker ; sandwich2 ; downstreamRabit
    //       <<<<<<<<<<<<<<<<<<<<<<<<<<<<
    // There are other constraints too - this includes fwdTailLenMax/revTailLenMax (the tail part of the oligo
    // should only go up to the end of the linker if there is a linker).
    // The sandwich sequences don't always occur. For example:
    //                 >>>>>>>>>>>>>>>
    // upstreamRabit ; linker ;  downstreamRabit
    //      <<<<<<<<<<<<<<<<<
    // In addition, since the same code does seamless designs with no linker, you can also can have something like:
    //         >>>>>>>>>>>>>>>
    // upstreamRabit;downstreamRabit
    //     <<<<<<<<<<<<<<
    // In this case you can see the tails stick out into the other rabit part (these form two part rabits ultimately)

    // If there is a constraint on the tail length, make the template just that DNA, else use as much as possible
    /// middleDNA (linker + sandwich sequences) + the body part of the forward oligo, which corresponds to the
    /// downstream rabit. Downstream rabit is included to deal with seamless designs without a linker
    let revTemplate =
        match revTailLenFixed with
        | Some(n) -> middleDNA.[..n-1].RevComp()
        | None ->
            DnaOps.append middleDNA fwd.body
            |> DnaOps.revComp

    /// the body part of the reverse oligo, which corresponds to the upstream rabit, + middleDNA (linker + sandwich seqs).
    /// Upstream rabit is included to deal with seamless designs without a linker
    let fwdTemplate =
        match fwdTailLenFixed with
        | Some(n) -> middleDNA.[middleDNA.Length-n..]
        | None -> DnaOps.append (rev.body.RevComp()) (middleDNA)

    /// The reverse primer body (end of upstream rabit) + middleDNA (linkers + sandwich sequence) + the forward
    /// primer body (beginning of downstream rabit)
    let fullTemplate = DnaOps.concat [ rev.body.RevComp() ; middleDNA ; fwd.body ]

    /// Target Tm for middle annealing part.  Cheat if it's a linker and we just want to keep this part (ideally) full length
    let annealTarget =
        match firmMiddle with
        | Some(x) ->
                if verbose then printfn "procAssembly: tuneTails: setAnnealTarget to firmMiddle=%A" x
                x
        | None ->
                if verbose then printfn "procAssembly: tuneTails: setAnnealTarget to seamlessOverlapTm=%A" dp.seamlessOverlapTm
                dp.seamlessOverlapTm
    //let annealTarget = dp.seamlessOverlapTm // Just use this,  rev/fwdTailLenFixed vars take care of constraining RYSE linkers

    // Find two positions f and r that create a better ovelap tm
    //                   ...............> r (rev tail length)
    //    <-------------Y-------X
    //                  o-------|----------->
    //              f<..........   (fwd tail length)
    //  ----------------^ (inlineOffset)
    let inlineLen = middleDNA.Length
    /// Last base of inline region
    let X = rev.body.Length+middleDNA.Length-1
    /// First base of inline region
    let Y = rev.body.Length
    if verbose then
        printfn "procAssembly: tuneTails: tuneTailOpt: X=%d Y=%d\n template=%s" X Y fullTemplate.str

    /// Maximum amount by which we can stray from ideal annealing term during tune tails search
    let maxAnnealSearchDeviation = 10.0<C>
    /// Recursive optimization of the primer ends, adjusting lengths to get the amp / anneal and primer lengths optimized
    let rec tuneTailsOpt itersRemaining (state:TuneState) (seen':Set<TuneVector>) =

        // record our current state so we never retrace our steps
        let seen = seen'.Add(TuneVector(state.fb,state.ft,state.rb,state.rt))

        // Calculate overlap oligo.  Tricky but it has to be a substring of the fwd oligo
        // from some point to the end
        if itersRemaining = 0 then
            failwithf "tuneTailsOpt failed - hit iteration limit"

        /// Matches when an oligo has hit its maximum length
        let (|OligoMax|_|) = function
            | x when x = dp.pp.maxLength -> Some(x)
            | _ -> None

        /// matches if an oligo gets too long
        let (|OligoOver|_|) = function
            | x when x > dp.pp.maxLength -> Some(x)
            | _ -> None
            (*
        /// matches if an oligo gets too short
        let (|OligoUnder|_|) = function
            | x when x <= dp.pp.oMax  -> Some(x)
            | _ -> None
        *)
        if verbose then
            printf "tuneTailsOpt: rt=%d rb=%d ft=%d fb=%d rD=%f aD=%f fD=%f (rLen=%d) (fLen=%d)"
                state.rt state.rb state.ft state.fb
                (state.bestRevDelta/1.0<C>) (state.bestAnnealDelta/1.0<C>) (state.bestFwdDelta/1.0<C>)
                (state.rt+state.rb) (state.ft+state.fb)

        /// length of the forward oligo, including the tail and the body
        let fwdLen = state.ft+state.fb
        /// length of the reverse oligo, including the tail and the body
        let revLen = state.rt+state.rb

        let updateFwd (s:TuneState) =
            {s with bestFwdDelta =
                    if s.fb < 5 then 999.0<C>
                    else dp.targetTm - (Amyris.Bio.primercore.temp dp.pp fwd.body.arr s.fb) }

        let updateRev (s:TuneState) =
            {s with bestRevDelta =
                    if s.rb < 5 then 999.0<C>
                    else dp.targetTm - (Amyris.Bio.primercore.temp dp.pp rev.body.arr s.rb) }

        let updateAnneal (s:TuneState) =
            {s with bestAnnealDelta =
                    annealTarget
                  - (Amyris.Bio.primercore.temp
                        dp.pp (fullTemplate.[X-s.ft+1..].arr) ((Y+s.rt-1)-(X-s.ft+1)+1))}

        /// Takes in a move (union case Tunestep) and updates the TuneState accordingly
        let makeMove = function
            | CHOP_F_AMP    -> {state with fb = state.fb-1} |> updateFwd
            | CHOP_F_ANNEAL -> {state with ft = state.ft-1} |> updateAnneal
            | CHOP_R_AMP    -> {state with rb = state.rb-1} |> updateRev
            | CHOP_R_ANNEAL -> {state with rt = state.rt-1} |> updateAnneal
            | SLIDE_F_LEFT  -> {state with fb = state.fb-1; ft = state.ft+1} |> updateFwd |> updateAnneal
            | SLIDE_F_RIGHT -> {state with fb = state.fb+1; ft = state.ft-1} |> updateFwd |> updateAnneal
            | SLIDE_R_LEFT  -> {state with rb = state.rb+1; rt = state.rt-1} |> updateRev |> updateAnneal
            | SLIDE_R_RIGHT -> {state with rt = state.rt+1; rb = state.rb-1} |> updateRev |> updateAnneal
            | EXT_F_AMP     -> {state with fb = state.fb+1} |> updateFwd
            | EXT_F_ANNEAL  -> {state with ft = state.ft+1} |> updateAnneal
            | EXT_R_AMP     -> {state with rb = state.rb+1} |> updateRev
            | EXT_R_ANNEAL  -> {state with rt = state.rt+1} |> updateAnneal

        // possible moves, given how long the current oligo is and the limitations (fwdTailLenMax, fwdTailLenMin, etc.).
        let moves =
            seq {
                match fwdLen with
                | OligoOver(_) -> // cut something off
                    if state.fb>dp.pp.minLength then  yield CHOP_F_AMP
                    // Put guard on this to stop best anneal data running away to zero kelvin ;(
                    if fwdTailLenFixed.IsNone && state.ft > fwdTailLenMin && state.bestAnnealDelta < maxAnnealSearchDeviation then
                        yield CHOP_F_ANNEAL
                | OligoMax(_) -> // could slide or cut
                    if state.bestFwdDelta < 0.0<C> && state.fb>dp.pp.minLength then yield CHOP_F_AMP
                    if fwdTailLenFixed.IsNone then
                        if  state.bestAnnealDelta < 0.0<C> && state.ft > fwdTailLenMin then
                            yield CHOP_F_ANNEAL
                        if state.fb < fwd.body.Length && state.ft > fwdTailLenMin then yield SLIDE_F_RIGHT
                        if state.rt > revTailLenMin && state.ft < fwdTailLenMax then
                            yield SLIDE_F_LEFT
                | _ ->
                    // All these moves are only possible if the tail isn't a fixed length
                    if fwdTailLenFixed.IsNone then
                        if state.bestFwdDelta < 0.0<C> && state.ft < fwdTailLenMin && state.fb>dp.pp.minLength
                            then yield CHOP_F_AMP
                        elif state.fb < fwd.body.Length then yield EXT_F_AMP
                        if state.bestAnnealDelta < 0.0<C> && state.ft > fwdTailLenMin then
                            yield CHOP_F_ANNEAL
                        elif state.ft < fwdTailLenMax then yield EXT_F_ANNEAL

                        match sign state.bestAnnealDelta, sign state.bestFwdDelta with
                        | +1,+1 -> // both anneal and fwd amp are too cold, could cut either back and extend the other
                            if state.bestAnnealDelta < state.bestFwdDelta then
                                if state.fb<fwd.body.Length && state.ft > fwdTailLenMin then
                                    yield SLIDE_F_RIGHT
                                elif (state.rt > revTailLenMin && state.ft < fwdTailLenMax)
                                    then yield SLIDE_F_LEFT
                        | +1,-1 -> // anneal too cold, fwd too hot
                            if (state.rt > revTailLenMin && state.ft < fwdTailLenMax) then yield SLIDE_F_LEFT
                        | -1,+1 -> // anneal too hot, fwd too cold
                            if state.fb < fwd.body.Length && state.ft > fwdTailLenMin then yield SLIDE_F_RIGHT
                        | -1,-1 -> // both too hot
                            if state.bestAnnealDelta < state.bestFwdDelta then
                                if state.fb < fwd.body.Length-1 && state.ft > fwdTailLenMin then yield SLIDE_F_RIGHT
                                else if (state.rt > revTailLenMin && state.ft < fwdTailLenMax) then yield SLIDE_F_LEFT
                        | 0,+1  // anneal perfect, amp too cold
                        | -1,0 -> // anneal hot, amp perfect
                            if state.fb < fwd.body.Length && state.ft > fwdTailLenMin then yield SLIDE_F_RIGHT
                        | 0, -1  // anneal perfect, amp too warm
                        | +1, 0 -> // anneal cold, amp perfect
                            if (state.fb<fwd.body.Length && state.rt > revTailLenMin && state.ft < fwdTailLenMax)
                                then yield SLIDE_F_LEFT
                        | 0, 0 -> () // no complaints
                        | x -> failwithf "unexpected delta sign combo %A" x

                match revLen with
                | OligoOver(_) -> // cut something off reverse primer
                    if state.rb>dp.pp.minLength then yield CHOP_R_AMP
                    if revTailLenFixed.IsNone && state.rt > revTailLenMin then yield CHOP_R_ANNEAL
                | OligoMax(_) -> // could slide or cut
                    if state.bestRevDelta < 0.0<C> && state.rb>dp.pp.minLength then yield CHOP_R_AMP
                    if revTailLenFixed.IsNone then
                        if state.bestAnnealDelta < 0.0<C> && state.rt > revTailLenMin
                            then yield CHOP_R_ANNEAL

                        if state.rb < rev.body.Length-1 && state.rt > revTailLenMin
                            then yield SLIDE_R_LEFT
                        elif state.rt < revTailLenMax then yield SLIDE_R_RIGHT
                | _ ->
                    if state.bestRevDelta < 0.0<C> && state.rb>dp.pp.minLength then yield CHOP_R_AMP
                    elif state.rb < rev.body.Length then yield EXT_R_AMP
                    if revTailLenFixed.IsNone then
                        if state.bestAnnealDelta < 0.0<C> && state.rt > revTailLenMin
                            then yield CHOP_R_ANNEAL
                        elif state.rt < revTailLenMax then yield EXT_R_ANNEAL

                        match sign state.bestAnnealDelta, sign state.bestRevDelta with
                        | +1,+1 -> // both anneal and rev amp are too cold, could cut either back and extend the other
                             if state.bestAnnealDelta < state.bestRevDelta then
                                if state.rb < rev.body.Length && state.rt > revTailLenMin
                                    then yield SLIDE_R_LEFT
                                elif state.rt < revTailLenMax then yield SLIDE_R_RIGHT
                        | +1,-1 -> // anneal too cold, rev too hot
                            if state.rt < revTailLenMax then yield SLIDE_R_RIGHT
                        | -1,+1 -> // anneal too hot, rev too cold
                            if state.rb < rev.body.Length-1 && state.rt > revTailLenMin
                                then yield SLIDE_R_LEFT
                        | -1,-1 -> // both too hot
                            if state.bestAnnealDelta < state.bestRevDelta then
                                if state.rb < rev.body.Length && state.rt > revTailLenMin
                                    then yield SLIDE_R_LEFT
                                elif state.rt < revTailLenMax then yield SLIDE_R_RIGHT
                        | 0,+1  // anneal perfect, amp too cold
                        | -1,0 -> // anneal hot, amp perfect
                            if state.rb < rev.body.Length && state.rt > revTailLenMin
                                then yield SLIDE_R_LEFT
                        | 0,-1  // anneal perfect, amp too warm
                        | +1,0 -> // anneal cold, amp perfect
                            if state.rb<rev.body.Length && state.rt < revTailLenMax
                                then yield SLIDE_R_RIGHT
                        | 0,0 -> () // no complaints
                        | _ as x -> failwithf "unexpected combo %A" x
            } |> Array.ofSeq

        let newStates =
            moves |> Array.map (fun s -> (s,makeMove s))
            |> Array.filter (fun (_,m) -> seen.Contains(TuneVector(m.fb,m.ft,m.rb,m.rt)) |> not)

        if verbose then
            let moveStates =
              [|for move,s in newStates ->
                sprintf "%A:tD=%3.1f/rD=%3.1f/aD=%3.1f/fD=%3.1f"
                    move
                    ((abs(s.bestFwdDelta)+(abs s.bestAnnealDelta)+(abs s.bestRevDelta))/1.0<C>)
                    (s.bestRevDelta/1.0<C>) (s.bestAnnealDelta/1.0<C>) (s.bestFwdDelta/1.0<C>)
              |]
            printf " moves: %s "
                (String.Join(",", moveStates))

        if newStates.Length = 0 then
            if verbose then printfn " done"
            state // use passed state
        else
            /// s1 is better than s2 if its length in excess of maxOligo is smaller or in case both
            /// primers are <= maxlength, then total Tm deviation is smaller
            let better (s1:TuneState)  (s2:TuneState) =
                let excess1 =
                    (s1.ft+s1.fb-dp.pp.maxLength |> max 0)+(s1.rt+s1.rb-dp.pp.maxLength |> max 0)
                let excess2 =
                    (s2.ft+s2.fb-dp.pp.maxLength |> max 0)+(s2.rt+s2.rb-dp.pp.maxLength |> max 0)
                (excess1 < excess2) ||
                (excess1 = excess2 &&
                    (abs s1.bestAnnealDelta)+(abs s1.bestFwdDelta)+(abs s1.bestRevDelta)
                  < (abs s2.bestAnnealDelta)+(abs s2.bestFwdDelta)+(abs s2.bestRevDelta))
            // Pick the lowest
            let bestMove, lowestS =
                newStates |> Array.fold (fun (bestMove,bestS) (move,s) ->
                        if better s bestS then (move,s) else (bestMove,bestS))
                    (newStates.[0])

            if verbose then printf "bestM=%A" bestMove

            let primersOutOfSpec =
                state.fb+state.ft > dp.pp.maxLength  || // total forward oligo too long
                state.rb+state.rt > dp.pp.maxLength  || // total reverse oligo too long
                state.fb+state.ft < dp.pp.minLength  || // total forward oligo too short
                state.rb+state.rt < dp.pp.minLength   // total reverse oligo too short

            if better lowestS state then
                if verbose then
                    printfn " cont, better option bestS=[%A]  currS=[%A]" lowestS state
                tuneTailsOpt (itersRemaining-1) lowestS seen
            else
                if primersOutOfSpec then
                    // Still need to do something even though the lowest state isn't better than the one we're in now
                    // Need to make sure we don't get stuck in a loop though.  Find the best non sliding move
                    if verbose then printfn " primers still out of spec"

                    let _,lowestS =
                        newStates
                        |> Array.filter (fun (m,_) -> nonSlide m)
                        |> Array.fold (fun (bestMove,bestS) (move,s) ->
                                if better s bestS then (move,s) else (bestMove,bestS))
                            (newStates.[0])

                    tuneTailsOpt (itersRemaining-1) lowestS seen
                else
                    if verbose then printfn " best not better, done"
                    state

    // Calculate all the starting temperatures for the 3 pieces so we know where we stand
    //                 |--anneal tm -|
    //                 --------------------->
    //                            (---------) Amp fwd tm
    //  <-----------------------------
    //  (----------------) Amp rev tmp
    //
    //let startAnnealTm = Amyris.Bio.primercore.temp dp.pp fwd.tail fwd.tail.Length
    let startAnnealTm =
        if fwd.tail.Length = 0 || rev.tail.Length = 0 then annealTarget
        else Amyris.Bio.primercore.temp
                dp.pp
                (fullTemplate.[X-fwd.tail.Length+1..].arr)
                ((Y+rev.tail.Length-1)-(X-fwd.tail.Length+1)+1)

    if verbose then
        printfn "tuneTailOpt: starting fwdTail=%O" fwd.tail
        printfn "tuneTailOpt: starting fwdBody=%O" fwd.body
        printfn "tuneTailOpt: starting revTail=%O" rev.tail
        printfn "tuneTailOpt: starting revBody=%O" rev.body
        printfn "tuneTailOpt: starting fwdTailLenFixed=%s"
            (match fwdTailLenFixed with | None -> "no" | Some(x) -> sprintf "yes %d" x)
        printfn "tuneTailOpt: starting revTailLenFixed=%s"
            (match revTailLenFixed with | None -> "no" | Some(x) -> sprintf "yes %d" x)
        printfn "tuneTailOpt: starting fwdTailLenMin=%d" fwdTailLenMin
        printfn "tuneTailOpt: starting revTailLenMin=%d" revTailLenMin
        printfn "tuneTailOpt: starting startAnnealTm=%f" (startAnnealTm/1.0<C>)
        printfn "tuneTailOpt: starting annealTarget=%f" (annealTarget/1.0<C>)
        printfn "tuneTailOpt: starting middleDNA=%O" middleDNA
        printfn "tuneTailOpt: starting fwdTemplate=%O" fwdTemplate
        printfn "tuneTailOpt: starting revTemplate=%O" revTemplate
        printfn "tuneTailOpt: starting fullTemplate=%O" fullTemplate

    let rec trimIfNeeded (p:Primer) =
        if p.lenLE(dp.pp.maxLength) then p else
        let ampTemp = Amyris.Bio.primercore.temp dp.pp p.body.arr p.body.Length
        let ampDelta = abs (ampTemp - dp.targetTm)
        let annealTemp = Amyris.Bio.primercore.temp dp.pp p.tail.arr p.tail.Length
        let annealDelta = abs (annealTemp - dp.seamlessOverlapTm)
        if ampDelta < annealDelta && p.body.Length > dp.pp.minLength then
            trimIfNeeded { p with body = p.body.[..p.body.Length-2]}
        elif ampDelta > annealDelta && p.tail.Length > dp.pp.minLength then
            trimIfNeeded  { p with tail = p.tail.[1..] }
        else p

    if fwd.Primer.Length = 0 || rev.Primer.Length = 0 then
        // No primer on one side, no joy in optimizing except we still need to ensure that
        // the starting primers we were handed observe the length requirements
        let fwd' = if fwd.Primer.Length = 0 then fwd else trimIfNeeded fwd
        let rev' = if rev.Primer.Length = 0 then rev else trimIfNeeded rev
        fwd',rev'
    else
        // precalculate the fwd / rev body temps for reference
        let fwdAmpTm = Amyris.Bio.primercore.temp dp.pp fwd.body.arr fwd.body.Length
        let revAmpTm = Amyris.Bio.primercore.temp dp.pp rev.body.arr rev.body.Length

        // Start optimization of tail/body with full length tail and body for the original designed primers.
        // This may well be too long for the max oligo length but the tuneTailsOpt function will adjust till they are legal
        let start: TuneState =
           {bestAnnealDelta = annealTarget-startAnnealTm;
            ft = fwd.tail.Length;
            rt = rev.tail.Length;
            bestFwdDelta = dp.targetTm-fwdAmpTm;
            bestRevDelta = dp.targetTm-revAmpTm;
            fb = fwd.body.Length;
            rb = rev.body.Length}

        // Call to tunetails - optimize lengths of body/tail
        // ================================================================================
        let finalParams = tuneTailsOpt maxTuneTailsIters start Set.empty
        let f = finalParams.ft
        let r = finalParams.rt

        if verbose then
            printfn "tuneTailOpt: ending ft=%d fb=%d" (finalParams.ft) (finalParams.fb)
            printfn "tuneTailOpt: ending rt=%d rb=%d" (finalParams.rt) (finalParams.rb)
            printfn "tuneTailOpt: ending fwd %O|%O"
                (fwdTemplate.[fwdTemplate.Length-f..]) (fwd.body.[..finalParams.fb-1])
            printfn "tuneTailOpt: ending middle %O" middleDNA
            printfn "tuneTailOpt: ending template %O" fullTemplate
            printfn "tuneTailOpt: ending rev %O|%O"
                (rev.body.[..finalParams.rb-1].RevComp())
                (revTemplate.[revTemplate.Length-r..].RevComp())

        assert(finalParams.fb<=fwd.body.Length)
        assert(finalParams.rb<=rev.body.Length)
        assert(finalParams.ft<=fwdTemplate.Length)
        assert(finalParams.rt<=revTemplate.Length)

        let overlapLen = f + r - inlineLen
        let fwdFinalLen = f + finalParams.fb
        let revFinalLen = r + finalParams.rb

        let fwd' =
           {fwd with
                tail = fwdTemplate.[fwdTemplate.Length-f..];
                body = fwd.body.[..finalParams.fb-1];
                annotation =
                   [{il = 0; ir = overlapLen-1; iType = DNAIntervalType.ANNEAL};
                    {il = fwdFinalLen-finalParams.fb; ir = fwdFinalLen-1; iType = DNAIntervalType.AMP};
                    {il = (fwdFinalLen-finalParams.fb-inlineLen |> max 0); // might not cover full inline region
                     ir = fwdFinalLen-finalParams.fb-1;
                     iType = DNAIntervalType.SANDWICH}]
           }
        let rev' =
           {rev with
                tail = revTemplate.[revTemplate.Length-r..];
                body = rev.body.[..finalParams.rb-1];
                annotation =
                   [{il = 0;ir=overlapLen-1; iType = DNAIntervalType.ANNEAL};
                    {il = revFinalLen-finalParams.rb; ir = revFinalLen-1; iType = DNAIntervalType.AMP};
                    {il = revFinalLen-finalParams.rb-inlineLen |> max 0; // might not cover full inline region
                     ir = revFinalLen-finalParams.rb-1;
                     iType = DNAIntervalType.SANDWICH}]
           }

        // Check that the antiparallel primers overlap in the middle except when they were clearly not intended to
        // e.g. linkerless cases we aren't actually briding
        if fwdTailLenMin > 0 && revTailLenMin > 0 then
            checkAntiParallelOverlap fwd.Primer rev.Primer

        // Ensure that after we chop everything up, the final primer is contained within the
        // original primer, otherwise bad things happen to the amplified sequence.
        //let mf = min fwd.Primer.Length fwd'.Primer.Length

        checkParallelOverlap fwd.Primer fwd'.Primer
        checkParallelOverlap rev.Primer rev'.Primer
        fwd',rev'

let prettyPrintPrimer = function
    | DPP(dpp) -> sprintf "DPP(%s)" dpp.name
    | GAP -> "GAP"
    | SANDWICHGAP -> "GAPSANDWICH"

type PrimerPosOrient = FWD of int |REV of int |NONE
let parsePrimerPos (pragmas:PragmaCollection) =
        match pragmas.TryGetValues("primerpos") with
        | Some(v) ->
            match v |> List.map (fun (s:string) -> s.ToUpper()) with
            // TODO: these should be parsed and converted into union cases much earlier
            // in compiler execution
            | [ "FWD"; offsetStr ] -> FWD(int offsetStr)
            | [ "REV"; offsetStr ] -> REV(int offsetStr)
            | _ -> failwithf "ERROR: invalid primerpos pragma '%A', should be fwd ### or rev ### " v
        | None -> NONE

/// Do some sanity checking of generated primers some tail of a must be a prefix of B
let prefix (aPre:char array) (bPre:char array) =
    // Assume there must be some bases in A that overhang
    let a = upper aPre
    let b = upper bPre

    // Need at least 12 bases on both sides of the overhang, a could be a complete prefix of B
    assert(a.Length >= 12 )
    // 0........................>i
    // AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    //                     BBBBBBBBBBBBBBBBBB
    // i starts at zero (index into A) and searches up till a window from the right hand side
    // Looking for a suffix of A that matches the prefix of B
    let rec checkAt i =
        if i > a.Length - 12 then // getting too close to the end
            failwithf "primer overhang too small in checkAt\n    a=%s (length=%d) i=%d\n    b=%s (%d)"
                (arr2seq a) a.Length i (arr2seq b.[..min (b.Length-1) 50]) b.Length
        else
            assert(a.Length-i <= b.Length)
            assert(a.Length-i >= 0)
            assert(i<a.Length)
            assert(i>=0)
            assert(a.[i..].Length = b.[..a.Length-i-1].Length)
            if a.[i..] = b.[..a.Length-i-1] then () // ok
            else checkAt (i+1)

    checkAt (a.Length- (min a.Length b.Length )) // (max 10 (a.Length-(min a.Length b.Length)))


let primerCheck (p:char array) (fr:char array)= prefix p fr

/// design seamless primers across a junction that may have flexible endpoints on either side
/// return primer, offset, len reverse and primer, offset len, fwd
let seamless verbose (dp:DesignParams) (prev:DNASlice) (next:DNASlice) =
    // We need a forward and reverse primer designed into the adjacent sequences
    // Allocate half max len to each primer
    let maxFwd = dp.pp.maxLength / 2
    let maxRev = dp.pp.maxLength - maxFwd

    /// Wrapper function for oligo design to capture and report errors in primer design
    let pdWrap (debug:bool) (pen:PrimerParams) (task:OligoTask) =
        match primercore.oligoDesignWithCompromise debug pen task with
        | Some(x) ->
            // Debugging
            if verbose then
                let t = primercore.temp pen x.oligo x.oligo.Length
                printf "seamless: %s %d\n" (arr2seq x.oligo) (int t)
            Some(x)
        | None ->
            printf "WARNING: primer design failed for %A %A \n%s\n" pen task (format60 task.temp)
            None

    /// Build a single primer into a DNA slice, allowing up to length bp and handling approximate edges
    let designSingle (s:DNASlice) fwd length =
        let pen =
            {dp.pp with
                   maxLength = length;
                   // be less tough on temp if  we asking for high temp 68C
                   tmPenalty = if dp.targetTm> 60.0<C> then 3.0 else 1.0}

        // Handle approximate ends, and make sure we are looking at the right end being flexible..
        // bugfix 20111111 - wasn't using correct approximate end flag
        if (fwd && s.sourceFrApprox) || (not fwd && s.sourceToApprox) then
            let task: OligoTask =
               {tag = if fwd then "PF" else "PR";
                temp =
                    if fwd then s.dna.[0..min (s.dna.Length-1) (2*approxMargin)].arr
                    else s.dna.RevComp().[0..min s.dna.Length (2*approxMargin)].arr;
                align = ANCHOR.CENTERLEFT;
                strand = STRAND.TOP;
                offset = 0;
                targetTemp = dp.targetTm; // was historically seamlessTm incorrectly till ~ 10/2017
                sequencePenalties = None}

            pdWrap false pen task

        else
            let task: OligoTask =
                {tag = if fwd then "PF" else "PR";
                 temp = if fwd then s.dna.arr else s.dna.RevComp().arr;
                 align = ANCHOR.LEFT;
                 strand = STRAND.TOP;
                 offset = 0;
                 targetTemp = dp.targetTm; // was historically seamlessTm incorrectly till ~ 10/2017
                 sequencePenalties = None}
            pdWrap false pen task
    /// Called on success at finding fwd and reverse primers that can amplify the adjacent regions
    /// Now tune the tails so that the internal overlap meets spec
    let success (f:Oligo) (r:Oligo) =
        //                       -------> (fwd)
        //   ===================|====================
        //          <----------- (rev)
        //
        let seamlessOptVerbose = false
        let rec optOverlapTm fLen rLen bestF bestR bestDelta =
            if rLen > r.oligo.Length || fLen > f.oligo.Length then
                if seamlessOptVerbose then
                    printfn "seamlesstmopt done (no more runway) fLen=%d rLen=%d target=%A bestDelta=%A"
                        bestF bestR dp.seamlessOverlapTm bestDelta
                bestF, bestR // Ran out of runway
            else
                let overlapOligo = Array.concat [ revComp (r.oligo.[..rLen-1]) ; f.oligo.[..fLen-1] ]
                let tm = primercore.temp dp.overlapParams overlapOligo overlapOligo.Length
                let delta = abs(tm - dp.seamlessOverlapTm)
                if seamlessOptVerbose then
                    printfn "seamlesstmopt fLen=%d rLen=%d target=%A delta=%A bestDelta=%A tm=%A"
                        fLen rLen dp.seamlessOverlapTm delta bestDelta tm

                if delta > bestDelta+3.0<C> && (fLen+rLen>=dp.overlapMinLen) then
                    // Reached the end and have a worse design
                    if seamlessOptVerbose then
                        printfn "seamlesstmopt done fLen=%d rLen=%d target=%A delta=%A bestDelta=%A tm=%A"
                            bestF bestR dp.seamlessOverlapTm delta bestDelta tm
                    bestF,bestR
                else
                    let bestF',bestR',bestDelta' =
                        if delta<bestDelta then fLen,rLen,delta else bestF,bestR,bestDelta
                    if rLen < fLen then
                        optOverlapTm fLen (rLen+1)   bestF' bestR' bestDelta'
                    else
                        optOverlapTm (fLen+1) rLen   bestF' bestR' bestDelta'
        // ------------ End optOverlapTm --------------------------

        // Get optimal tail lengths ( ~~~s below) to get the right internal seamlessoverlaptm
        let bestF,bestR =
            optOverlapTm
                (min (dp.overlapMinLen/2) f.oligo.Length)
                (min (dp.overlapMinLen/2) r.oligo.Length)
                f.oligo.Length
                r.oligo.Length
                9999.0<C>

        //
        //                   >~~~~~~~~~  o----------->  fwd
        //    ======================== | ==============================
        //            <---------------o  rev
        //                               ~~~~~~~~~<
        //
        let fwd = {tail = Dna(revComp (r.oligo.[..bestR-1])); body = Dna(f.oligo); annotation = []}
        let rev = {tail = Dna(revComp (f.oligo.[..bestF-1])); body = Dna(r.oligo); annotation = []}

        let leftOverlap = min fwd.tail.Length rev.body.Length
        let rightOverlap = min fwd.body.Length rev.tail.Length

        let fwd' = {fwd with annotation =
                                [{il = fwd.tail.Length-leftOverlap;
                                  ir=fwd.tail.Length+rightOverlap-1;
                                  iType = DNAIntervalType.ANNEAL}]}
        let rev' = {rev with annotation =
                                [{il = rev.tail.Length-rightOverlap;
                                  ir=rev.tail.Length+leftOverlap-1;
                                  iType = DNAIntervalType.ANNEAL}]}

        fwd', f.offset, rev', r.offset
    // ------ End success function definition ------------------------------------------------

    match (designSingle next true maxFwd), (designSingle prev false maxRev) with
    | None,None ->
        failwithf "failed primer design for seamless %A::%A" prev next // Both failed
    | Some(f),Some(r) -> success f r // Both succeeded
    | Some(f),None ->
        // See if we can do better using leftover bases from fwd design
        match designSingle prev false (dp.pp.maxLength-f.oligo.Length) with
        | None ->
            failwithf "failed primer design for seamless %A::%A - prev " prev next
        | Some(r) -> success f r
    | None,Some(r) ->
        // See if we can do better using leftover bases from rev design
        match designSingle next true (dp.pp.maxLength-r.oligo.Length) with
        | None ->
            failwithf "failed primer design for seamless %A::%A - next \n" prev next
        | Some(f) -> success f r
// ^^ End Seamless design ---------------------------------------------------------------------------------------------
/// Modified version that doesn't take sandwich length into account while designing the amp primer
let linkerFwd2 verbose (dp:DesignParams) errorName (next:DNASlice) =
    let rec linkerFwd2Iterative pen margin =
        let x =
            if next.dna.Length < 10 then
                if verbose then
                    printf "WARNING: template dna (%s) (len=%d) %O too short for succcessful primer design\n"
                        next.description next.dna.Length next.dna
                None
            else
                if next.sourceFrApprox then
                    let task =
                       {tag ="PF";
                        temp = next.dna.[0..min (next.dna.Length-1) margin].arr;
                        align = ANCHOR.CENTERLEFT;
                        strand = STRAND.TOP;
                        offset =0;
                        targetTemp = dp.targetTm;
                        sequencePenalties = None}
                    primercore.oligoDesignWithCompromise false pen task
                else
                    let task =
                       {tag = "PF";
                        temp = next.dna.arr;
                        align = ANCHOR.LEFT;
                        strand = STRAND.TOP;
                        offset = 0;
                        targetTemp = dp.targetTm;
                        sequencePenalties = None}
                    if pen.maxLength < pen.minLength then
                        failwithf "oMax=%d<oMin=%d  in linkerFwd2" pen.maxLength pen.minLength
                    primercore.oligoDesignWithCompromise false pen task
        match x with
        | None ->
            if margin < 3*approxMargin then linkerFwd2Iterative pen (margin+10)
            else failwithf "failed primer design for design %s in linkerFwd2" errorName
        | Some(oligo) ->
            // Oligo design might have chopped off DNA , so cut accordingly
            primerCheck oligo.oligo (next.dna.[oligo.offset..].arr)
            // Annotation regions for the oligo
            {tail = Dna(""); body = Dna(oligo.oligo); annotation = []}, oligo.offset

    linkerFwd2Iterative dp.pp (2*approxMargin)


/// Modified version that doesn't take sandwich length into account while designing the amp primer
/// Design reverse linker into upstream slice.  Returns primer and amount to chop off upstream
/// slice if it was an approx slice
let linkerRev2 verbose (dp:DesignParams) errorName (last:DNASlice option) =
    // Recursively try different compromises on how much we eat into the
    // linker length and extend the approximate region for the primer.  If
    // we fail, try eating more and roaming more widely

    let rec linkerRev2Iterative pen margin =
        match last with
        | None ->
            if verbose then
                printfn "procAssembly: linkerFwd2: entering with margin=%d last=None" margin
            { body = Dna(""); tail=Dna("") ; annotation=[]},0
        | Some(ds) ->
            if verbose then
                printfn "procAssembly: linkerFwd2: entering with margin=%d last=%s sourceToApprox=%s"
                    margin ds.description (if ds.sourceToApprox then "y" else "n")
            let x,task =
                if ds.sourceToApprox then
                    let task =
                       {tag = "PR";
                        temp = ds.dna.RevComp().[0..min (ds.dna.Length-1) margin].arr;
                        align = ANCHOR.CENTERLEFT;
                        strand = STRAND.TOP;
                        offset = 0;
                        targetTemp = dp.targetTm;
                        sequencePenalties  = None}
                    if verbose then
                        printf "procAssembly: linkerRev: oligo design approx\n template=%s\npen=%A\n"
                            (arr2seq task.temp) pen
                    primercore.oligoDesignWithCompromise false pen task,task
                else
                    let task =
                       {tag = "PR";
                        temp = ds.dna.RevComp().arr;
                        align = ANCHOR.LEFT;
                        strand = STRAND.TOP;
                        offset = 0;
                        targetTemp = dp.targetTm;
                        sequencePenalties  = None}
                    if verbose then
                        printf "procAssembly: linkerRev: oligo design non approx\n template=%s\npen=%A\n"
                            (arr2seq task.temp) pen
                    primercore.oligoDesignWithCompromise false pen task,task
            match x with
            | None ->
                if margin < 3*approxMargin then linkerRev2Iterative pen (margin+10)
                else failwithf "failed primer design in linkerRev2 for design %s\nlast task was =%A" errorName task
            | Some(oligo) ->
                primerCheck oligo.oligo (ds.dna.RevComp().[oligo.offset..].arr)
                {tail = Dna(""); body = Dna(oligo.oligo); annotation = []}, oligo.offset


    linkerRev2Iterative dp.pp (2*approxMargin)

/// Chop bases off a primer whose tail is a linker until it meets the length requirement
let trimLinkerTailBody (dp:DesignParams) (p:Primer) =
    let tailTargetTM = if p.tail.Length=0 then 0.0<C> else temp dp.pp p.tail.arr p.tail.Length
    let bodyTargetTm = dp.targetTm

    let rec find bLen tLen =
        if bLen+tLen<=dp.pp.maxLength || (bLen=dp.pp.minLength && tLen=dp.pp.minLength) then
            {p with body=p.body.[..bLen-1]; tail = p.tail.[p.tail.Length-tLen..]}
        else
            // Someone needs to lose a basepair
            if bLen = dp.pp.minLength then find bLen (tLen-1)
            elif tLen = dp.pp.minLength then find (bLen-1) tLen
            else
                let bLen' = bLen - 1
                let tLen' = tLen - 1
                let bTm = temp dp.pp p.body.arr bLen'
                let tTm = temp dp.pp (p.tail.[p.tail.Length-tLen'..].arr) tLen'
                if abs(tTm-tailTargetTM) < abs(bTm-bodyTargetTm) then
                    // Pick on tail, it's not doing so bad
                    find bLen (tLen-1)
                else
                    // Pick on body, it's not doing so bad
                    find (bLen-1) tLen
    find p.body.Length p.tail.Length

let nameFromSlice (s:DNASlice) =
        if s.description.Length > 25 then
            sprintf "%s..[%d]" (s.description.[..24]) s.description.Length
        else
            s.description

let checkSliceCoods(slice:DNASlice) n =
    if slice.dna.Length-1-n<0 then
        failwithf "in cutX dna slice %s length is %d, cut is %d"
            slice.description  slice.dna.Length n

    if slice.sourceFr > slice.sourceTo then
        failwithf "slice %s has sourceFwd=Y sliceFr=%d sliceTo=%d.  sliceFr should be less than sliceTo"
            slice.description
            slice.sourceFr
            slice.sourceTo

// Two functions for cutting left or right into a slice, used when those slice ends were approximate
// and primer design ended up chopping into them.
let cutRight verbose (slice:DNASlice) n =
    checkSliceCoods slice n
    // work out which end of the original source coordinate to adjust based on orientation of the
    // part.  cut 'right' refers to right end of part as placed.
    if verbose then
        printfn "procAssembly: cutRight moving sourceTo for %s left by %d sourceFwd=%s destFwd=%s"
            slice.description n (if slice.sourceFwd then "Y" else "N") (if slice.destFwd then "Y" else "N")
    if (slice.sourceFwd && slice.destFwd) ||
        ((not slice.sourceFwd) && (not slice.destFwd))
    then
        {slice with
               sourceTo = slice.sourceTo - (n*1<ZeroOffset>);
               dna = slice.dna.[0..slice.dna.Length-1-n]}
    else
        {slice with
               sourceFr = slice.sourceFr + (n*1<ZeroOffset>);
               dna = slice.dna.[0..slice.dna.Length-1-n]}

let cutLeft verbose (slice:DNASlice) n =
    checkSliceCoods slice n

    // work out which end of the original source coordinate to adjust based on orientation of the
    // part in the genome.  cut 'left' refers to left end of part as placed.
    // sourceFr and sourceTo are the 5' and 3' ends of the gene

    if verbose then
        printfn "procAssembly: cutLeft moving sourceFr for %s right by %d sourceFwd=%s destFwd=%s"
            slice.description n (if slice.sourceFwd then "Y" else "N") (if slice.destFwd then "Y" else "N")
    if (slice.sourceFwd && slice.destFwd) ||
       ((not slice.sourceFwd) && (not slice.destFwd)) then
        {slice with
               sourceFr = slice.sourceFr + (n*1<ZeroOffset>);
               dna = slice.dna.[n..slice.dna.Length-1]}
    else
        {slice with
               sourceTo = slice.sourceTo - (n*1<ZeroOffset>);
               dna = slice.dna.[n..slice.dna.Length-1]}

/// Recursively process an assembly, tracking the previous and remaining DNA slices
/// emitting an output set of DNA slices and a diverged primer pair list
let rec procAssembly
        verbose
        /// Needed for primer design
        (dp:DesignParams)
        /// Name to use in any things that blow up (passed in from parent)
        errorName
        (prev : DNASlice list)
        /// Emitted slices
        sliceOut
        /// Primer sets we have emitted.  They come in fwd/rev pairs (both of which can be None at ends)
        (primersOut:DivergedPrimerPair list)
        /// Remaining slices to process
        (l:DNASlice list) =

    (*
What is the difference between prev and sliceOut?
The short answer is that sometimes when we process a slice we aren't ready to emit it
(we don't know what has to happen till we see future pieces).  An example is a slice with a fuzzy / approximate end.
The compiler captures the longest possible piece of DNA but when we hit the next slice (typically a linker)
we do primer design and might chop the previous piece down (and then spit it out).  The reason it isn't always
just the last slice we handed is that we also hit things like virtual slices so the previous slice we
can design (say) a primer against might be further back in the stack and it's just easier to explicitly keep that prev unemitted slice
    *)
    if verbose then
        printfn ""
        printfn "procAssembly: ==========================================================================================="
        printfn "procAssembly: TOP,  prev=[%s]\n                    n=[%s]\n                    l=[%s]\n            slice out=[%s]"
            (String.Join(";",(prev |> Seq.map(nameFromSlice))))
            (String.Join(";",(l |> Seq.map(nameFromSlice))))
            (String.Join(";",(primersOut |> Seq.map(prettyPrintPrimer))))
            (String.Join(";",(sliceOut |> Seq.map(nameFromSlice))))
        printfn "procAssembly: ==========================================================================================="
        printfn ""

    /// Include head of prev in slicesOut if prev isn't None (we delayed emission at time of processing but now
    /// need to send it out).
    let incPrev (prev: DNASlice list) (slices : DNASlice list) =
        if verbose then
            printfn "Called incPrev"

        match prev with
        | [] -> slices
        | p::_ -> // There is a head of the prev
            if verbose then
                printfn "Adding %s" p.dna.str
            p::slices // add it to the out slices passed in (on front - we are pushing LIFO)

    // Main match for remaining cases
    match l with
    | [] ->
        // No remaining slices - everything processed.
        let sliceOut' = sliceOut // daz  (I think this is now reincluding a slice)  //    incPrev prev sliceOut |> List.rev
        if verbose then
            printfn "procAssembly: PACASE 0 - done with slices"
        // since we made primers and sliceOut with LIFO model, need to reverse them now
        List.rev primersOut,List.rev sliceOut' // reverse the primers and the slices since we pushed as we built
    | hd::next::tl when hd.sliceType = FUSIONST && next.sliceType=INLINEST ->
        // Fusing a slice to a following inline sequence is redundant as that's the
        // strategy we employ by default, but it will mess things up to try to put a seamless stitch here into a possibly
        // small DNA sequence, so just ignore FUSION directive
        if verbose then
            printfn "procAssembly: PACASE 1 - skipping redudant FUSIONST/INLINEST (skip)"
        // hd was a vitual part (no sequence), so we skip adding it to the sliceOut and add next instead
        // also doesnt change prev - last real slice is still head of prev (since hd is virtual)
        procAssembly verbose dp errorName prev sliceOut primersOut (next::tl)
    | hd::next::tl when hd.sliceType = FUSIONST ->
        // Slice hd is a fusion slice, which is virtual, it exists only to mark
        // the intention to fuse prev and next together
        if verbose then
            printfn "procAssembly: PACASE 2 - FUSIONST - generating primers (DPP)"
        if prev = [] then failwith "INTERNAL ERROR: unexpected prev = [] in procAssembly\n"
        // make a seamless junction with head of prev (last slice we can design into) and next (next real
        // slice after the virtual fusion marker
        let primerF,offsetF,primerR,offsetR = seamless verbose dp (List.head prev) next

        // If we stitched fwd/rev off of linker hd then the previous and next elements (prev) and next
        // might need to be modified (chopped) if the ends were flexible
        if verbose then printfn "procAssembly: fusion slice, cutting offsetF=%d offsetR=%d" offsetF offsetR

        //            (empty/virtual)
        //  prev .... hd(fusionst) .. next .. tl...
        //  <---------------------------
        //     ------------------------->

        // We designed into prev, and potentially shortened it if its end was approximate / flexible
        // so now we can actually emit the head of prev into the sliceOut (chopped potentially)
        // sliceOut' is the updated version with chopped prev on the head

        let sliceOut' = match prev with | [] -> sliceOut | p::_ -> (cutRight verbose p offsetR)::sliceOut

        procAssembly
            verbose
            dp
            errorName
            (hd::prev) // hd becomes the next prev.  DP: Not sure how helpful this is since it's virtual..
            sliceOut' // updated sliceOut with prev emitted
            (DPP({fwd = primerF ; rev = primerR ; name = next.sliceName})::primersOut)
            ((cutLeft verbose next offsetF)::tl) // Remove bases from the next slice if we moved the primer

    // linker::next
    // or /inline/ (but not rabit start/end) :: next
    // AND.. inline is after a previous slice that can be designed against
    | hd::next::tl when
        (hd.sliceType = LINKER) ||
        (hd.sliceType =
            INLINEST && // Actual mid rabit inline slice not one at the end of a rabit
            not (       hd.pragmas.ContainsKey("rabitend") ||
                        hd.pragmas.ContainsKey("rabitstart") ||
                        hd.pragmas.ContainsKey("amp") // if directed to amplify, don't do as an inline
            )
         ) && prev <> [] // must be a previous slice to do an inline
        ->

        if verbose then
            printfn "procAssembly: PACASE 3 - LINKER or inline not rabitstart/end"

        // If the inline is short enough (should probably check also for #inline pragma that forces inline)
        if hd.sliceType = INLINEST && hd.dna.Length < 12 then
            if verbose then
                printfn "procAssembly: ... shortcase (DPP)"
            // SHORTINLINE Case
            //
            // Special case for short inline sequences.  We can sort of do a seamless design
            // pretending there isn't a sequence in the middle, then insert the middle part back
            // in
            // Seamless starting material
            //                    >>>>>         >>>>>>>>
            //  prev ppppppppppppppppp iiiiiiii nnnnnnnnnnnnnnn next
            //                    <<<<<         <<<<<<<<<
            //
            // adjusted to include internal sequence
            //                          >>>>>>>>>>>>>>>>
            //  prev ppppppppppppppppp iiiiiiii nnnnnnnnnnnnnnn next
            //                    <<<<<<<<<<<<<<

            // head of prev becomes what we design against on the left
            // next is what we design against on the right
            let primerF,offsetF,primerR,offsetR = seamless verbose dp (List.head prev) next

            //
            // They can instruct us to keep the forward or reverse primers past a particular point.  If
            // so, we might need to truncate the primers a little
            //
            //
            //  #primerpos FWD 5   implies fwd primer should start from the 5th base relative to the start of the sandwich segment
            //                     >>>>>>>>>>X>>>>>>>>>>>
            //  prev ppppppppppppppppppp iiiiiii nnnnnnnnnnnnnnn next
            // primerpos is useful for enzyme library designs where they want a fixed primer on one side of a lot of designs
            // to ensure consistent primer pair overlap in mixtures (and reduce primers)
            //
            // Parse any primer position directive and determine cosntraints on lengths
            let fwdTailLenMax,revTailLenMax =
                match parsePrimerPos hd.pragmas with
                | FWD(offset) ->
                    let x = (hd.dna.Length+1)-offset |> max 0// Convert to tail length
                    x, 999999
                | REV(offset) ->
                    let x = offset |> max 0
                    999999,x
                | NONE -> 999999,999999 // no primerpos

            if verbose then
                printf "primerpos directive: fwdTailLenMax = %A revTailLenMax = %A\n" fwdTailLenMax revTailLenMax

            // Tail consists of internal sequence and reverse oligo concatenated.
            // This is messed up - note - double recomp :)
            //
            //  prev ppppppppppppppppp hdhdhdhdh nnnnnnnnnnnnn next
            //                <<<<<<<<<<<<<<<<<<
            //                revbody  hd.RevComp
            //                fwdRunway.........>
            //
            let fwdRunway = DnaOps.concat([hd.dna.RevComp(); primerR.body]).RevComp()

            // Up to how many bases can we use for tail?
            let fwdTailLen =
                min fwdRunway.Length (dp.pp.maxLength - primerF.body.Length)
                |> min fwdTailLenMax // restrict if needed

            // Define the forward primer using the primerF into the body and the fwdRunway cut
            // to the length of the reverse primer
            // Annotation regions for the oligo

            //                              X>>>>>>>>>>>>>>
            //  prev ppppppppppppppppppp iiiiiii nnnnnnnnnnnnnnn next
            //                    <<<<<<<<<<<<<<<<<<<<<<<<<
            // Rev tail consists of internal sequence and fwd oligo concatenated
            let revRunway = DnaOps.concat([hd.dna; primerF.body]).RevComp()
            let revTailLen =
                min revRunway.Length (dp.pp.maxLength - primerR.body.Length)
                |> min revTailLenMax
            // Amplification part of fwd primer
            let a1F = {il = fwdTailLen; ir=fwdTailLen+primerF.body.Length-1; iType = DNAIntervalType.AMP}
            // Overlapping anneal section of fwd primer
            let a2F = {il = 0; ir= fwdTailLen-1+revTailLen-hd.dna.Length; iType = DNAIntervalType.ANNEAL}

            let primerF' =
               {tail = fwdRunway.[fwdRunway.Length-fwdTailLen..fwdRunway.Length-1];
                body = primerF.body; annotation = [a2F; a1F]}
            // Amplification part of rev primer
            let a1R = {il = revTailLen; ir=revTailLen+primerR.body.Length-1; iType = DNAIntervalType.AMP}
            // Overlapping anneal section of rev primer
            let a2R = {il = 0; ir= fwdTailLen-1+revTailLen-hd.dna.Length; iType = DNAIntervalType.ANNEAL}

            assert (a2F = a2R) // Annealing regions should be the same length and range
            let primerR' =
               {body = primerR.body;
                tail = revRunway.[revRunway.Length-revTailLen..revRunway.Length-1];
                annotation = [a2R ; a1R]}

            if verbose then printfn "short inline case, cutting offsetR=%d" offsetR

            // chop the head of prev (last slice that we can design into) now we have potentially shortened it (if
            // it had an approximate end),  then move it over to the slicesOut  generating sliceOut'
            //        <------(chop this bit) -------------
            //  prevprevprevprevprevprevprev hdhdhdhdhdhdh nextnextnext
            //                                  ---------- ------>
            let sliceOut' = match prev with | [] -> sliceOut | p::_ -> (cutRight verbose p offsetR)::(List.tail sliceOut)

            // Note: next is also potentially chopped a bit if its leading edge is an approximate end.  Next will
            // include all the possible bases but primer design could have done this
            //
            //  prevprevprevprevprevprevprev hdhdhdhdhdhdh nextnextnext
            //                                  ---------- (chopped) ------>
            // this is taken
            let next' = ((cutLeft verbose next offsetF)::tl) // Remove bases from the next slice if we moved the primer

            assert(primerF'.lenLE(dp.pp.maxLength))
            assert(primerR'.lenLE(dp.pp.maxLength))


            procAssembly
                verbose
                dp
                errorName
                (hd::prev) // hd (inline sequence) becomes next head of prev sequence
                (hd::sliceOut') // emit hd and sliceOut directly into slicesOut (we aren't going to chop hd more)
                (DPP({fwd = primerF'; rev = primerR'; name = "shortinline"+hd.sliceName})::primersOut)
                next' // Remove bases from the next slice if we moved the primer
        else // phew..
            // (as an aisde, this match case violates every last thing I was taught in school about reasonable function sizes).
            // Probably worth breaking up and writing damn unit tests against cases but be careful to not break tail recursion

            // In this case the inline sequence is too long (we should also look for the #amp pragma technically to
            // force this case.  We are going to make the inline via PCR rather than just try to make it with a pair
            // of primers
            // so we have these for parts
            //
            // hd(long inline not rabit start/end)::next::tail..
            // OR.....
            // hd(linker)::next::tail..

            if verbose then
                printfn "procAssembly: ... longcase (DPP)"
            // LONGINLINE Case
            //
            // Regular inline case or linker case - just design fwd and reverse then prepend
            // leading sequence (inline or linker)
            //
            //     hd (inline/linker) :: next :: tail....
            //     ------------------------->
            // <--------------------
            //
            // Wrinkle - the fwd primer might go through an inline sequence, so we need to
            // handle that case and really design against the inline sequence two positions downstream.
            //
            //
            //     hd (inline/linker) :: next :: tail....
            //     ---------------------(sandw)..---->  (primer needs to be designed against tail not next)
            // <--------------------
            // c,d and e are ....
            // c (sandwichF) is the sandwich slice if any
            // d   is the actual "next" slice we design the primer into
            // e   are the remaining slices for further processing
            //    hd    ::    c   ::   d    ::e..
            //    --------------------->
            let sandwichF, d, e =
                if next.sliceType = INLINEST then
                    if verbose then
                        printfn "procAssembly: ... next slice is INLINEST, so sandwich case" // hd is linker, next is short inline..
                    match tl with
                    | [] -> failwithf "expected next dnaslice preparing for linkerFwd design\n"
                    | tlhd::tltl -> Some(next),tlhd,tltl
                else
                    if verbose then
                        printfn "procAssembly: ... not sandwich case hd=sliceType %A (%s), next sliceType=%A (%s)"
                            hd.sliceType
                            hd.sliceName
                            next.sliceType // e.g. hd = inline
                            next.sliceName
                    None,next,tl

            // FWD primer creation
            //     -------------------------->
            //     linkerorslice iiiiiii ffffff
            //let primerF',offsetF = linkerFwd dp errorName hd c d   // linker sandwich next

            // Design primer first into forward DNA segment, ignoring any sandwich slice for now
            let primerF', offsetF = linkerFwd2 verbose dp errorName d

            // Complication - when we design a reverse primer, the immediately preceding slice
            // might just be a small inline (sandwich) slice, and we really want to reach back
            // even further to find an actual regular slice we can design the primer into.
            // Inspect prev and pull out (a) a possible sandwich slice and (b) the regular slice
            // to design primer against
            // <<<<<<<<------------------0
            //   b  :: inline (a) :: LINKER ..
            //
            // skipped marks if we had to step over an inline/sandwich slice
            let sandwichR, b, skipped =
                match prev with
                | hd1::hd2::_ when hd1.sliceType = INLINEST -> // hd1 needs to be skipped and hd2 is the slice to design again - sucks if there was more than 1 small slice :(
                    // skipped is set if we had to reach over a slice
                    Some(hd1),Some(hd2),true // yes we skipped hd1, design agaisnt hd2
                | hd::_ -> None,Some(hd),false // no sandwich, just design against hd // yay F# matching.. BTW
                | [] -> None,None,false // nothing on left hand side to design again

            // REV primer creation.  If we need to go back through b then skipped is set to true
            // when b is a small inline slice
            // <-----------------------
            //          linkerorinline
            //   sandwichR :: b ::    hd
            let primerR', offsetR = linkerRev2 verbose dp errorName b // Design into next non sandwich segment first

            // Our scheme at the moment has produced fwd/rev primers that overlap exactly the
            // length of the hd slice which is either a linker or an inline slice.
            // This can be too long/short in case of inline sequences.  Get the up/downstream
            // sequences and fine tune the primers in the case of inline sequences to target the seamlesstm

            /// First get the tail of the primer we are proposing
            let revTail =
                match sandwichR with
                | None -> hd.dna.RevComp() //  hdhdhdhdhdhdhd // just the head
                | Some(s) ->
                    // put together the hd and sandwich dna for the primer tail (non amplification region).  Get right orientation
                    DnaOps.append s.dna hd.dna |> DnaOps.revComp

            // Build data for tuneTails.  For the purpose of assignment, body is the part that
            // amplifies the neighbouring regions.
            // Any sandwich and linker DNA goes into the tail

            // ----------|--------|----->
            // overlap   sandwich  amplification (body)
            // tail--------------->

            // TUNING STEP

            let fwdArg = {primerF' with tail = (if primerF'.body.Length>0 then hd.dna else Dna(""))}
            let revArg = {primerR' with tail = (if primerR'.body.Length>0 then revTail else Dna(""))}

            // Mildly evil setup.  We can have one or two sandwich pieces on the side, and
            // fwd/reverse starting primers crossing them.
            // Set up the "middle" dna sequence and primer tails covering them by default

            (* We handle the optimization like this since the fb part has to amplify the
               adjacent regions and the sandwich and middle contribute to annealing but not amplification.
               However, when we make the final construct for thumper, the sandwich region goes into the body not
               the tail since the tails are checked against linkers and the sandwich sequence ends up in the rabit
               DNA sequence.  What a mess.. :)

            //               ftftftftftftftftftf  fbfbfbfbfbf
            //    sandwichR ; middle ; sandwichF ; >>>>>
            // rbr<rtrtrtrtrtrtrtrtrtrt
            *)

            // create rev body, middle, fwd body and length of sandwich regions for rev and fwd
            let revArg', midArg, fwdArg', lenSR, lenSF =
                match sandwichR,sandwichF with
                | None,None -> revArg, hd.dna, fwdArg, 0,0
                | Some(r),None ->
                   ({revArg with tail = DnaOps.append r.dna hd.dna |> DnaOps.revComp},
                    DnaOps.append r.dna hd.dna,
                    fwdArg,
                    r.dna.Length,
                    0)
                | None,Some(f) ->
                   (revArg, DnaOps.append hd.dna f.dna,
                    {fwdArg with tail = DnaOps.append hd.dna f.dna},
                    0,
                    f.dna.Length)
                | Some(r),Some(f) ->
                   ({revArg with tail = DnaOps.append r.dna hd.dna |> DnaOps.revComp},
                    DnaOps.concat [r.dna; hd.dna; f.dna],
                    {fwdArg with tail = DnaOps.append hd.dna d.dna},
                    r.dna.Length,
                    f.dna.Length)

            // Are there any restrictions of the positions/lengths of the fwd/rev tails
            // If the middle part is a ryse linker, then we should fix the ends.
            // todo: if there's a FWD/REV pos pragma, we might also want to set things
            let isLinker = (hd.sliceType = LINKER)

            // Two strategies possible here.  Fix the middle part if it's a linker.
            // Always generate full length linkers.
            // Alternatively we can try to target the linker tm but compromise and chew it back
            // a little if we run out of linker
            let linkerChew = true // ok to reduce length of linker region slightly
            let round (f:float) = (f+1.0) |> int
            let (fwdTailLenFixed,
                 fwdTailLenMin,
                 fwdTailLenMax,
                 firmMiddleTm,
                 revTailLenFixed,
                 revTailLenMin,
                 revTailLenMax) =
                // 50 // Don't try to cover more than 30 bp of a sandwich fragment no matter how long it is
                let maxSandwichLength = 999999
                //                 [--------------]
                //     <--------------------------
                //     SRSRSRSRSR< hddnahddnahddnahddna > SFSFSFSFSFSF
                //                          ------------------------->
                //                          [------------]
                // What is the minimum amount each primer tail must contribute to the central hd.dna piece?
                let minInlineOverlap = 12
                let midOverlapContribution = (hd.dna.Length * 2 - minInlineOverlap)/2 |> max 0
                let fwdMiddleLen = (min lenSF maxSandwichLength)+midOverlapContribution
                let revMiddleLen = (min lenSR maxSandwichLength)+midOverlapContribution
                if hd.dna.Length = 0 then
                    (None,
                     0,
                     Microsoft.FSharp.Core.int.MaxValue,
                     None,
                     None,
                     0,
                     Microsoft.FSharp.Core.int.MaxValue) // Hack to support Mt (empty) linkers
                elif not linkerChew then
                    // Length of fwd linker is fixed to RYSE + fwd sandwich if linker
                    let fwdTailLenFixed = if isLinker then Some(fwdMiddleLen) else None
                    // Length of rev linker is fixed to RYSE+rev sandwich if linker
                    let revTailLenFixed = if isLinker then Some(revMiddleLen) else None
                    (fwdTailLenFixed,
                     fwdMiddleLen,
                     Microsoft.FSharp.Core.int.MaxValue,
                     None,
                     revTailLenFixed,
                     revMiddleLen,
                     Microsoft.FSharp.Core.int.MaxValue)
                elif isLinker then
                    let linkerTm = temp dp.pp hd.dna.arr hd.dna.Length
                    ///beginning of linker.
                    let fwdTailLenMax = midArg.Length - lenSR
                    ///end of linker.
                    let revTailLenMax = midArg.Length - lenSF
                    //None,((0.8 * float fwdMiddleLen)|>round),fwdTailLenMax,Some(linkerTm),None,((0.8 * float revMiddleLen)|>int),revTailLenMax
                    let revTailLenMin =
                        max revMiddleLen hd.dna.Length  // Estimate of middle overlap length but since linker, don't go under 80% of actual linker
                        |> float |> (*) 0.80 |> int

                    (None,
                     ((0.8 * float fwdMiddleLen)|>round),
                     fwdTailLenMax,
                     Some(linkerTm),
                     None,
                     revTailLenMin,
                     revTailLenMax)
                else
                    // let linkerTm = temp dp.pp hd.dna.arr hd.dna.Length
                    // changing this because if the inline sequence is really short, that will become the target overlap
                    // could do a min value for this, but leaving it to default seamlessTm
                    (None,
                     ((0.8 * float fwdMiddleLen)|>round),
                     Microsoft.FSharp.Core.int.MaxValue,
                     None, // Some(linkerTm),
                     None,
                     ((0.8 * float revMiddleLen)|>int),
                     Microsoft.FSharp.Core.int.MaxValue)

            if fwdTailLenMin > dp.pp.maxLength then
                failwithf "%s: required primer fwd primer tail length %d exceeds primer max length of %d"
                                errorName fwdTailLenMin dp.pp.maxLength
            if revTailLenMin > dp.pp.maxLength then
                failwithf "%s: required primer revfwd primer tail length %d exceeds primer max length of %d"
                                errorName revTailLenMin dp.pp.maxLength

            // --------------------------------------------------------------------------------
            // main show - play with all the primer lengths to get optimal forward and reverse primers
            // --------------------------------------------------------------------------------
            let primerF,primerR =
                tuneTails
                    verbose
                    dp
                    fwdTailLenFixed
                    fwdTailLenMin
                    fwdTailLenMax
                    firmMiddleTm
                    revTailLenFixed
                    revTailLenMin
                    revTailLenMax
                    fwdArg'
                    revArg'
                    midArg

            // One final little snag with building the primers for RYSE.   Although we included sandwich regions in the
            // tail for optimization purposes, we now put them into the *body* since that's the convention followed in thumper.  Ideally we'd model 3 part linkers
            // since body is strictly for amplification,  tail should be for overlap with neighnor and sandwich is along for the ride..
            // Transfer some bases from the tails to the body if necessary

            let primerF =
                if lenSF=0 then primerF
                else {primerF with
                              body = DnaOps.append primerF.tail.[primerF.tail.Length-lenSF|> max 0..] primerF.body;
                              tail = primerF.tail.[..primerF.tail.Length-lenSF-1|> min (primerF.tail.Length-1)]}
            let primerR =
                if lenSR=0 then primerR
                else {primerR with
                              body = DnaOps.append primerR.tail.[primerR.tail.Length-lenSR..] primerR.body;
                              tail = primerR.tail.[..primerR.tail.Length-lenSR-1]}
            // HACK FIXFIX - turning off for linkerless adventure 20140820
            //assert ( primerF.tail.Length >= fwdTailLenMin)
            //assert (primerR.tail.Length = 0 || primerR.tail.Length >= revTailLenMin)

            if not (primerF.lenLE(dp.pp.maxLength)) then
                failwithf "for %s primer design violates length constraint in procAssembly primerF %d not <= %d for %O"
                    errorName primerF.Primer.Length dp.pp.maxLength primerF.Primer
            if not (primerR.lenLE(dp.pp.maxLength)) then
                failwithf "for %s primer design violates length constraint in procAssembly primerR %d not <= %d for %O"
                    errorName primerR.Primer.Length dp.pp.maxLength primerR.Primer
            assert(primerR.lenLE(dp.pp.maxLength))

            // Ensure primers overlap
            // --------------------------------------------------

            // If we stitched fwd/rev off of linker hd then the previous and next elements (prev) and next
            // might need to be modified (chopped) if the ends were flexible.  Calculate the new lengths/positions of the
            // ends of the adjacent slices.
            //
            // chop the head of prev (last slice that we can design into) now we have potentially shortened it (if
            // it had an approximate end),  then move it over to the slicesOut  generating sliceOut'
            //        <------(chop this bit) -------------
            //  prevprevprevprevprevprevprev hdhdhdhdhdhdh nextnextnext (d)
            //                                  ---------- ------>
            // Note: next is also potentially chopped a bit if its leading edge is an approximate end.  Next will
            // include all the possible bases but primer design could have done this
            //
            //  prevprevprevprevprevprevprev hdhdhdhdhdhdh nextnextnext (d)
            //                                  ---------- (chopped) ------>
            // this is taken

            if verbose then
                printfn "regular inline case hd=%s, cutting offsetR=%d" hd.description offsetR
                printfn "procAssembly: prepping for sliceOut' skipped=%s sliceOut=%s"
                    (if skipped then "yes" else "no")
                    (String.Join(";",[for x in sliceOut -> x.description]))
            let sliceOut' =
                match prev with // look at previous slices to consider sliceOut update
                | [] -> sliceOut // no changes to sliceout
                | p1::p2::tl when skipped -> // skipped means we had to reach over a small inline while generating reverse primer
                    if verbose then
                        printfn "cutRight p1=%s p2=%s offsetR=%d sliceOut=%A"
                            p1.description p2.description offsetR sliceOut
                    // take p2 (penultimate previous) and cut it by the offset of the primer from the floating end.  Keep p1, the inline slice and rest of sliceOut beyond first

                    // is List.tail sliceOut correct?  Shouldn't is be skipping two previous slices FIX? BUG?

                    // new sliceout starts with p1, the small skipped inline slice, then p2 cut to reflect the primer adjusted boundary
                    // finally we include the remainder of the slice out
                    p1::(cutRight verbose p2 offsetR)::tl
                | p::_ -> // simple prev case, cut to any offset we generated
                    if verbose then printfn "cutRight p=%s offsetR=%d" p.description offsetR
                    //(cutRight p offsetR)::sliceOut
                    (cutRight verbose p offsetR)::(List.tail sliceOut)

            let choppedD =  // The next slice might have a flexible end in which case we need to respect where the primer chopped it
                            let chopped = cutLeft verbose d offsetF
                            if verbose then
                                printfn "chop d %s to %d\ndpre=%s\ndPost=%s"
                                    d.description
                                    offsetF
                                    d.dna.str
                                    chopped.dna.str
                            chopped

            // --------------         new prev
            match sandwichF with
            | None ->
                // If no sandwich sequence, hd was not a linker and this is a simple inline sequence
                if verbose then
                    printfn "procAssembly:  taking None arm of long inline hd=%A" hd
                    printfn "procAssembly:  sliceout construction:"
                    printfn "procAssembly:  hd=%s" hd.description
                    printfn "procAssembly:  choppedD=%s" choppedD.description
                    printfn "procAssembly:  sliceOut'=%s" (String.Join(";",[for x in sliceOut' -> x.description]))
                procAssembly
                    verbose
                    dp
                    errorName
                    (choppedD::hd::prev) // prev slices (emit chopped version of next slide choppedD, and this inline hd as well)
                    (choppedD::hd::sliceOut') // updated slices we are emitting - everything is emitted already
                    (GAP::DPP({fwd = primerF; rev = primerR; name = hd.description})::primersOut) // primers out - GAP for the D slice and DPP for the primers on the hd/linker piece
                    e // remaining unprocessed slices
            | Some x ->
                if verbose then
                    printfn "procAssembly:  taking Some x=%s arm of long inline hd=%s" x.description hd.description
                procAssembly
                    verbose
                    dp
                    errorName
                    (choppedD::x::hd::prev) // prev slices. Include the sandwich x and the downstream d which may have been chopped - note LIFO so choppedD right most
                    (choppedD::x::hd::sliceOut') // updated slices we are emitting  - pair the sandwich with a SANDWICHGAP Dpp type in the primer out stream
                    (GAP::SANDWICHGAP::DPP({fwd = primerF; rev = primerR; name = hd.description})::primersOut)
                    e // todo slices

    // technically shouldn't end on a non linker (unless non ryse design) but..
    | [last] when (last.sliceType = LINKER || last.sliceType = INLINEST) ->
        if verbose then
            printfn "procAssembly: PACASE 4 - ... last LINKER or INLINEST last=%A name=%s" last.sliceType last.description
        // We are about to design a primer back into the previous sequence if it exists.
        // There is a catch if the previous sequence was a short inline sequence.
        // We should treat that as a sandwich sequence, build it into the primer but not
        // design amplification against it.
        // hd2 :: hd (sandwich)             :: this slice..
        // <-- (skipped for primer design)..
        let sandwich,prevAmp, skipped =
            match prev with
            | hd::hd2::_ when hd.sliceType = SliceType.INLINEST -> Some(hd),Some(hd2), true // reach back past inline sequence hd
            | [hd] when hd.sliceType = SliceType.INLINEST ->
                // Only one slice to the left (head of prev) and it's inline so not enough DNA to make a primer against
                // Note this could be a bad decision if it's a big inline sequence but those get promoted elsewhere to being real slices not inline
                // some room to allow users to use #amp to shape this decision in the future..
                failwithf "internal amplification error amplifying INLINEST %A" hd.description
            | hd::_ -> None, Some(hd), false // head isn't an inline, no sandwich present
            | _ -> None, None, false // who knows whatever other cases... :)

        // design back into whichever slice we selected to amplify against. prevAmp is the slice that we design against
        let primerR'',offsetR = linkerRev2 verbose dp errorName prevAmp

        // Now mark sandwich dna if anything or empty if nothing (flip for reverse primer)
        let sandwichDNA =
            match sandwich with | None -> Dna("") | Some(dna) -> dna.dna.RevComp()

        /// Proto primer including tail with linker and potential sandwich sequence.  Could still be too long
        let primerR' =
            if primerR''.body.Length = 0 then
                match sandwich with
                | None -> ()
                | Some(s) -> assert (s.dna.Length = 0) // must be no sandwich present
                primerR''
            else
                {primerR'' with
                           tail = last.dna.RevComp();
                           body = DnaOps.append sandwichDNA primerR''.body}
                |> trimLinkerTailBody dp
        //
        // Body, [sandwich],  tail  (linker)
        // <-bbbbbbbbbbb sssssss TTTTTTTTTTTTTTT
        /// Mark the bases that became the sandwich within the primer
        let sandwichAnnotation =
            if sandwichDNA.Length = 0 then []
            else [{il=primerR'.tail.Length;
                   ir = primerR'.tail.Length + sandwichDNA.Length-1;
                   iType = DNAIntervalType.SANDWICH}]

        /// Final primer including annotation of intervals
        let primerR =
            {primerR' with
                      annotation =
                        if primerR'.body.Length = 0 then []
                        else
                            sandwichAnnotation
                           @[{il=0; ir=primerR'.tail.Length-1; iType = DNAIntervalType.RYSELINKER};
                             {il=primerR'.tail.Length+sandwichDNA.Length;
                              ir = primerR'.tail.Length + primerR'.body.Length-1;
                              iType = DNAIntervalType.AMP}]}
        // prevAmp was the slice we designed the rev primer against
        match prevAmp with
        | None -> assert(primerR.Primer.Length=0) // if none, should be no reverse primer
        | Some(ds) -> primerCheck primerR.Primer.arr (ds.dna.RevComp().[offsetR..].arr) // Otherwise check primer actually lands in it otherwise bad things happen

        // potentially adjust previously emitted slice if primer generation moved the boundary (it was approximate ended)
        // sliceOut' is the final slice list (reversed into the correct left to right order as we are finishing up)
        let sliceOut' =
            match prev with
            | [] ->
                // We are on the last element and there are no preceding elements.  This could
                // be a stand-alone, single DNA slice.  Process it as such.  This is going to create
                // trouble if the user intended a RYSE design but they might not be headed there so we have
                // to handle this case.
                // Assume it's a stand-alone inline sequence that could be made with a pair of primers
                printfn "procAssembly:  final sliceOut' prev empty branch"
                if List.isEmpty sliceOut then // Assume it's a stand-alone inline sequence that could be made with a pair of primers
                    [last] // Fake slice for now.. TODO TODO
                else failwith "expected preceding linker adjacent to terminal inline sequence"
            | previousInline :: previousAmp :: tail when skipped -> // this case assumes previousInline is a sandwich because skipped is set
                if verbose then
                    printfn "procAssembly:  potentially trimming p=%s last=%s" previousAmp.description last.description
                // take remaining element (last) and push it onto sliceOut with inline and adjusted amp slice cut if necessary
                last :: previousInline ::(cutRight verbose previousAmp offsetR) :: tail
                |> List.rev // Finally reverse the slice out list since we pushed it as we created it  ( we are done - not recursing any more and this is the result)
            | p::_ ->
                // 1) prepend the final part (probably the terminal linker)
                // 2) cut the previous element (head of prev slices) if primer moved boundary
                // 3) incllude remainder of previous slices
                // 4) reverse list to get natural forward order since we pushed results on successively
                if verbose then
                    printfn "procAssembly:  potentially trimming p=%s last=%s" p.description last.description
                last::(cutRight verbose p offsetR)::(List.tail sliceOut)
                |> List.rev // Finally reverse the slice out list since we pushed it as we created it  ( we are done - not recursing any more and this is the result)

        if verbose then
                printfn "sliceOut'=%s" (String.Join(";",[for s in sliceOut' -> s.description]))

        // This is going back to caller.  Collect the primer arrangment and slices as a tuple (this deserves at least an anonymous record)
        let finalOutput = (DPP({fwd = {body=Dna(""); tail =Dna(""); annotation = []}; rev = primerR; name = last.description})::primersOut |> List.rev,
                             sliceOut') // Last linker
        let finalDPPs,finalSlices = finalOutput
        let outputParity = finalDPPs.Length = finalSlices.Length
        if verbose || (not outputParity) then
            // Debugging output
            // ======================================

            let y = finalDPPs |> Seq.map prettyPrintPrimer |> Array.ofSeq
            let x = finalSlices |> Seq.map nameFromSlice |> Array.ofSeq

            // format things so they line up - which is the longer element in each position
            // being careful because one list is smaller
            // kind of excessive but makes debugging output a lot easier to read
            let formatWidth =
                Array.init
                    (max x.Length y.Length)
                    (fun i ->
                        let len1 = if i < x.Length then x.[i].Length else 0
                        let len2 = if i < y.Length then y.[i].Length else 0
                        printfn "l1=%d l2=%d" len1 len2
                        max len1 len2
                    )
            let pad (s:string) n = if s.Length >=n then s else s+("                                              ".[0..n-s.Length-1])
            let x' = x |> Array.mapi (fun i v -> pad v (formatWidth.[i]))
            let y' = y |> Array.mapi (fun i v -> pad v (formatWidth.[i]))

            let y = String.Join(" ; ",x')
            let x = String.Join(" ; ",y')
            printfn "procAssembly: finalOutput(slices n=%d): %s" (finalSlices.Length) x
            printfn "procAssembly: finalOutput(primer n=%d): %s" (finalDPPs.Length) y
            if not outputParity then
                failwithf "These lists should have same length :( - error in procAssembly"

        finalOutput // RETURN POINT *****
    // /inline/ (not amp or rabitend) ::.....
    // this is strictly an inline slice that is going to get built by primers when we do the next
    // slice
    | hd::tl when
        hd.sliceType = INLINEST &&
        (not (hd.pragmas.ContainsKey("rabitend") || hd.pragmas.ContainsKey("amp"))) ->
        if verbose then
            printfn "procAssembly: PACASE 5 -... (GAP) INLINEST"
        // For now we don't know what is going to happen with this slice.  It will get created
        // as part of the next slice that gets processed (e.g. a slice or a linker).  For now
        // just put the slice onto the prev stack
        let prevNew = hd::prev

        // We also include any slice on head prev in the sliceOut. (incPrev)
        // todo:
        // This seems a bit sketchy - how sure can we be that the previous slice hasn't
        // already been emitted.  I'd be more comfortable in future if every slice had a marker
        // noting if/how it had been emitted and we could do a sanity check at end of the run to
        // make sure everything went out just once

        procAssembly verbose dp errorName prevNew (incPrev prev sliceOut) (GAP::primersOut) tl
    // This cases catches inline sequences just before a linker marked rabitend
    // This slice will be implemented when linker goes out
    | hd::tl when hd.sliceType = INLINEST && hd.pragmas.ContainsKey("rabitend") ->
        if verbose then
            printfn "procAssembly: PACASE 6 -"
            printfn "procAssembly: ... (GAP) INLINEST rabitend case hd=%s" hd.description
            printfn "procAssembly: ... new sliceOut = %s" (String.Join(";",[for x in hd::sliceOut -> x.description]))
        // push the slice onto the prev stack (it's going to get implemented by next slice primer gen)
        let prevNew = hd::prev
        // push hd onto slices out.  It will be of type sandwich on primer side
        procAssembly verbose dp errorName prevNew (hd::sliceOut) (SANDWICHGAP::primersOut) tl
        // procAssembly verbose dp errorName prevNew (incPrev prev sliceOut) (GAP::primersOut) tl
    // Finally catch all case
    // -----------------------------------------------
    | hd::tl ->
        if verbose then
            printfn
                "procAssembly: PACASE 7 -... (GAP) catchall case - should this be fused with previous slice? hd.dna.Length=%d hd.containsAmp=%s"
                hd.dna.Length
                (if hd.pragmas.ContainsKey("amp") then "Y" else "N")
            // emit some diagnostics on prev slice
            match prev with
            | pHd::_ ->
                printfn "procAssembly: ...                      phd.dna.Length=%d phd.containsAmp=%s"
                    pHd.dna.Length
                    (if pHd.pragmas.ContainsKey("amp") then "Y" else "N")
            | [] ->
                printfn "procAssembly: ...                      prev empty"

        // Check if this slice should have been fused with previous slice?
        match prev with
        | pHd::_ when
            // IF prev head long enough or using amp / pcr
            (pHd.dna.Length>100 || pHd.pragmas.ContainsKey("amp")) &&
            // AND this big enough or using amp/pcr THEN
            (hd.dna.Length > 100 || hd.pragmas.ContainsKey("amp")) ->

            // If previous head and this slice are big enough or being forced into an amplification
            // strategy then we are going seamless between them
            printfn "procAssembly: ... generate seamless junction between prev=%s and this=%s" pHd.description hd.description

            // look for any tricky instructions of primer position for previous head
            let primerPos = parsePrimerPos pHd.pragmas

            // determine primer constraints
            let fwdTailLenMax,revTailLenMax =
                match primerPos with
                | FWD(offset) ->
                    let x = -offset // Convert to tail length
                    x, 999999
                | REV(offset) ->
                    let x = offset
                    999999,x
                | NONE -> 999999,999999 // no primerpos
            if verbose then
                    printfn "procAssembly: hasPrimerPos = %s" (match primerPos with | NONE -> "no" | _ -> "yes")
                    printfn "procAssembly: fwdTailLenMax = %d" fwdTailLenMax
                    printfn "procAssembly: revTailLenMax = %d" revTailLenMax

            // step 1, make seamless primers fwd and reverse to join segments without worrying about length constraints (pre chop)
            let primerFPreChop,offsetF,primerRPreChop,offsetR = seamless verbose dp pHd hd

            // step 2, chop tails off maybe if the primer positioning requires it
            let primerFStep2 =
                    {primerFPreChop with
                        tail = primerFPreChop.tail.[..(fwdTailLenMax|> min primerFPreChop.tail.Length)-1]
                    }
            let primerRStep2 =
                    {primerRPreChop with
                        tail = primerRPreChop.tail.[..(revTailLenMax|> min primerRPreChop.tail.Length)-1]
                    }

            // step 3,  backfill primer if warranted to make a longer overlap
            let primerF,primerR =
                match primerPos with
                | FWD x  when x > 0 ->  // extend rev tail
                      //                     o----------->
                      //    ---------------|--------------------
                      //          <------------oxxxxxxx
                      let maxTailLen = dp.pp.maxLength-primerRStep2.body.Length |> min (primerFStep2.body.Length+x-1)
                      let primerRNewTail = hd.dna.[..maxTailLen-1].RevComp()
                      {primerFStep2 with body = primerFStep2.body.[x-1..]},{primerRStep2 with tail = primerRNewTail}
                | REV x  when x < 0 ->  // extend fwd fail
                      //              xxxxxo----------->
                      //    ---------------|--------------------
                      //          <------o
                      let maxTailLen = dp.pp.maxLength-primerFStep2.body.Length |> min (primerRStep2.body.Length+x-1)
                      let primerFNewTail = pHd.dna.[pHd.dna.Length-maxTailLen..]
                      {primerFStep2 with tail = primerFNewTail},{primerRStep2 with body = primerRStep2.body.[(-x)-1..]}
                | _ -> primerFStep2,primerRStep2


            // If we stitched fwd/rev off of linker hd then the previous and next elements (prev) and next
            // might need to be modified (chopped) if the ends were flexible
            let sliceOut' = match prev with | [] -> sliceOut | p::_ -> (cutRight verbose p offsetR)::(List.tail sliceOut)

            // todo: feelings like this should defined elsewhere.  It's just an empty fusion slice
            let fusionSlice = {
                id = None
                extId = None
                dna = Dna("")
                sourceChr=""
                sourceFr = 0<ZeroOffset>
                sourceTo = 0<ZeroOffset>
                sourceFwd = true
                sourceFrApprox = false
                sourceToApprox = false
                destFr = 0<ZeroOffset>;
                destTo = 0<ZeroOffset>;
                destFwd = true
                /// is this slice created by PCR
                amplified = false
                template = None
                sliceName = "fusion"
                uri = None
                description = "fusion"
                sliceType = FUSIONST
                pragmas = EmptyPragmas
                dnaSource = ""
                breed = B_VIRTUAL
                /// Keep track of the part this slice was materialized from.
                materializedFrom = None
                annotations = []
            }

            procAssembly
                verbose
                dp
                errorName
                (hd::prev) // head goes onto previous
                (hd::fusionSlice::sliceOut') // push out the head with fusion slice to note what we did (there has to be a slice corresponding to the primer pair in that position)
                (GAP::DPP({fwd = primerF ; rev = primerR ; name = hd.sliceName})::primersOut)
                tl // Remove bases from the next slice if we moved the primer
                // FIXFIX - I am pretty sure this will result in a bug if the next slice has an approximate end.  We should minimally assert that this is not true...
                //((cutLeft hd offsetF)::tl) // Remove bases from the next slice if we moved the primer
        // This is another monster match inside a match case.  Badly needs some gentle dissection into more managable pieces.  I'd probably start with test coverage though before we break too much
        | _ -> // Catch all case for prev when prev was just one item.  todo: should be [hd] with an error case for anything else?
            if verbose then
                printfn "procAssembly: ... regular branch of catch all"
            // hd just moves to prev and slices out (any primers will come from processing adjacent slices)
            procAssembly verbose dp errorName (hd::prev) (hd::sliceOut)  (GAP::primersOut) tl

// --- end procAssembly ------------------------------------------------------------------------

/// Time to design some primers given a list of assemblyout structures
let designPrimers (opts:ParsedOptions) (linkedTree : DnaAssembly list) =
    let verbose = opts.verbose

    let primers',newSlices' =
        linkedTree
        |> Array.ofList
        |> (if opts.doParallel then Array.Parallel.map
            else Array.map)
                (fun a ->
                    let errorName = a.name
                    let primerMaxLen =
                        match a.pragmas.TryGetOne("primermax") with
                        | None -> primerMaxDefault
                        | Some(v) -> int v
                    let primerMinLen =
                        match a.pragmas.TryGetOne("primermin") with
                        | None -> primerMinDefault
                        | Some(v) -> int v

                    procAssembly
                        verbose
                        {a.designParams with
                            pp = {a.designParams.pp with maxLength = primerMaxLen; minLength=primerMinLen}}
                        errorName
                        []
                        []
                        []
                        a.dnaParts)
        |> Array.unzip

    /// Slightly ugly hack.  If we are doing linkerless designs, there is no anneal region between
    /// the divergent primers, so the primer annotation for the anneal region is an empty interval.
    /// To avoid download validation and display mess, we filter those primer annotation intervals out
    let cleanAnnealIntervals (p:DivergedPrimerPair list) =
        let filterAnneal (p:Primer) =
            {p with annotation =
                    p.annotation |> List.filter (fun x ->x.iType <> ANNEAL && x.iType <> SANDWICH)}
        p |> List.map (fun x ->
            match x with
            | GAP -> x
            | SANDWICHGAP -> x
            | DPP(y) ->
                if y.fwd.tail.Length = 0 || y.rev.tail.Length = 0 then
                    DPP({y with fwd = filterAnneal y.fwd; rev = filterAnneal y.rev})
                else
                    DPP(y)) // leave it alone

    let primers = List.ofArray primers' |> List.map (cleanAnnealIntervals)
    let newSlices = List.ofArray newSlices'
    let newTree =
        List.zip linkedTree newSlices
        |> List.map (fun (a,b) -> { a with dnaParts = recalcOffset b})
    //let oldTree = linkedTree |> List.map (fun a -> a.dnaParts)

    // Validate primer annotation and primers are legit
    primerValidation.checkPrimers primers
    primerValidation.checkPrimersVAssembly (List.zip primers newTree)
    primers, newTree
