module PrimerCreation
/// Support routines for primer design scenarios and primer generation for stitches

open shared // common routines used in several modules
open commonTypes
open System
open System.Text
open constants // primer parameters
open Amyris.Bio.primercore
open Amyris.Bio
open Amyris.Bio.utils
open Amyris.Bio.biolib
open DesignParams

/// Check if tail of A overlaps head of B
let checkTailAOverlapsHeadB (a:char array) (b:char array) =
    let rec maxOverlap i =
        if i >= a.Length-12 then
            false
        else
            if a.[i..] = (b.[..a.Length-i-1]) then
                true
            else maxOverlap (i+1)

    maxOverlap (max (a.Length-b.Length) 0)

let checkBContainedInA (a:char array) (b:char array) =
    let rec checkAt i =
        if i > a.Length-b.Length then
            false
        elif a.[i..i+b.Length-1] = b then
            true
        else
            checkAt (i+1)
    checkAt 0

let checkParallelOverlap (a:char array) (b:char array) =
    if (checkTailAOverlapsHeadB a b) ||
       (checkTailAOverlapsHeadB b a) ||
       (checkBContainedInA a b)  ||
       (checkBContainedInA b a)
    then
        () // test passes
    else
        failwithf "insufficient overlap in checkParallelOverlap \na=%s \nb=%s"
            (a |> arr2seq) (b |> arr2seq)

let checkAntiParallelOverlap (a:char array) (b:char array) =
    let rec maxOverlap i =
        if i < 11 then
            failwithf "insufficient overlap in checkAntiparallelOvelap a=%s b=%s bRC=%s"
                (a |> arr2seq) (b |> arr2seq) (b |> revComp |> arr2seq)

        if a.[..i] = (b.[..i] |> revComp) then () else maxOverlap (i-1)

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
        (middleDNA : char array) =
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
        | Some(n) -> revComp middleDNA.[..n-1]
        | None -> Array.concat [middleDNA; fwd.body] |> revComp

    /// the body part of the reverse oligo, which corresponds to the upstream rabit, + middleDNA (linker + sandwich seqs).
    /// Upstream rabit is included to deal with seamless designs without a linker
    let fwdTemplate =
        match fwdTailLenFixed with
        | Some(n) ->  middleDNA.[middleDNA.Length-n..]
        | None -> Array.concat [rev.body |> revComp; middleDNA]

    /// The reverse primer body (end of upstream rabit) + middleDNA (linkers + sandwich sequence) + the forward
    /// primer body (beginning of downstream rabit)
    let fullTemplate = Array.concat [ rev.body |> revComp ; middleDNA ; fwd.body ]

    /// Target Tm for middle annealing part.  Cheat if it's a linker and we just want to keep this part (ideally) full length
    let annealTarget = match firmMiddle with | Some(x) -> x | None -> dp.seamlessOverlapTm
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
                    else dp.targetTm - (Amyris.Bio.primercore.temp dp.pp fwd.body s.fb) }

        let updateRev (s:TuneState) =
            {s with bestRevDelta =
                    if s.rb < 5 then 999.0<C>
                    else dp.targetTm - (Amyris.Bio.primercore.temp dp.pp rev.body s.rb) }

        let updateAnneal (s:TuneState) =
            {s with bestAnnealDelta =
                    annealTarget
                  - (Amyris.Bio.primercore.temp
                        dp.pp (fullTemplate.[X-s.ft+1..]) ((Y+s.rt-1)-(X-s.ft+1)+1))}

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
                    if fwdTailLenFixed.IsNone && state.ft > fwdTailLenMin then yield CHOP_F_ANNEAL
                | OligoMax(_) -> // could slide or cut
                    if state.bestFwdDelta < 0.0<C> && state.fb>dp.pp.minLength then yield CHOP_F_AMP
                    if fwdTailLenFixed.IsNone then
                        if  state.bestAnnealDelta < 0.0<C> && state.ft > fwdTailLenMin then yield CHOP_F_ANNEAL
                        if state.fb < fwd.body.Length && state.ft > fwdTailLenMin then yield SLIDE_F_RIGHT
                        if state.rt > revTailLenMin && state.ft < fwdTailLenMax then
                            yield SLIDE_F_LEFT
                | _ ->
                    // All these moves are only possible if the tail isn't a fixed length
                    if fwdTailLenFixed.IsNone then
                        if state.bestFwdDelta < 0.0<C> && state.ft < fwdTailLenMin && state.fb>dp.pp.minLength
                            then yield CHOP_F_AMP
                        elif state.fb < fwd.body.Length then yield EXT_F_AMP
                        if state.bestAnnealDelta < 0.0<C> && state.ft > fwdTailLenMin
                            then yield CHOP_F_ANNEAL
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
                        | _ as x -> failwithf "unexpected delta sign combo %A" x

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
                (fullTemplate.[X-fwd.tail.Length+1..])
                ((Y+rev.tail.Length-1)-(X-fwd.tail.Length+1)+1)

    if verbose then
        printfn "tuneTailOpt: starting fwdTail=%s" (fwd.tail |> arr2seq)
        printfn "tuneTailOpt: starting fwdBody=%s" (fwd.body |> arr2seq)
        printfn "tuneTailOpt: starting revTail=%s" (rev.tail |> arr2seq)
        printfn "tuneTailOpt: starting revBody=%s" (rev.body |> arr2seq)
        printfn "tuneTailOpt: starting fwdTailLenFixed=%s"
            (match fwdTailLenFixed with | None -> "no" | Some(x) -> sprintf "yes %d" x)
        printfn "tuneTailOpt: starting revTailLenFixed=%s"
            (match revTailLenFixed with | None -> "no" | Some(x) -> sprintf "yes %d" x)
        printfn "tuneTailOpt: starting fwdTailLenMin=%d" fwdTailLenMin
        printfn "tuneTailOpt: starting revTailLenMin=%d" revTailLenMin
        printfn "tuneTailOpt: starting startAnnealTm=%f" (startAnnealTm/1.0<C>)
        printfn "tuneTailOpt: starting annealTarget=%f" (annealTarget/1.0<C>)
        printfn "tuneTailOpt: starting middleDNA=%s" (middleDNA |> arr2seq)
        printfn "tuneTailOpt: starting fwdTemplate=%s" (fwdTemplate |> arr2seq)
        printfn "tuneTailOpt: starting revTemplate=%s" (revTemplate |> arr2seq)
        printfn "tuneTailOpt: starting fullTemplate=%s" (fullTemplate |> arr2seq)

    let rec trimIfNeeded (p:Primer) =
        if p.lenLE(dp.pp.maxLength) then p else
        let ampTemp = Amyris.Bio.primercore.temp dp.pp p.body p.body.Length
        let ampDelta = abs (ampTemp - dp.targetTm)
        let annealTemp = Amyris.Bio.primercore.temp dp.pp p.tail p.tail.Length
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
        let fwdAmpTm = Amyris.Bio.primercore.temp dp.pp fwd.body fwd.body.Length
        let revAmpTm = Amyris.Bio.primercore.temp dp.pp rev.body rev.body.Length

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
            printfn "tuneTailOpt: ending fwd %s|%s"
                (fwdTemplate.[fwdTemplate.Length-f..] |> arr2seq) (fwd.body.[..finalParams.fb-1] |> arr2seq)
            printfn "tuneTailOpt: ending middle %s" (middleDNA |> arr2seq)
            printfn "tuneTailOpt: ending template %s" (fullTemplate |> arr2seq)
            printfn "tuneTailOpt: ending rev %s|%s"
                (rev.body.[..finalParams.rb-1] |> revComp |> arr2seq)
                (revTemplate.[revTemplate.Length-r..] |> revComp |> arr2seq)

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

/// Time to design some primers given a list of assemblyout structures
let designPrimers (opts:ParsedOptions) (linkedTree : DnaAssembly list) =
    let verbose = opts.verbose

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
//    let primerCheckR (p:char array) (prev : DNASlice option) =
//         match prev with| None -> assert(p.Length = 0)
//                        | Some(d) ->
//                            prefix p (revComp d.dna)

    /// design seamless primers across a junction that may have flexible endpoints on either side
    /// return primer, offset, len reverse and primer, offset len, fwd
    let seamless (dp:DesignParams) (prev:DNASlice) (next:DNASlice) =
        // We need a forward and reverse primer designed into the adjacent sequences
        // Allocate half max len to each primer
        let maxFwd = dp.pp.maxLength / 2
        let maxRev = dp.pp.maxLength - maxFwd

        /// Wrapper function for oligo design to capture and report errors in primer design
        let pdWrap (debug:bool) (pen:PrimerParams) (task:OligoTask) =
            match primercore.oligoDesign debug pen task with
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
                       tmPenalty = if dp.seamlessTm> 60.0<C> then 3.0 else 1.0}

            // Handle approximate ends, and make sure we are looking at the right end being flexible..
            // bugfix 20111111 - wasn't using correct approximate end flag
            if (fwd && s.sourceFrApprox) || (not fwd && s.sourceToApprox) then
                let task: OligoTask =
                   {tag = if fwd then "PF" else "PR";
                    temp =
                        if fwd then s.dna.[0..min (s.dna.Length-1) (2*approxMargin)]
                        else (revComp s.dna).[0..min s.dna.Length (2*approxMargin)];
                    align = ANCHOR.CENTERLEFT;
                    strand = STRAND.TOP;
                    offset = 0;
                    targetTemp = dp.seamlessTm;
                    sequencePenalties = None}

                // primercore.oligoDesign false pen task
                pdWrap false pen task

            else
                let task: OligoTask =
                    {tag = if fwd then "PF" else "PR";
                     temp = if fwd then s.dna else revComp s.dna;
                     align = ANCHOR.LEFT;
                     strand = STRAND.TOP;
                     offset = 0;
                     targetTemp = dp.seamlessTm;
                     sequencePenalties = None}
                //primercore.oligoDesign false pen task
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
            let fwd = {tail = revComp (r.oligo.[..bestR-1]); body = f.oligo; annotation = []}
            let rev = {tail = revComp (f.oligo.[..bestF-1]); body = r.oligo; annotation = []}

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
    let linkerFwd2 (dp:DesignParams) errorName (next:DNASlice) =
        let rec linkerFwd2Iterative pen margin =
            let x =
                if next.dna.Length < 10 then
                    if verbose then
                        printf "WARNING: template dna (%s) (len=%d) %s too short for succcessful primer design\n"
                            next.description next.dna.Length (arr2seq next.dna)
                    None
                else
                    if next.sourceFrApprox then
                        let task =
                           {tag ="PF";
                            temp = next.dna.[0..min (next.dna.Length-1) margin];
                            align = ANCHOR.CENTERLEFT;
                            strand = STRAND.TOP;
                            offset =0;
                            targetTemp = dp.targetTm;
                            sequencePenalties = None}
                        primercore.oligoDesign false pen task
                    else
                        let task =
                           {tag = "PF";
                            temp = next.dna;
                            align = ANCHOR.LEFT;
                            strand = STRAND.TOP;
                            offset = 0;
                            targetTemp = dp.targetTm;
                            sequencePenalties = None}
                        if pen.maxLength < pen.minLength then
                            failwithf "oMax=%d<oMin=%d  in linkerFwd2" pen.maxLength pen.minLength
                        primercore.oligoDesign false pen task
            match x with
            | None ->
                if margin < 3*approxMargin then linkerFwd2Iterative pen (margin+10)
                else failwithf "failed primer design for design %s" errorName
            | Some(oligo) ->
                // Oligo design might have chopped off DNA , so cut accordingly
                primerCheck oligo.oligo (next.dna.[oligo.offset..])
                // Annotation regions for the oligo
                {tail = [||]; body = oligo.oligo; annotation = []}, oligo.offset

        linkerFwd2Iterative dp.pp (2*approxMargin)

    /// Modified version that doesn't take sandwich length into account while designing the amp primer
    /// Design reverse linker into upstream slice.  Returns primer and amount to chop off upstream
    /// slice if it was an approx slice
    let linkerRev2 (dp:DesignParams) errorName (last:DNASlice option) =
        // Recursively try different compromises on how much we eat into the
        // linker length and extend the approximate region for the primer.  If
        // we fail, try eating more and roaming more widely

        let rec linkerRev2Iterative pen margin =
            match last with
            | None -> { body = [||]; tail=[||] ; annotation=[]},0
            | Some(ds) ->
                let x =
                    if ds.sourceToApprox then
                        let task =
                           {tag = "PR";
                            temp = (revComp ds.dna).[0..min (ds.dna.Length-1) margin];
                            align = ANCHOR.CENTERLEFT;
                            strand = STRAND.TOP;
                            offset = 0;
                            targetTemp = dp.targetTm;
                            sequencePenalties  = None}
                        if opts.verbose then
                            printf "linkerRev: oligo design\n template=%s\npen=%A\n"
                                (arr2seq task.temp) pen
                        primercore.oligoDesign false pen task
                    else
                        let task =
                           {tag = "PR";
                            temp = revComp ds.dna;
                            align = ANCHOR.LEFT;
                            strand = STRAND.TOP;
                            offset = 0;
                            targetTemp = dp.targetTm;
                            sequencePenalties  = None}
                        primercore.oligoDesign false pen task
                match x with
                | None ->
                    if margin < 3*approxMargin then linkerRev2Iterative pen (margin+10)
                    else failwithf "failed primer design for design %s" errorName
                | Some(oligo) ->
                    primerCheck oligo.oligo ((revComp ds.dna).[oligo.offset..])
                    {tail = [||]; body = oligo.oligo; annotation = []}, oligo.offset

        linkerRev2Iterative dp.pp (2*approxMargin)

    /// Recursively process an assembly, tracking the previous and remaining DNA slices
    /// emitting an output set of DNA slices and a diverged primer pair list
    let rec procAssembly
            (dp:DesignParams)
            errorName
            (prev : DNASlice list)
            sliceOut
            (primersOut:DivergedPrimerPair list)
            (l:DNASlice list) =

        if verbose then
            let nameFromSlice (s:DNASlice) = s.description
            printfn "procAssembly: top,  prev=[%s] l=[%s]"
                (String.Join(";",(prev |> Seq.map(nameFromSlice))))
                (String.Join(";",(l |> Seq.map(nameFromSlice))))

        /// Include prev in slices out if it wasn't None
        let incPrev (prev: DNASlice list) (slices : DNASlice list) =
            match prev with | [] -> slices | p::_ -> p::slices

        /// Chop bases off a primer whose tail is a linker until it meets the length requirement
        let trimLinkerTailBody (p:Primer) =
            let tailTargetTM = if p.tail.Length=0 then 0.0<C> else temp dp.pp p.tail p.tail.Length
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
                        let bTm = temp dp.pp p.body bLen'
                        let tTm = temp dp.pp (p.tail.[p.tail.Length-tLen'..]) tLen'
                        if abs(tTm-tailTargetTM) < abs(bTm-bodyTargetTm) then
                            // Pick on tail, it's not doing so bad
                            find bLen (tLen-1)
                        else
                            // Pick on body, it's not doing so bad
                            find (bLen-1) tLen
            find p.body.Length p.tail.Length

        // Two functions for cutting left or right into a slice, used when those slice ends were approximate
        // and primer design ended up chopping into them.
        let cutRight (slice:DNASlice) n =
            if slice.dna.Length-1-n<0 then
                failwithf "in cutRight dna slice %s length is %d, cut is %d"
                    slice.description  slice.dna.Length n
            {slice with
                   sourceTo = slice.sourceTo - (n*1<ZeroOffset>);
                   dna = slice.dna.[0..slice.dna.Length-1-n]}

        let cutLeft (slice:DNASlice) n =
            if slice.dna.Length-1-n<0 then
                failwithf "in cutLeft dna slice %s length is %d, cut is %d"
                    slice.description  slice.dna.Length n
            {slice with
                   sourceFr = slice.sourceFr + (n*1<ZeroOffset>);
                   dna = slice.dna.[n..slice.dna.Length-1]}

        match l with
        | [] ->
            let sliceOut' = incPrev prev sliceOut |> List.rev
            List.rev primersOut,sliceOut'
        | hd::next::tl when hd.sliceType = FUSIONST && next.sliceType=INLINEST ->
            // Fusing a slice to a following inline sequence is redundant as that's the
            // strategy we employ by default, but it will mess things up to try to put a seamless stitch here into a possibly
            // small DNA sequence, so just ignore FUSION directive
            procAssembly dp errorName prev sliceOut primersOut (next::tl)
        | hd::next::tl when hd.sliceType = FUSIONST ->
            // Slice hd is a fusion slice, which is virtual, it exists only to mark
            // the intention to fuse prev and next together
            if prev = [] then failwith "INTERNAL ERROR: unexpected prev = [] in procAssembly\n"
            let primerF,offsetF,primerR,offsetR = seamless dp (List.head prev) next

            // If we stitched fwd/rev off of linker hd then the previous and next elements (prev) and next
            // might need to be modified (chopped) if the ends were flexible
            if opts.verbose then printfn "fusion slice, cutting offsetF=%d offsetR=%d" offsetF offsetR
            let sliceOut' = match prev with | [] -> sliceOut | p::_ -> (cutRight p offsetR)::sliceOut

            procAssembly
                dp
                errorName
                (hd::prev)
                sliceOut'
                (DPP({fwd = primerF ; rev = primerR ; name = errorName})::primersOut)
                ((cutLeft next offsetF)::tl) // Remove bases from the next slice if we moved the primer
        // linker::next   or non-rabitend-inline::next
        | hd::next::tl when
            (hd.sliceType = LINKER) ||
            (hd.sliceType =
                INLINEST && // Actual mid rabit inline slice not one at the end of a rabit
                not (hd.pragmas.ContainsKey("rabitend") || hd.pragmas.ContainsKey("rabitstart"))) &&
                prev <> []
            ->
            if hd.sliceType = INLINEST && hd.dna.Length < 12 then
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
                let primerF,offsetF,primerR,offsetR = seamless dp (List.head prev) next

                //
                // They can instruct us to keep the forward or reverse primers past a particular point.  If
                // so, we might need to truncat the primers a little
                //
                //
                //  #primerpos FWD 5   implies fwd primer should start from the 5th base relative to the start of the sandwich segment
                //                     >>>>>>>>>>X>>>>>>>>>>>
                //  prev ppppppppppppppppppp iiiiiii nnnnnnnnnnnnnnn next
                //
                // Parse any primer position directive
                let fwdTailLenMax,revTailLenMax =
                    match hd.pragmas.TryGetValues("primerpos") with
                    | Some(v) ->
                        match v |> List.map (fun (s:string) -> s.ToUpper()) with
                        // TODO: these should be parsed and converted into union cases much earlier
                        // in compiler execution
                        | [ "FWD"; offsetStr ] ->
                                let x = (hd.dna.Length+1)-int(offsetStr) |> max 0// Convert to tail length
                                x, 999999
                        | [ "REV"; offsetStr ] ->
                                let x = int(offsetStr) |> max 0
                                999999,x
                        | _ -> failwithf "ERROR: invalid primerpos pragma '%A', should be fwd ### or rev ### " v
                    | None -> 999999,999999
                if verbose then
                    printf "primerpos directive: fwdTailLenMax = %A revTailLenMax = %A\n" fwdTailLenMax revTailLenMax

                // Tail consists of internal sequence and reverse oligo concatenated.
                let fwdRunway = Array.concat [(biolib.revComp hd.dna); primerR.body] |> biolib.revComp

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
                let revRunway = Array.concat [hd.dna; primerF.body] |> biolib.revComp
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

                if opts.verbose then printfn "short inline case, cutting offsetR=%d" offsetR

                let sliceOut' = match prev with | [] -> sliceOut | p::_ -> (cutRight p offsetR)::sliceOut

                assert(primerF'.lenLE(dp.pp.maxLength))
                assert(primerR'.lenLE(dp.pp.maxLength))

                procAssembly
                    dp
                    errorName
                    (hd::prev)
                    sliceOut'
                    (DPP({fwd = primerF'; rev = primerR'; name = errorName})::primersOut)
                    ((cutLeft next offsetF)::tl) // Remove bases from the next slice if we moved the primer
            else
                // LONGINLINE Case
                // Regular inline case or linker case - just design fwd and reverse then prepend
                // leading sequence (inline or linker)
                // Wrinkle - the fwd primer might go through an inline sequence, so we need to
                // handle that case and really design against the inline sequence two positions downstream.
                // c,d and e are ....
                // c   is the sandwich slice if any
                // d   is the actual "next" slice we design the primer into
                // e   are the remaining slices for further processing
                let sandwichF, d, e =
                    if next.sliceType = INLINEST then
                        match tl with
                        | [] -> failwithf "expected next dnaslice preparing for linkerFwd design\n"
                        | tlhd::tltl -> Some(next),tlhd,tltl
                    else None,next,tl

                // FWD primer creation
                //     -------------------------->
                //     linkerorslice iiiiiii ffffff
                //let primerF',offsetF = linkerFwd dp errorName hd c d   // linker sandwich next

                // Design primer first into forward DNA segment, ignoring any sandwich slice for now
                let primerF', offsetF = linkerFwd2 dp errorName d

                // Complication - when we design a reverse primer, the immediately preceding slice
                // might just be a small inline (sandwich) slice, and we really want to reach back
                // even further to find an actual regular slice we can design the primer into.
                // Inspect prev and pull out (a) a possible sandwich slice and (b) the regular slice
                // to design primer against
                // <<<<<<<<------------------0
                //   b  :: inline (a) :: LINKER ..
                //
                let sandwichR, b, skipped =
                    match prev with
                    | hd1::hd2::_ when hd1.sliceType = INLINEST ->
                        // skipped is set if we had to reach over a slice
                        Some(hd1),Some(hd2),true
                    | hd::_ -> None,Some(hd),false
                    | [] -> None,None,false

                // REV primer creation.  If we need to go back through b then skipped is set to true
                // when b is a small inline slice
                // <-----------------------
                //          linkerorinline
                //   sandwichR :: b ::    hd
                let primerR', offsetR = linkerRev2 dp errorName b // Design into next non sandwich segment first

                // Our scheme at the moment has produced fwd/rev primers that overlap exactly the
                // length of the hd slice which is either a linker or an inline slice.
                // This can be too long/short in case of inline sequences.  Get the up/downstream
                // sequences and fine tune the primers in the case of inline sequences to target the seamlesstm

                let revTail =
                    match sandwichR with
                    | None -> hd.dna |> revComp
                    | Some(s) -> Array.concat [s.dna; hd.dna]  |> revComp

                // Build data for tuneTails.  For the purpose of assignment, body is the part that
                // amplifies the neighbouring regions.
                // Any sandwich and linker DNA goes into the tail

                // TUNING STEP

                let fwdArg = {primerF' with tail = (if primerF'.body.Length>0 then hd.dna else [||])}
                let revArg = {primerR' with tail = (if primerR'.body.Length>0 then revTail else [||])}

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
                       ({revArg with tail = Array.concat [r.dna ; hd.dna] |> revComp},
                        Array.concat [r.dna; hd.dna],
                        fwdArg,
                        r.dna.Length,
                        0)
                    | None,Some(f) ->
                       (revArg,Array.concat [hd.dna; f.dna],
                        {fwdArg with tail = Array.concat [hd.dna;f.dna]},
                        0,
                        f.dna.Length)
                    | Some(r),Some(f) ->
                       ({revArg with tail = Array.concat [r.dna; hd.dna] |> revComp},
                        Array.concat [r.dna; hd.dna; f.dna],
                        {fwdArg with tail = Array.concat [hd.dna; d.dna]},
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
                let linkerChew = true
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
                        let linkerTm = temp dp.pp hd.dna hd.dna.Length
                        ///beginning of linker.
                        let fwdTailLenMax = (Array.length midArg) - lenSR
                        ///end of linker.
                        let revTailLenMax = (Array.length midArg) - lenSF
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
                        let linkerTm = temp dp.pp hd.dna hd.dna.Length
                        (None,
                         ((0.8 * float fwdMiddleLen)|>round),
                         Microsoft.FSharp.Core.int.MaxValue,
                         Some(linkerTm),
                         None,
                         ((0.8 * float revMiddleLen)|>int),
                         Microsoft.FSharp.Core.int.MaxValue)

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
                // tail for optimization purposes, we now put them into the body since that's the convention followed in thumper.
                // Transfer some bases from the tails to the body if necessary

                let primerF =
                    if lenSF=0 then primerF
                    else {primerF with
                                  body = Array.concat [primerF.tail.[primerF.tail.Length-lenSF..]; primerF.body];
                                  tail = primerF.tail.[..primerF.tail.Length-lenSF-1]}
                let primerR =
                    if lenSR=0 then primerR
                    else {primerR with
                                  body = Array.concat [primerR.tail.[primerR.tail.Length-lenSR..]; primerR.body];
                                  tail = primerR.tail.[..primerR.tail.Length-lenSR-1]}
                // HACK FIXFIX - turning off for linkerless adventure 20140820
                //assert ( primerF.tail.Length >= fwdTailLenMin)
                //assert (primerR.tail.Length = 0 || primerR.tail.Length >= revTailLenMin)

                if not (primerF.lenLE(dp.pp.maxLength)) then
                    failwithf "for %s primer design violates length constraint in procAssembly primerF %d not <= %d for %s"
                        errorName primerF.Primer.Length dp.pp.maxLength (arr2seq primerF.Primer)
                if not (primerR.lenLE(dp.pp.maxLength)) then
                    failwithf "for %s primer design violates length constraint in procAssembly primerR %d not <= %d for %s"
                        errorName primerR.Primer.Length dp.pp.maxLength (arr2seq primerR.Primer)
                assert(primerR.lenLE(dp.pp.maxLength))

                // Ensure primers overlap

                // If we stitched fwd/rev off of linker hd then the previous and next elements (prev) and next
                // might need to be modified (chopped) if the ends were flexible.  Calculate the new lengths/positions of the
                // ends of the adjacent slices.

                if opts.verbose then
                    printfn "regular inline case hd=%s, cutting offsetR=%d" hd.description offsetR
                let sliceOut' =
                    match prev with
                    | [] -> sliceOut
                    | p1::p2::_ when skipped ->
                        if opts.verbose then
                            printfn "cutRight p1=%s p2=%s offsetR=%d sliceOut=%A"
                                p1.description p2.description offsetR sliceOut
                        p1::(cutRight p2 offsetR)::(List.tail sliceOut) // messy, undo previously pushed out slice in penultimate pos
                    | p::_ ->
                        if opts.verbose then printfn "cutRight p=%s offsetR=%d" p.description offsetR
                        (cutRight p offsetR)::sliceOut

                // --------------         new prev
                procAssembly
                    dp
                    errorName
                    (hd::prev)
                    sliceOut'
                    (DPP({fwd = primerF; rev = primerR; name = errorName})::primersOut)
                    // Remove bases from the next slice if we moved the primer
                    ((match sandwichF with | None -> [] | Some(t) -> [t])@[(cutLeft d offsetF)]@e)
        // technically shouldn't end on a non linker (unless non ryse design) but..
        | [last] when (last.sliceType = LINKER || last.sliceType = INLINEST) ->
            // We are about to design a primer back into the previous sequence if it exists.
            // There is a catch if the previous sequence was a short inline sequence.
            // We should treat that as a sandwich sequence, built it into the primer but not
            // design amplification against it.
            let sandwich,prevAmp =
                match prev with
                | hd::hd2::_ when hd.sliceType = SliceType.INLINEST -> Some(hd),Some(hd2)
                | [hd] when hd.sliceType = SliceType.INLINEST ->
                    failwithf "internal amplification error amplifying INLINEST %A" hd.description
                | hd::_ -> None,Some(hd)
                | _ -> None,None

            let primerR'',offsetR = linkerRev2 dp errorName prevAmp
            let sandwichDNA = match sandwich with | None -> [||] | Some(dna) -> (dna.dna |> revComp)

            /// Proto primer including tail with linker and potential sandwich sequence.  Could still be too long
            let primerR' =
                if primerR''.body.Length = 0 then
                    match sandwich with
                    | None -> ()
                    | Some(s) -> assert (s.dna.Length = 0) // must be no sandwich present
                    primerR''
                else
                    {primerR'' with
                               tail = last.dna |> revComp;
                               body = Array.concat [sandwichDNA; primerR''.body ]}
                    |> trimLinkerTailBody
            //
            // Body, [sandwich],  tail  (linker)
            // <-bbbbbbbbbbb sssssss TTTTTTTTTTTTTTT
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
            match prevAmp with
            | None -> assert(primerR.Primer.Length=0)
            | Some(ds) -> primerCheck primerR.Primer ((revComp ds.dna).[offsetR..])

            let sliceOut' =
                match prev with
                | [] ->
                    // We are on the last element and there are no precending elements.  This could
                    // be a stand-alone, single DNA slice.  Process it as such.  This is going to create
                    // trouble if the user intended a RYSE design but they might not be headed there so we have
                    // to handle this case.
                    // Assume it's a stand-alone inline sequence that could be made with a pair of primers
                    if sliceOut = [] then // Assume it's a stand-alone inline sequence that could be made with a pair of primers
                        (* type DNASlice = { id : int option ; extId : string option ; dna : char array ;  sourceChr : string ; sourceFr : int<ZeroOffset> ; sourceTo : int<ZeroOffset>
                            ; sourceFwd : bool ;
                            sourceFrApprox: bool; sourceToApprox : bool;
                             destFr : int<ZeroOffset> ; destTo : int<ZeroOffset> ; destFwd : bool
                            ; sliceName : string ; description : string ; sliceType : SliceType ; pragmas:Map<string,string>;
                            dnaSource : string}      *)
                            (*
                            let slice = { id = None ; extId = None ; dna = last.dna ; sourceChr = "inline" ;
                                            sourceFr = 0<ZeroOffset> ; (sourceTo = last.dna.Length-1)*1<ZeroOffset> ;
                                            sourceFwd = true ; sourceFrApprox = false; sourceToApprox = false ;
                                            destFr = 0<ZeroOffset> ; destTo = 0<ZeroOffset> (*fixed later *) ;
                                            description = "inline dna" ; sliceType = INLINEST ; pragmas = last.pragmas

                            *)
                        [last] // Fake slice for now.. TODO TODO
                    else failwith "expected preceding linker adjacent to terminal inline sequence"
                | p::_ -> last::(cutRight p offsetR)::sliceOut  |> List.rev

            (DPP({fwd = {body=[||]; tail =[||]; annotation = []}; rev = primerR; name = errorName})::primersOut |> List.rev,
             sliceOut') // Last linker
        | hd::tl when hd.sliceType = INLINEST ->
            let prevNew = hd::prev
            procAssembly dp errorName prevNew (incPrev prev sliceOut) (GAP::primersOut) tl
        | hd::tl ->
            procAssembly dp errorName (hd::prev) (incPrev prev sliceOut)  (GAP::primersOut) tl

    // --- end procAssembly ------------------------------------------------------------------------



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
