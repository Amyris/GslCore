module alleleSwaps

///
/// Support for introducing mutations into genes
///
open Amyris.Bio
open Amyris.ErrorHandling
open Amyris.Dna
open System
open constants
open DesignParams
open AstTypes
open pragmaTypes
open LegacyParseTypes
open commonTypes
open RefGenome
open IO.CodonUsage 
open utils
open biolib
open primercore
open FetchPart

open PluginTypes

(*

if < 1000 to 5' and > 2000 to 3' then   5' design
otherwise 3'

Warn if > 1000 and > 2000 then warn

if IG region < 600 then just duplicate

If upstream gene also same direction, then fine
if divergent and mutation within 500bp of other gene, then repeat mutation

consider double HB

*)

/// Build a reverse lookup table of possible codons encoding a particular amino
/// acid, avoiding a provided list of non starters
let makeRevLookup (avoid:Set<Dna>) =
    DnaConstants.codons
    |> Array.fold (fun (aaMap:Map<char,Set<Dna> >) codon ->
        if avoid.Contains(codon) then aaMap
        else
            let aa = DnaOps.codon2aa codon
            if aaMap.ContainsKey(aa) then
                aaMap.Remove(aa).Add(aa,aaMap.[aa].Add(codon) )
            else
                aaMap.Add(aa,Set.ofList [codon])
        ) Map.empty

// Rare yeast codons we'd like to avoid at the 5' end
// Yeast specific, we'll need to change this potentially
let rareCodons =
    ["CTC" ; "CGC" ; "CGA" ; "CGG"]
    |> List.map Dna
    |> Set.ofSeq

let polyG =
    ["GGG"]
    |> List.map Dna
    |> Set.ofSeq

/// list of possible codons encoding each amino acid
let aa2Codons = makeRevLookup polyG
let aa2CodonsNonCare = makeRevLookup (rareCodons + polyG)

/// Count #differences between two codons
let diff (c1: Dna) (c2: Dna) =
    Array.map2 (fun a b -> if a = b then 0 else 1) c1.arr c2.arr |> Array.sum

/// How far left is desired change - want a codon change that minimizes base
/// pair changes on right hand side
let diffLeft (c1: Dna) (c2: Dna) =
    Array.mapi2 (fun i a b -> if a<>b then (i+1) else 0) c1.arr c2.arr |> Array.sum

/// How far right is desired change - want a codon change that minimizes base
/// pair changes on left hand side
let diffRight (c1: Dna) (c2: Dna) =
    Array.mapi2 (fun i a b -> if a<>b then (4-i) else 0) c1.arr c2.arr |> Array.sum

/// Count GC bases in sequence
let gcCount (c: Dna) =
    c.arr |> Array.sumBy (fun c ->
        if c = 'G' || c = 'C' || c = 'g' || c = 'c' then 1 else 0)

/// Pick a codon for an amino acid that is maximally different to a given codon
/// given a comparison function cmp that gives a positive score for degree of difference
let selectMutCodonBase
    cmp (cl:CodonLookup) (minFreq:float) (origCodon: Dna) (targetAA:char) =

    aa2Codons.[targetAA]
    |> Set.fold
        (fun (bestDiff, bestGC, bestFreq, bestCodon) thisCodon ->
            let thisFreq =
                match cl.Codon(thisCodon.str) with
                | None -> 0.0
                | Some(f) -> f.relFreq1

            let d = cmp thisCodon origCodon
            let better = 
                d> bestDiff ||
                (d=bestDiff && (gcCount thisCodon > bestGC)) ||
                (thisFreq >=minFreq && bestFreq < minFreq)

            if better then (d,gcCount thisCodon,thisFreq,thisCodon)
            else (bestDiff,bestGC,bestFreq,bestCodon)
            )
        (-1, -1, 0.0, Dna(""))
    |> fun (_,_,_,codon) -> codon

/// Find codon that is maximally different given a function to calculate different
let selectMutCodon = selectMutCodonBase diff
/// Variant of selectMutCodon, that forces the base pair difference as far left as possible
let selectMutCodonLeft = selectMutCodonBase diffLeft
/// Variant of selectMutCodon that forces the base pair differences as far right as possible
let selectMutCodonRight = selectMutCodonBase diffRight

/// expand a simple mutation inline with a part
let expandSimpleMut (asAACheck:bool) (_:GenomeDef) (g:PartIdLegacy) (m:Mutation) : GslSourceCode =

    let dna = 
        let part = fetchPart g.id |> returnOrFail
        part.dna
    // Now split by type of mutation and check the original base/amino acid is legit
    match m.mType with
    | NT ->
        if m.loc <=0 || m.loc > dna.Length then
            failwithf
                "ERROR: mutation position %d is outside range of rabit %s"
                m.loc g.id
        if dna.[m.loc-1] <> m.f then
            failwithf
                "ERROR: existing base at position %d in rabit %s is %c not %c"
                m.loc g.id (dna.[m.loc-1]) m.f

        // Design for mutation

        let lhs = m.loc-1 // before the mutated base
        let rhs = m.loc+1 // after the mutated base
        let id = g.id
        sprintf
            "@%s[1:%d] {#dnasrc %s}; /%c/ {#inline }; @%s[%d:-1E] {#dnasrc %s} "
            id lhs id m.t id rhs id
        |> GslSourceCode
    | AA ->
        if m.loc <=0 || m.loc > dna.Length/3 then
            failwithf
                "ERROR: mutation position %d outside range of rabit %s amino acids"
                m.loc g.id

        let currentCodon = (dna.[(m.loc-1)*3..(m.loc-1)*3+2]).arr

        // Ensure we are in the right place in the gene
        if (codon2aa currentCodon <> m.f) && asAACheck then
            failwithf
                "ERROR: for mutation %c%d%c , gene %s has amino acid %c (%s) in that position not %c"
                m.f m.loc m.t g.id (codon2aa currentCodon) (arr2seq currentCodon) m.f
        else
            let id = g.id
            let lhs = m.loc-1
            let rhs = m.loc+1
            sprintf
                "@%s[1:%da] {#dnasrc %s}; /$%c/ {#inline }; @%s[%da:-1E] {#dnasrc %s} "
                id lhs id m.t id rhs id
            |> GslSourceCode


/// 5' promoter mutation design - construct mutation with overlapping primers,
/// repeat promoter region around marker to minimize distruption and allow loop out
/// and use a heterology block downstream to prevent crossovers that eliminate the mutation
/// a         b            a         b          b+2                 c
/// promoterRep ; marker ; promoterRep(mutation)promoter ...ATG..... HB ....
let private classicPromoterNT gene name (f:sgd.Feature) (rg:GenomeDef) (m:Mutation) =
    // TODO - this design isn't compatible with the thumper workflow, as it involves a 4 piece rabit

    // m.pos is the zero based version of the offset from the 'A' in the start codon
    // note counterintuitively, we don't have to remove 1 from the '1' based coordinate system,
    // that only happens for the positive coordinate system
    let errorDesc = name

    // b is the base before the mutation, so one base upstream of the promoter mutation
    // b is the basepair before the mutation relative to gene orientation
    let b = (m.loc-1)*1<ZeroOffset> 
    let genomicCoord =
        (if f.fwd then f.l-m.loc else f.r - m.loc (* remember m.pos is negative *) )
      * 1<ZeroOffset>

    let existing =
        rg.Dna(errorDesc,sprintf "%d" f.chr , genomicCoord,genomicCoord)
        |> DnaOps.revCompIf (not f.fwd)

    if existing.[0] <> m.f then
        let diag =
            if f.fwd then
                rg.Dna(errorDesc,sprintf "%d" f.chr , genomicCoord,(f.l+2)*1<ZeroOffset>)
            else
                rg.Dna(errorDesc,sprintf "%d" f.chr , (f.r-2)*1<ZeroOffset>,genomicCoord).RevComp()  

        failwithf
            "ERROR: g%s promoter mutation at %d should be base %c a (in gene orientation) and is %c instead\n leading gene oriented seq=%O"
            name m.loc m.f (existing.[0]) diag
                        
    // Ensure we capture promoter and stay far enough away from b
    let a = (b-800<ZeroOffset>)
    let c' = max (b+200<ZeroOffset>) (29<ZeroOffset>) 
    let c = (c'/3) * 3 + 2<ZeroOffset>
                    
    sprintf
        "%s[~%A:%A] {#name %s.5us};### ;%s[~%A:%A] {#name %s.hb} ; /%c/ {#inline };%s[%A:%A];~ ;%s[%A:~%A]"
        gene (zero2One a) (zero2One b) name
        gene (zero2One a) (zero2One b) name
        m.t // replacement base
        gene (zero2One (b+2<ZeroOffset>)) (zero2One c)
        gene (zero2One (c+1<ZeroOffset>)) (zero2One (c+1000<ZeroOffset>))
    |> GslSourceCode

let private classicCodingNT
        _ (* verbose*)
        g
        (_:sgd.Feature)
        (_:GenomeDef)
        (m:Mutation) =
    // Not going to implement this completely just yet - i.e no heterology block.
    // This has to be used as part of a larger design that ensures swapping
    sprintf "%s[1S:%A];/%c/ {#inline };%s[%A:-1E] " g (m.loc-1) m.t g (m.loc+1)
    |> GslSourceCode


/// Classic reference implementation for an allele swap always
/// available but overwritten by other implementations
let jobScorerClassicAAMut _ = Some 0.0<PluginScore>

/// Decide which end of the gene the mutation is closer to, 
/// and pick design
/// Gene is the actual gene name,  name is the #name entry or full gYNG2$C227Y entry
let classicAAMut (dp:AlleleSwapDesignParams) =
    let minFreq = 0.05
    let x1 = z2i dp.mutOff
    let x2 = (z2i dp.mutOff) + 2

    if x2 >= dp.orf.Length+3 (* include stop codon *) then
        failwithf
            "ERROR: attempting to mutate amino acid position %d which is outside ORF length %d bases"
            dp.m.loc dp.orf.Length
    let currentCodon = dp.orf.[x1..x2]
// Removing: this seems to be checked by the parent function
//    // Ensure we are in the right place in the gene
//    if not (codon2aa currentCodon = m.f) && asAACheck then
//        printfn "ORF: %s" (arr2seq orf)
//        failwithf
//            "ERROR: for mutation %c%d%c , gene g%s has amino acid %c (%s) in that position not %c"
//            m.f m.pos m.t name (codon2aa currentCodon) (arr2seq currentCodon) m.f

    // If mutation is relatively far from 3' end and close enough to 5' end, use 5' design
    // but we prefer 3' designs since they are less disruptive
    // Can't get too close to 5' end and still insert hB on left, so will need a 5' design
    // 20120204:  Chris suggested changing 50 -> 30bp as the proximity limit for 5' design
    if (dp.endPref = NTERM ||
            (dp.mutOff < 30<ZeroOffset> ||
             (dp.mutOff < 1000<ZeroOffset> && dp.len-dp.mutOff > 2500<ZeroOffset>)
        )) then
        let mutSeq = selectMutCodonRight dp.codonLookup minFreq currentCodon dp.m.t

        assert( (DnaOps.translate mutSeq).[0] = dp.m.t)
        // 5' end design, need to rewrite promoter and put in marker
        //                                   b
        // a......-1; marker ; a...ATG.......Mutation ; HB ...  HB + 700
        let a = -1000<ZeroOffset> // TODO - could adjust this based on intergenic distance
        let a2 = -600<ZeroOffset> // Promoter region

        // 3' end design, classic
        sprintf
            "%s[~%A:~-100] {#name %s.5us} ;### ;%s[~%A:%A] {#name %s.hb} ;/%O/ {#inline };~ ;%s[%A:~%A]"
            dp.gene
            (zero2One a)
            dp.name
            dp.gene
            (zero2One a2)
            (zero2One (dp.mutOff-1<ZeroOffset>))
            dp.name 
            mutSeq
            dp.gene
            (dp.mutOff+3<ZeroOffset> |> zero2One)
            (dp.mutOff+703<ZeroOffset> |> zero2One)
    else
        let mutSeq = selectMutCodonLeft dp.codonLookup minFreq currentCodon dp.m.t
        assert( (DnaOps.translate mutSeq).[0] = dp.m.t)
                
        //  a            b                       c
        //  US700.......Mutation; HB ; dnaToDS200 ; marker ; 800bp downstream including 200bp repeat
        let a' = dp.mutOff-700<ZeroOffset> // Pick 700 so we can still sequence through
        let a = if (zero2One a') = 0<OneOffset> then 1<OneOffset> else zero2One a'
        let c = 200<OneOffset> 
        sprintf
            "%s[~%A:%A] {#name %s.hb} ;~ ;/%O/ {#inline };%s[%A:~%AE] ;### ;%s[1E:~%AE] {#name %s.3ds} "
            dp.gene
            a
            ((zero2One (dp.mutOff-1<ZeroOffset>))) 
            dp.name
            mutSeq
            dp.gene
            (dp.mutOff+3<ZeroOffset> |> zero2One)
            c
            dp.gene
            (c+600<OneOffset>)
            dp.name
    |> GslSourceCode
            


 //----- General entry point for allele swaps

/// expand an allele swap.  takes verbose/GenomeDef/gene and mutation    
/// Common entry point for different types of designs.  Does sanity checking and basic lookups
/// then passes off expansion to specific subdesign routines
let expandAS
        (providers : AlleleSwapProvider list)
        (asAACheck:bool)
        (name:string)
        (verbose:bool)
        (rg:GenomeDef)
        (codonLookup:CodonLookup)
        (g:string)
        (m:Mutation)
        (endPref:EndPref)
        (capa:Capabilities)
        (pragmas:PragmaCollection)
        (longStyle:bool) =

    if verbose then printf "Processing mutation gene:%s %A\n" g m

    // remove prefix of gene name and retrieve feature description
    let f = rg.get(g.[1..])
    let errorDesc = name

    match m.mType with
    | NT ->   
        match m.loc with
        | x when x = 0 -> failwithf "ERROR: illegal mutation offset zero for %s:%A\n" g m
        | x when x < 0 -> classicPromoterNT g name f rg m // Point mutation in promoter
        | x when x > 0 && x < f.r-f.l+1 -> classicCodingNT verbose g f rg m // Point mutation in coding part of gene
        | _ -> failwithf "ERROR: unimplemented, 3' terminator mutations for %s:%A" g m
    | AA -> 
        // General checks we should always make for a coding amino acid change

        // Gen length of ORF
        let len = (f.r - f.l + 1) * 1<ZeroOffset>
        let b = (m.loc-1) * 3<ZeroOffset> // zero based coordinates, location of mutant codon

        // Extend ORF to include stop codon so we can mutate that if needed
        let l',r' = if f.fwd then f.l,f.r+3  else f.l-3,f.r
        let orf =
            rg.Dna(errorDesc,sprintf "%d" f.chr,l'*1<ZeroOffset>,r'*1<ZeroOffset>)
            |> DnaOps.revCompIf (not f.fwd)

        /// Orf sequence plus "orfPlusMargin" 
        let orfPlus =
            rg.Dna(
                errorDesc,
                sprintf "%d" f.chr,
                ((l'-orfPlusMargin ) |> max 0)*1<ZeroOffset>,
                (r'+orfPlusMargin)*1<ZeroOffset>)
            |> DnaOps.revCompIf (not f.fwd)

        let x1 = z2i b
        let x2 = (z2i b) + 2

        if x2 >= orf.Length+3 then // Include stop codon
            failwithf
                "ERROR: attempting to mutate amino acid position %d which is outside ORF length %d bases"
                m.loc orf.Length
            
        let currentCodon = orf.[x1..x2]

        // Ensure we are in the right place in the gene
        if (codon2aa currentCodon.arr <> m.f) && asAACheck then
            printfn "ORF: %O" orf
            failwithf
                "ERROR: for mutation %c%d%c , gene %s has amino acid %c (%O) in that position not %c"
                m.f m.loc m.t g (codon2aa currentCodon.arr) currentCodon m.f
        
        let designParams ={ verbose=verbose
                            longStyle=longStyle
                            codonLookup=codonLookup
                            endPref=endPref
                            gene=g
                            name=name
                            rg=rg
                            f=f
                            m=m
                            len=len
                            mutOff=b
                            orf=orf
                            orfPlus=orfPlus
                            pragmas=pragmas
                        }
        // Choose provider
        let fn = providers |> 
                     List.choose (fun provider -> 
                                            match provider.jobScorer capa with
                                                | None -> None
                                                | Some(score) -> Some (score,provider.provider)
                                    ) |>
                     List.sortWith(fun (a,_) (b,_) -> compare b a)  |> // Note sort largest first
                     List.head |>
                     snd

        fn designParams

// Heterology block implementation
// ============================================================================
             
(*                    
/// Create a heterology block
/// Remove part of the right hand slice and replace with rewritten sequence
///
let private generateRightHB' (prefix:char array) (slice:char array) =
    // Dumbest implementation imaginable, cut away 10 amino acids
    let current = slice.[0..29] |> translate 
    // Pick alternative codons
    let alt = current |> Array.mapi (fun i codon -> selectMutCodon codonLookup minFreq (slice.[i*3..i*3+2]) codon) |> Array.concat
    alt // return the alternative starting sequence
*)

type private HBMove = HBLEFT | HBRIGHT | HBBOTH 

///
/// Generate a heterology block by eating into the downstream sequencing
/// and replacing it with a sequence that translates into the equivalent
/// amino acid sequence.  Be mindful of primer design and ensure that
/// the heterology block is long enough while having a good Tm of overlap
/// and leaving enough bases in a 60mer to overlap the adjacent regions
/// Takes up  prefix and down  which are the adjacent sequence and the prefix sequence
/// (e.g. a mutation) we'd like just before the heterology block
/// 
/// Takes an optional targetAALen if a particular number of changed amino acids are required
/// This overrides any Tm optimization considerations.
/// Returns alternative sequence for down not including prefix sequence, but assuming Tm calculation will be for
/// prefix + down
let private generateHBCore
        (cl:CodonLookup)
        (minFreq:float)
        (targetAALen:int option)
        (dp:DesignParams)
        (fwd:bool)
        (up: Dna)
        (prefix: Dna)
        (down: Dna) =
    // Parameters
    // Want ~10 amino acids changed
    // 
    // ds500Rev 65
    // Overlap  65
    // us500Fwd 65
    // cPCR     56
    // leftHB overhang   56
    // rightHB overhang   56
    let localVerbose = false
    let meltHB = 70.0<C>
    let meltHBNeighbour = 60.0<C>
    let maxOligoLen = dp.pp.maxLength

    let upRev = up.RevComp()
    // TODO - if we are near the 5' end of the gene we should use non rare
    // codons.  Can't currently figure out position from params
    // Also need to ensure we don't chew back past 5' start codon
    
    // Build a really long het block to use as a template
    // Note which strand we are translating on.  If this is a reverse (left) design, watch other strand
    
    /// Have to ensure this is a multiple of 3 to ensure correct translation
    /// Make a decent length of downstream alternatives, at least 120 bp or max if there is one but not longer
    /// than available dna segment
    let transLen =
        (down.Length/3*3)
        |> min (match targetAALen with | None -> 120 | Some(v) -> v*3)

    let downTrans = down.[0..transLen-1] |> DnaOps.revCompIf (not fwd)
    let current = DnaOps.translate downTrans

    // Pick alternative codons
    let alt =
        current
        |> Array.mapi (fun i aa -> 
            let orig = downTrans.[i*3..i*3+2]
            // ensure we passed in something sensible
            assert ( (DnaOps.translate orig).[0] = aa )
            selectMutCodon cl minFreq orig aa)
        |> DnaOps.concat
        // If the template was flipped to get translation right, flip it back
        |> DnaOps.revCompIf (not fwd)

    let altRetranslated = alt |> DnaOps.revCompIf (not fwd) |> DnaOps.translate

    if localVerbose then printf "templat: %O\n" downTrans.[0..min 60 downTrans.Length]
    if localVerbose then printf "    alt: %O\n" alt
    if localVerbose then printf "Current:%O\n" current
    if localVerbose then printf "Alt    :%s\n" (altRetranslated |> arr2seq)

     // ensure the sequence still reads the same amino acids out
    assert (DnaOps.translate downTrans = altRetranslated )
    
    match targetAALen with
    | Some(v) -> alt.[..v*3-1]
    | None ->
        /// How many bases do we need to go out till 10 amino acids have had changed codons
        let rec countTo10 i j =
            if i = 10 || j+3 = alt.Length then
                j
            else if alt.[j..j+2] = down.[j..j+2] then
                countTo10 i (j+3)
            else countTo10 (i+1) (j+3)

        let ten = countTo10 0 0
        // Decide on a cutover point between the hetblock and the adjacent wild type DNA
        // so we meet tm requirements for the HB part (A+B) , and tm requirements for the wt part (C)
        // and tm requirements for the other reverse adjacent sequence 
        // and the hetblock is long enough and the total primer doesn't exceed 60bp
        //                          i
        //         pref~~~~~~~~~~~~~~~~~~~wtwtwtwtwtwtwtwtwtwtwt
        // upupupupprefwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtw
        // DDDDDDDDAAAABBBBBBBBBBBBBCCCCCCCCCCCCCCC
        // Pick ideal point i, first base post het block
        //let localVerbose = true
        let minC = 18
        let tm2PrimerParams = 20.0<1/C> // Cost of deviation in the flanking region
        let tm1PrimerParams = 20.0<1/C> // Cost of deviation in the HB
        let hbLenPrimerParams = 30.0 // Cost of hb not reaching desired # amino acid changes
        let rec transitionOpt left right i =
            // Eval position i
            let AB = DnaOps.append prefix alt.[..i-1]
    
            let penHB = { dp.pp with maxLength = maxOligoLen - minC}
            let task = { tag ="HBFwd" ; temp = AB.arr; align = ANCHOR.LEFT ; strand = STRAND.TOP ; offset=0 ; targetTemp = meltHB;
                            sequencePenalties  = None}
    
            let score,primerLen,move =
                match oligoDesign false penHB task with
                | None -> 999.0,0,HBBOTH // Failed primer generation, not an option
                | Some(o1) -> 
                    // HB primer designed, what's left for the C region?
                
                    let primer1 =
                        if o1.oligo.Length % 3 = 0 then Dna(o1.oligo)
                        else AB.[0..((o1.oligo.Length/3+1)*3)-1]

                    let penC =
                        {dp.pp with
                            maxLength = maxOligoLen - primer1.Length;
                            minLength = max 10 (54 - primer1.Length)}
                    let task2 =
                        {tag ="CFwd";
                         temp = down.[primer1.Length..primer1.Length+penC.maxLength].arr;
                         align = ANCHOR.LEFT;
                         strand = STRAND.TOP; 
                         offset = 0;
                         targetTemp = meltHBNeighbour;
                         sequencePenalties  = None}

                    let move1 =
                        if o1.temp < meltHB then HBRIGHT
                        else if o1.temp > meltHB then HBLEFT
                        else HBRIGHT
            
                    match oligoDesign false penC task2 with
                    | None -> 999.0,0,HBLEFT // Failed primer 2 generation, not an option
                    | Some(o2) ->
                        // We have two primers now, one over HB and one matching RHS
                        // Oligo 3, upstream
                        let task3 =
                           {tag ="DRev";
                            temp = upRev.[0..min (upRev.Length-1) (penC.maxLength-1)].arr;
                            align = ANCHOR.LEFT;
                            strand = STRAND.TOP; 
                            offset = 0;
                            targetTemp = meltHBNeighbour;
                            sequencePenalties  = None}
                
                        match oligoDesign false penC task3 with
                        | None -> 999.0,0,HBLEFT // Failed primer 3 generation, not an option
                        | Some(o3) ->
                            // We have three primers now and can calculate some sense of optimiality for this
                            // heterology block.  Take into account the two tms and 
                            let score = 
                                (o1.temp-meltHB|> abs ) * tm1PrimerParams +
                                (o2.temp-meltHBNeighbour |> abs ) * tm2PrimerParams +
                                (o3.temp-meltHBNeighbour |> abs ) * tm2PrimerParams +
                                (if primer1.Length <= ten then
                                    (ten-primer1.Length |> float) * hbLenPrimerParams + 15.0
                                 else if primer1.Length < ten+15 then (ten+15-primer1.Length |> float)
                                 else 0.0)

                            assert(score>=0.0)
                            // WARNING - can't customize the sodium and primer concentration values here
                            let x = primercore.temp dp.pp o3.oligo o3.oligo.Length
                            if localVerbose then
                                printf
                                    "i=%-3d p1l=%-3d p2l=%-3d p3l=%-3d p1t=%-3A p2t=%-3A p3t=%-3A %f score=%f %s %s\n" i 
                                    o1.oligo.Length o2.oligo.Length o3.oligo.Length
                                    o1.temp o2.temp o3.temp (float x) score (arr2seq o2.oligo) (arr2seq o3.oligo)

                            let move2 =
                                if o2.temp < meltHBNeighbour then HBRIGHT
                                else if o2.temp > meltHBNeighbour then HBLEFT
                                else HBRIGHT

                            // Return the score, the length of the HB portion of the primer and the suggested direction to move
                            if move1 = move2 then score,primer1.Length,move1
                            else score,primer1.Length,HBBOTH
                        
            match score,move with
            | 0.0, _ -> score, primerLen // done
            | _, HBLEFT when i>left ->
                let score',primerLen' = transitionOpt left (i-3) (i-3)
                if score' < score then score',primerLen' else score,primerLen
            | _, HBBOTH when i>left && i=right -> 
                let score',primerLen' = transitionOpt left (i-3) (i-3)
                if score' < score then score',primerLen' else score,primerLen
            | _, HBRIGHT when i<right ->
                let score',primerLen' = transitionOpt (i+3) right (i+3)
                if score' < score then score',primerLen' else score,primerLen
            | _, HBBOTH when i<right && i=left -> 
                let score',primerLen' = transitionOpt (i+3) right (i+3)
                if score' < score then score',primerLen' else score,primerLen
            | _, HBBOTH when i > left && i < right ->
                let scoreR,primerLenL = transitionOpt (i+3) right (i+3)
                let scoreL,primerLenR = transitionOpt left (i-3) (i-3)
                let score2,primer2 =
                    if scoreL < scoreR then scoreL,primerLenL
                    else scoreR,primerLenR
                if score2 < score then score2,primer2 else score,primerLen
            | _, _ -> score,primerLen
                 
        let left = 6 * 3 // min 3 amino acids changed
        let right = 42 // min 18 bp for overlap primer
        let mid = maxOligoLen / 2
        let finalScore,finalPrimer = transitionOpt left right mid                        
        if localVerbose then printf "finalScore=%f finalPrimerLen=%d\n" finalScore finalPrimer
        //Array.append prefix (alt.[..finalPrimer-4])
        //printf "\n// final primer before clipping first 3:\n// %s\n" (arr2seq alt)
        //alt.[3..finalPrimer-1] //else alt.[0..finalPrimer-4]
        // Note - the alternative sequence is flipped around if needed
        alt.[..finalPrimer-1]

/// Create a heterology block, removing part of the right hand (down) slice
/// sequence and replacing with a rewritten sequence
/// Entry point into this module
let generateRightHB
        (cl:CodonLookup)
        (minFreq:float)
        (targetAALen : int option)
        (pp: DesignParams)
        (up: Dna)
        (prefix: Dna)
        (down: Dna) =
    generateHBCore cl minFreq targetAALen pp true up prefix down    

/// Create a heterology block, removing part of the left hand (up) slice sequence and replacing with a rewritten sequence    
/// Entry point into this module
let generateLeftHB
        (cl:CodonLookup)
        (minFreq:float) 
        (targetAALen : int option) 
        (pp: DesignParams) 
        (up: Dna) 
        (prefix: Dna) 
        (down: Dna) = 
    // generateRight takes upstream prefix and downstream sequence (to be eaten into)
    // reverse everything so that it works as a right design
    //
    //    upupupupup pref downdowndowndown
    generateHBCore
        cl
        minFreq
        targetAALen
        pp
        false
        (down.RevComp())
        (prefix.RevComp())
        (up.RevComp())
    |> DnaOps.revComp
