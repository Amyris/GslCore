module BasicCodonProvider

open System
open System.Text.RegularExpressions
open Amyris.Bio.utils
open MathNet.Numerics.Random
open System.Collections.Generic
open System.IO
open System.Threading
open pragmaTypes
open Amyris.Bio.biolib
open Amyris.Bio.IO.CodonUsage
open Amyris.Bio.SuffixTree
open RefGenome
open PluginTypes
open commandConfig

(*

UUU 26.1(170666)  UCU 23.5(153557)  UAU 18.8(122728)  UGU  8.1( 52903)
UUC 18.4(120510)  UCC 14.2( 92923)  UAC 14.8( 96596)  UGC  4.8( 31095)
UUA 26.2(170884)  UCA 18.7(122028)  UAA  1.1(  6913)  UGA  0.7(  4447)
UUG 27.2(177573)  UCG  8.6( 55951)  UAG  0.5(  3312)  UGG 10.4( 67789)

CUU 12.3( 80076)  CCU 13.5( 88263)  CAU 13.6( 89007)  CGU  6.4( 41791)
etc

*)

type PopScore = { score : float ; pos : int}
type Chrom = {chrom : char array ; score : float ; problems : bool array}
type Result = { count : int ; best : Chrom ; prot : char []}

/// typical sequences we want to exclude from synthesized DNA       
let defaultBadSeqs = [ "GCTCTTC" (*sapI*); "GAAGAGC" (* sapI RC *); "GGGGGGG" (* Trp Gly TGG GGx *); 
                    "GTTTAAAC" (*PmeI*); "GGTCTC" (* BsaI *) ; "GAGACC" (* BsaI RC *) ; 
                    "CACCTGC" (* AarI *) ; "GCAGGTG" (* Aar1 RTC *) ;
                    "AAAAAAA" ;  "TTTTTTT" ; "CCCCCCC"  ] 

let defaultMinFreq = 0.1 //0.2 // 0.05
let defaultMaxRank = 5
let mutRate = 0.005
let mutRateHigh = 0.5
let popSize = 100
let generations = 100
let genNoProgress = 10
let selectionBias = 0.25
let defaultMerLen = 6
let defaultStartMargin = 100
let defaultCodonOptSeed = 170270

// Versions
// 1: legacy, initial implementation
// 2: includes 5prime optimization to match NYT usage
type CodonOptParams =
   {startMargin: int;
    merLen : int;
    badSeqs : string list;
    minFreq : float;
    maxRank : int; 
    codonAvoid : string list;
    seed : int; 
    algVersion : int;
    prefixes : string list;
    globalRepeatCheck : bool}

let defaultCodonOptParamsV2 = { startMargin = defaultStartMargin ; badSeqs = defaultBadSeqs ; maxRank = defaultMaxRank ; 
                                merLen = defaultMerLen ; minFreq = defaultMinFreq ; codonAvoid = [] ; seed = defaultCodonOptSeed ; 
                                algVersion = 2;
                                prefixes = ["ACCTCCCGCGACCTCCAAAATCGAACTACCTTCACA";//A linker
                                            "ACGCACGCACACTCCCGACAGACAACTAGCTTGATA";//B linker
                                            "ACCCCACCCGAAGTCGCGCAACCAACTAACTTTACA" // C reversed
                                ];
                                globalRepeatCheck = false
                            }

let defaultCodonOptParamsV1 = { startMargin = defaultStartMargin ; badSeqs = defaultBadSeqs ; maxRank = defaultMaxRank ; 
                                merLen = 8 ; minFreq = defaultMinFreq ; codonAvoid = [] ; seed = defaultCodonOptSeed 
                                algVersion = 1;
                                prefixes = []
                                globalRepeatCheck = false

                            }

let defaultCodonOptParams = defaultCodonOptParamsV2

// ================= 5prime scoring support
let base2Idx c = match c with | 'A' -> 0 | 'T' -> 1 | 'C' -> 2 | 'G' ->3 | _ as x -> failwithf "ERROR: bad base %c" x
    
// ============================================
type ScoreResult = { totalAffected:int ; probs:bool []}
            with member x.Score = (float x.totalAffected)

/// Evaluate a dna string given a set of optimization parameters.  Returns # of problems and vector in amino acid space of problem areas
let scoreWithCoords (rng:MersenneTwister) (cop : CodonOptParams) (stPrefix:SuffixTree) (s:string) = 
        
        let st = new SuffixTree(s)
        st.buildFwdChain()
        let probs = Array.init s.Length (fun _ -> false)
        for x,y in 
            seq { 
                for b in cop.badSeqs do 
                    for x in st.FindAll(b) do
                        yield x,x+b.Length-1
                for i in {0..min cop.startMargin (s.Length-cop.merLen)} do
                    let revMer = s.[i..i+cop.merLen-1].ToCharArray() |> revComp |> arr2seq 
                    let hits = st.FindAll(revMer)
                    let hitsInMargin = hits |> Array.filter (fun i -> i<cop.startMargin)
                    for x in hitsInMargin do
                        yield x,x+cop.merLen-1 // Mutate matching dest

                    

                    if hitsInMargin.Length<> 0 || stPrefix.FindAll(revMer).Length<>0 then
                        yield i,i+cop.merLen-1 // Mutate source as well
                // If needed, check for fwd fwd (tandem) repeats that could cause
                // assembly or stability problems
                if cop.globalRepeatCheck then
                    let repLen = 8
                    
                    for i in {0..(s.Length-repLen)} do
                        let thisMer = s.[i..i+repLen-1].ToCharArray()
                        for x in (st.FindAll(thisMer |> arr2seq ) |> Array.filter (fun j -> i<>j) ) do
                            if rng.Next()%10=0 then // mutate sparingly, we only need to disrupt a repeat in one place
                                yield x,x+repLen-1 // Mutate matching dest
                                yield i,i+repLen-1 // Mutate source as well

            }
            do
                for z in {x/3..y/3} do // convert to amino acid position
                    probs.[z] <- true // mark as problematic
        let totalAffected = (probs|> Array.fold (fun count x -> if x then (count+1) else count) 0 )
        {totalAffected = totalAffected ; probs = probs}

/// Genome specific data needed to generate a codon optimized sequence
type CodonOptData =
   {freq : Map<string,float>
    codonAvoid : string list}


//let loadCodonData (gd:GenomeDef) =
//    let env = gd.Env
//    let codonAvoid = getCodonAvoid gd
//    { 
//        codonAvoid = codonAvoid
//        freq = Path.Combine([| libRoot ;genome ;"codon.txt" |]) |> loadCodonTable
//    }


let parseCodonOptParams (pr:PragmaCollection) =
    let seed =
        match pr.TryGetOne("seed") with
        | None -> defaultCodonOptSeed
        | Some(s) ->
            match Int32.TryParse s with
            | true,s' -> s'
            | _ -> failwithf "ERROR: invalid integer '%s' for #seed" s

    let globalRepeatAvoidUser =
        match pr.TryGetOne("codonopt") with
        | None -> defaultCodonOptParams.globalRepeatCheck
        | Some(s) ->
            let parts =
                s.Split([|';'|],StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun s->
                    match (s.Split( [| '=' |],StringSplitOptions.RemoveEmptyEntries ) ) with
                    | [| name ; value |] -> (name,value)
                    | _ as x ->
                        failwithf
                            "ERROR: bad name=value pair '%A' in codonopt params" x)
                |> Map.ofArray

            let grc =
                match parts.TryFind "repeatcheck" with
                | None -> defaultCodonOptParams.globalRepeatCheck
                | Some(v) ->
                    match v.ToLower() with
                        | "true" -> true
                        | "false" -> false
                        | _ as x -> failwithf "ERROR: bad #codonopt repeatcheck param '%s' - should be true or false" x
            grc

    {defaultCodonOptParams with 
        globalRepeatCheck = globalRepeatAvoidUser;
        seed = seed
    }

let doCodonOpt verbose (cop:CodonOptParams) (codoptData:CodonOptData) (localSeed:int) (protSeq:string) =

    let codonTable' = codoptData.freq
    let avoid = cop.codonAvoid |> Set.ofList

    let codonTable = codonTable' |> Map.filter (fun k _ -> avoid.Contains(k) |> not )
    let rng = new MersenneTwister(localSeed)

    /// Restricted codon usage table focused on top codons
    let cut = prepCUT cop.minFreq cop.maxRank codonTable

    /// This is a more full codon usage table for use at the 5prime end
    let cutFullPre = prepCUT 0.01 99 codonTable

    // Prepare input sequence
    let p = protSeq.ToCharArray()

    /// Build an initial guess codon usgae proportionally to preference
    let randProt() = p |> Array.map (fun aa ->  cut.Choose(rng.NextDouble(),aa)
                                      ) |> Array.concat

    /// Targeted mutation of an existing sequence  
    let mutate (mutProfile:bool array) (a : char[]) =
        let a' = Array.copy a // avoid mutating argument
        let l = a.Length/3
        for i in {0..l-1} do
            if rng.NextDouble() < (if mutProfile.[i] then mutRateHigh else mutRate) then
                a'.[i*3..i*3+2] <- cut.Choose(rng.NextDouble(),p.[i])

        a'

    /// Take two chromosomes and matching mutation hotspot preferences and
    /// generate mutated/ crossed over versions
    let crossover (a:char[]) (mutProfileA:bool [])  (b:char[]) (mutProfileB:bool []) =
        // pick crossover point
        let i = rng.Next(a.Length/3)*3

        // Build mutated versions of chromosomes
        let a' = mutate mutProfileA a
        let b' = mutate mutProfileB b

        // Reconstruct mutated chromosome
        if i = 0 then
            (b', a')
        else
            (Array.concat [a'.[..i-1] ; b'.[i..] ]  , Array.concat [b'.[..i-1] ; a'.[i..] ] )
    
    let stPrefix = new SuffixTree(String.Join("N",cop.prefixes))
    stPrefix.buildFwdChain()
    /// Create initial random population
    let startPop = Array.init popSize (fun _ -> 
                                            let c = randProt()
                                            let r = scoreWithCoords rng cop stPrefix (arr2seq c)
                                            //let score,probs = r.totalAffected,r.probs
                                            {chrom = c ; score = (if cop.algVersion = 1 then float r.totalAffected else r.Score) ; problems = r.probs })

    /// Take through one round of mating/evolution
    let cycle (popIn : Chrom array) =
        // Large score is bad, sort so worst first
        let pop = popIn |> Array.sortWith (fun a b -> compare b.score a.score)

        // Assume minimization, worst is first after sorting, best is last
        let worst = pop.[0].score

        // Not sure why this was in there unless we want to do some scaling
        // let avg = pop |> Seq.map (fun x ->x.score) |> Seq.average

        // Invert scores so you get a bigger score for best chromosomes
        // which would previously have had low scores
        let modScore = pop |> Array.map (fun c -> c,(worst-c.score))

        // Get total modified score
        let total = modScore |> Array.fold (fun total (_,s) -> total + s) 0.0

        let choose() =
                // Chromosomes are sorted worst to best.  We pick a score and serially
                // subtract chromosomes till we find the range matching the random
                // score.  We do it smallest to largest to avoid rounding errors at the
                // end.  We bias the picking towards the top part of the population to
                // create more fitness difference between the best and the worst
                let r = (rng.NextDouble()*selectionBias + (1.0-selectionBias)) *float(total)

                let rec find i f = 
                        let chr,score = modScore.[i]
                        if score > f || i = modScore.Length-1 then chr else find (i+1) (f-score)
                let x = find 0 r
                //printf "c%d " x
                x
        let newPop = seq {
                            for _ in {0..pop.Length/2-1} do
                                let c1 = choose()
                                let c2 = choose()
                                let c3,c4 = crossover c1.chrom c1.problems c2.chrom c2.problems
                                let r3 = scoreWithCoords rng cop stPrefix (arr2seq c3)
                                //let s3,p3 = r3.totalAffected,r3.probs
                                let r4 = scoreWithCoords rng cop stPrefix (arr2seq c4)
                                //let s4,p4 = r4.totalAffected,r4.probs
                                yield {chrom=c3 ; score = (if cop.algVersion = 1 then float r3.totalAffected else r3.Score) ; problems = r3.probs}; 
                                yield {chrom = c4; score = (if cop.algVersion = 1 then float r4.totalAffected else r4.Score)  ; problems = r4.probs}
                            } |> Array.ofSeq
        newPop.[0] <- pop.[pop.Length-1] // Preserve best population member outright
        newPop

    let rec run verbose count lastScore sameFor (p : Chrom [])=
        let minScore = p |> Seq.map (fun c -> c.score) |> Seq.min
        if verbose then printf "cycle=%d min=%f\n" count minScore
        let sameFor',lastScore' = 
            match lastScore with
                | Some(ls) when abs(ls-p.[0].score)<0.000001 -> sameFor+1,Some(p.[0].score)
                | _ -> 0,Some(p.[0].score)

        if count = generations || p.[0].score <  0.000001 || sameFor' >= genNoProgress then count,p
        else 
            let newPop = cycle p
                
            run verbose (count+1) lastScore' sameFor' newPop

    let count,finalPop = run verbose 0 None 0 startPop
    let best = finalPop.[0]

    //let s = best.chrom |> arr2seq

    let trans = Amyris.Bio.biolib.translate best.chrom
    for i in 0..3..best.chrom.Length-3 do
        let codon = best.chrom.[i..i+2] |> arr2seq
        assert (avoid.Contains(codon) |> not && cutFullPre.byCodon.ContainsKey(codon))
    assert (trans = protSeq.ToCharArray())

    if verbose then
        printfn "codopt: %s" (arr2seq best.chrom)
        printfn "        %s" (
                                String.Join (
                                   "",
                                    [| for i in 0..best.chrom.Length-1 -> string (i%3) |]
                                )
                             )
    let final = {count = count ; best = best ; prot = p}
    final.best.chrom |> arr2seq // convert to final DNA sequence as a string

/// Given a library root, load (and cache) genomes on demand
type CodonTableCache(libRoot:string) = class
    let cache = new Dictionary<string,CodonOptData>()
    let semaphore = "sequential access!"
    do
        ()

    member x.Get(gd:GenomeDef) =
        lock semaphore (fun () ->
            let genome = gd.Name
            if cache.ContainsKey(genome) then cache.[genome]
            else
                let table = Path.Combine([| libRoot ;genome ;"codon.txt" |]) |> loadCodonTable
                let data = { freq = table ; codonAvoid = gd.GetCodonAvoid()}
                cache.[genome] <- data
                data)
end

// ========================
// plugin interface to the pure functions above
// ========================

type BasicCodonProvider = {parameters: CodonOptParams option; cache: CodonTableCache option}
    with
    member x.Parameters =
        match x.parameters with
        | Some(p) -> p
        | None -> failwith "Codon optimizer is not configured; must run x.Setup(...) first."
    member x.CodonTableCache = 
        match x.cache with
        | Some(c) -> c
        | None -> failwith "Codon optimizer is not configured; must run x.Configure(...) first."
    interface ICodonProvider with
        member x.ProvidedArgs() = []
        member x.Configure(arg) =
            if arg.spec = libCmdArg.spec then
                /// Use lib dir to initialize cache table.
                let libDir = arg.values.[0]
                {x with cache = Some(CodonTableCache(libDir))}
            else x
            :> ICodonProvider
        member x.Setup(pc) =
            let configuredParams = parseCodonOptParams pc
            {x with parameters = Some(configuredParams)} :> ICodonProvider
        member x.DoCodonOpt verbose seedOverride rg protSeq =
            let parameters = x.Parameters
            let cache = x.CodonTableCache
            let seed = defaultArg seedOverride parameters.seed
            let data = cache.Get(rg)
            doCodonOpt verbose parameters data seed protSeq
        member x.GetCodonLookupTable(rg) =
            x.CodonTableCache.Get(rg)
            |> fun cod -> Amyris.Bio.IO.CodonUsage.prepCUT 0.0 100 cod.freq

/// Plugin providing basic codon optimization and lookup.
let basicCodonProviderPlugin = 
    {name = "basic_codon_provider";
     description = Some("Simple codon optimization plugin providing basic behaviors.");
     behaviors =
       [{name = None;
         description = None;
         behavior = CodonProvider({parameters = None; cache = None})}
       ];
     providesPragmas = [];
     providesCapas = []}