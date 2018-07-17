module BasicMarkerProvider
open constants
open LegacyParseTypes
open commonTypes
open Amyris.Dna
open PluginTypes
open DnaCreation

/// Classic URA3 sequence that has always been hard coded into center of megastitches (from lib.fa)
let ura3Marker = Dna(
   "GTTCATCATCTCATGGATCTGCACATGAACAAACACCAGAGTCAAACGACGTTGAAATTG\
    AGGCTACTGCGCCAATTGATGACAATACAGACGATGATAACAAACCGAAGTTATCTGATG\
    TAGAAAAGGATTAAAGATGCTAAGAGATAGTGATGATATTTCATAAATAATGTAATTCTA\
    TATATGTTAATTACCTTTTTTGCGAGGCATATTTATGGTGAAGGATAAGTTTTGACCATC\
    AAAGAAGGTTAATGTGGCTGTGGTTTCAGGGTCCATAAAGCTTTTCAATTCATCTTTTTT\
    TTTTTTGTTCTTTTTTTTGATTCCGGTTTCTTTGAAATTTTTTTGATTCGGTAATCTCCG\
    AGCAGAAGGAAGAACGAAGGAAGGAGCACAGACTTAGATTGGTATATATACGCATATGTG\
    GTGTTGAAGAAACATGAAATTGCCCAGTATTCTTAACCCAACTGCACAGAACAAAAACCT\
    GCAGGAAACGAAGATAAATCATGTCGAAAGCTACATATAAGGAACGTGCTGCTACTCATC\
    CTAGTCCTGTTGCTGCCAAGCTATTTAATATCATGCACGAAAAGCAAACAAACTTGTGTG\
    CTTCATTGGATGTTCGTACCACCAAGGAATTACTGGAGTTAGTTGAAGCATTAGGTCCCA\
    AAATTTGTTTACTAAAAACACATGTGGATATCTTGACTGATTTTTCCATGGAGGGCACAG\
    TTAAGCCGCTAAAGGCATTATCCGCCAAGTACAATTTTTTACTCTTCGAAGACAGAAAAT\
    TTGCTGACATTGGTAATACAGTCAAATTGCAGTACTCTGCGGGTGTATACAGAATAGCAG\
    AATGGGCAGACATTACGAATGCACACGGTGTGGTGGGCCCAGGTATTGTTAGCGGTTTGA\
    AGCAGGCGGCGGAAGAAGTAACAAAGGAACCTAGAGGCCTTTTGATGTTAGCAGAATTGT\
    CATGCAAGGGCTCCCTAGCTACTGGAGAATATACTAAGGGTACTGTTGACATTGCGAAGA\
    GCGACAAAGATTTTGTTATCGGCTTTATTGCTCAAAGAGACATGGGTGGAAGAGATGAAG\
    GTTACGATTGGTTGATTATGACACCCGGTGTGGGTTTAGATGACAAGGGAGACGCATTGG\
    GTCAACAGTATAGAACCGTGGATGATGTGGTCTCTACAGGATCTGACATTATTATTGTTG\
    GAAGAGGACTATTTGCAAAGGGAAGGGATGCTAAGGTAGAGGGTGAACGTTACAGAAAAG\
    CAGGCTGGGAAGCATATTTGAGAAGATGCGGCCAGCAAAACTAAAAAACTGTATTATAAG\
    TAAATGCATGTATACTAAACTCACAAATTAGAGCTTCAATTTAATTATATCAGTTATTAC\
    CCGGGAATCTCGGTCGTAATGATTTCTATAATGACGAAAAAAAAAAAATTGGAAAGAAAA\
    AGCTTCATGGCCTTTATAAAAAGGAACTATCCAATACCTCGCCAGAACCAAGTAACAGTA")

/// Expand a marker part into DNA pieces.
/// Exception on failure.
let expandMarkerPartIntoUra3 dnaSource (ppp:PPP) =
   {id = None;
    extId = None;
    sliceName = getSliceName ppp;
    uri = getUri ppp; // TODO: should this marker have a static URI we always assign here?
    dna = ura3Marker;
    sourceChr = "library";
    sourceFr = 0<ZeroOffset>;
    sourceTo = (ura3Marker.Length-1)*1<ZeroOffset>;
    sourceFwd = true;
    sourceFrApprox = false;
    sourceToApprox = false;
    // Don't assign coordinates to pieces until later when we
    // decide how they are getting joined up
    template = Some ura3Marker;
    amplified = false;
    destFr = 0<ZeroOffset>;
    destTo = 0<ZeroOffset>;
    destFwd = ppp.fwd;
    description = "URA3 marker";
    sliceType = MARKER;
    dnaSource = dnaSource;
    pragmas = ppp.pr;
    breed = B_MARKER;
    materializedFrom = Some(ppp);
    annotations = []}

type Ura3MarkerProvider() =
    do
        ()
    with
    interface IMarkerProvider with
        member __.ProvidedArgs() = []
        member x.Configure(_) = x :> IMarkerProvider
        member x.ConfigureFromOptions(_) = x :> IMarkerProvider
        member x.CreateDna(task:MarkerMaterializationTask) = 
            expandMarkerPartIntoUra3 task.dnaSource task.ppp
        member x.IsLegal m = m.ToLower() = "ura3" || m.ToLower() = "default"
        member x.ListMarkers() = ["ura3"]
        member x.ScoreJob(_) = Some 0.0<PluginScore>

/// Original default URA3 behavior for materialized ### parts
let basicMarkerProviderURA3 =
   {name = "ura3 dropin marker provider";
    description = Some "Include default ura3 sequence in materialized ### sequences.";
    behaviors = [{name = None; description = None; behavior = MarkerProvider(Ura3MarkerProvider())}];
    providesPragmas = [];
    providesCapas = []};
