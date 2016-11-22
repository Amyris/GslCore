module BasicAlleleSwapProvider

open alleleSwaps
open PluginTypes


let basicAlleleSwapPlugin =
   {name = "allele swap"; // Marker based allele swap
    behaviors =
        [AlleleSwapAA(
           {jobScorer = jobScorerClassicAAMut ;
            provider = classicAAMut})];
    providesPragmas = [];
    providesCapas = []};