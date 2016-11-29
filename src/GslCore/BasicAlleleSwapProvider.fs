module BasicAlleleSwapProvider

open alleleSwaps
open PluginTypes

let basicAlleleSwapPlugin =
   {name = "classic_allele_swap";
    description = Some "Allele swap implementation using markers.";
    behaviors =
       [{name = None;
         description = None;
         behavior =
            AlleleSwapAA(
                {jobScorer = jobScorerClassicAAMut ;
                provider = classicAAMut})}
       ];
    providesPragmas = [];
    providesCapas = []};