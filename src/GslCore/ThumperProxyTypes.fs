/// Call/return types for thumper proxy service
module ThumperProxyTypes

/// Single rabit instance that could be re-used in a design
type RabitCandidate ={
    id:int; 
    fivePrimerLinkCode:string; 
    threePrimerLinkCode:string; 
    breedCode:string; 
    direction:string; 
    inventoryStatusRollup:string
    dna:string
}

/// Rabit lookup reply structure.  Candidate rabits
type RabitLookupReply = {ok:bool; message:string; rabitArray:RabitCandidate[]}

type RabitLookupRequest = {insertName:string; breed:string}