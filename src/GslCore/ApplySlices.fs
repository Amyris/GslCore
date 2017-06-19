module applySlices
open LegacyParseTypes
open commonTypes
open constants

/// Given a gene end and two one based offsets, calculate a new one based offset
let addOneOffset (ge:GeneEnd) (a:int<OneOffset>) (b:int<OneOffset>) = 
    match ge with
        | FivePrime ->
            if b > 0<OneOffset> then a+b-1<OneOffset> else a+b
        | ThreePrime ->
            if b < 0<OneOffset> then a+b+1<OneOffset> else a+b

/// What does it mean to apply further slice notations to an existing piece?
let rec applySlices verbose (mods : Mod list) (s:Slice) = 
    match mods with
        | [] -> s
        | SLICE(sl)::tl ->
            // Concatenate slices
            let s' = { // subsequent slices could be relative to either end of the existing slice
                  lApprox = (if sl.lApprox then true else s.lApprox ); rApprox = (if sl.rApprox then true else s.rApprox) ;
                  left = match sl.left.relTo with
                          | FivePrime -> { x = addOneOffset FivePrime sl.left.x s.left.x ; relTo = s.left.relTo}
                          | ThreePrime -> { x = addOneOffset ThreePrime sl.left.x s.right.x ; relTo = s.right.relTo}
                          ;
                  right = match sl.right.relTo with
                          | FivePrime -> { x = addOneOffset FivePrime sl.right.x s.left.x ; relTo = s.left.relTo}
                          | ThreePrime -> { x = addOneOffset ThreePrime sl.right.x s.right.x ; relTo = s.right.relTo}
            }
            applySlices verbose tl s'
        | x::tl -> 
            if verbose then printf "WARNING: ignoring unimplemented mod %A\n" x
            applySlices verbose tl s
