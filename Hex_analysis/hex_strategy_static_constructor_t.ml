(* 

#use"Hex_analysis/hex_strategy_static_constructor_t.ml";;

For the Molecular variant, the fst (snd) arg is the passive (active) part.
For the Disjunction variant, each cell in the list argument represents one move
allowing to fall back in an already known winning position.

*)

type t= 
    Molecular of Hex_molecular_linker_t.t * Hex_cell_set_t.t 
   |Exhaustive_Disjunction of Hex_cell_t.t  list;;
   
   