(* 

#use"Hex_analysis/hex_strategy_constructor_t.ml";;

*)

type t= 
    Basic_Linker of Hex_cell_set_t.t * Hex_cell_pair_set_t.t
   |Glued 
   |Disjunction of Hex_cell_t.t  list;;
   