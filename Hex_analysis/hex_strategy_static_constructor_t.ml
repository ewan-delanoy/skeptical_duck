(* 

#use"Hex_analysis/hex_strategy_static_constructor_t.ml";;

*)

type t= 
    Basic_Linker of Hex_cell_set_t.t * Hex_cell_pair_set_t.t
   |Gluing 
   |Disjunction of Hex_cell_t.t  list;;
   