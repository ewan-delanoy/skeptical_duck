(* 

#use"Hex_analysis/hex_flattened_end_strategy_t.ml";;

"Flattened" means that the details of how the strategy proceeds are ommited,
and all that's left is the cells that are involved in it.

*)

type t= {
   beneficiary : Hex_player_t.t;
   active_part : Hex_cell_set_t.t ; 
   passive_part : Hex_cell_set_t.t ; 
   index : int;
};;