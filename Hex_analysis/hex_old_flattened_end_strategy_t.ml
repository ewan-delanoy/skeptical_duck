(* 

#use"Hex_analysis/hex_old_flattened_end_strategy_t.ml";;

"Flattened" means that the details of how the strategy proceeds are ommited,
and all that's left is the cells that are involved in it.
The "index" field refers to the index Hex_old_end_strategy_factory_t.t object.

*)

type t= {
   beneficiary : Hex_player_t.t;
   character : Hex_strategy_character_t.t;
   active_part : Hex_cell_set_t.t ; 
   passive_part : Hex_cell_set_t.t ; 
   index : int;
};;