(* 

#use"Hex_analysis/hex_flattened_end_strategy_t.ml";;

"Flattened" means that the details of how the strategy proceeds are ommited,
and all that's left is the cells that are involved in it.
The "index" field refers to the index Hex_end_strategy_factory_t.t object.

*)

type t= {
   dimension : Hex_dimension_t.t ;
   beneficiary : Hex_player_t.t;
   data : Hex_extended_molecular_t.t;
   index : int;
};;