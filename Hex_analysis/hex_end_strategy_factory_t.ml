(* 

#use"Hex_analysis/hex_strategy_factory_t.ml";;

*)

type t= F of 
  Hex_player_t.t *
 ((Hex_strategy_static_constructor_t.t * string * (int list) * Hex_flattened_end_strategy_t.t ) list);;
   