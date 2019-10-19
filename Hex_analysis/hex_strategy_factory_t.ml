(* 

#use"Hex_analysis/hex_strategy_factory_t.ml";;

*)

type t= F of 
  Hex_player_t.t *
 ((Hex_strategy_static_constructor_t.t * (int list) * Hex_end_configuration_t.t ) list);;
   