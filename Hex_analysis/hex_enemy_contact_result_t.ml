(* 

#use"Hex_analysis/hex_enemy_contact_result_t.ml";;

*)

type t= 
   No_change
  |Weakening of Hex_flattened_end_strategy_t.t
  |Destruction;;