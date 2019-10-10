(* 

#use"Hex_analysis/hex_end_configuration_t.ml";;


*)

type t= {
   beneficiary : Hex_player_t.t;
   active_part : Hex_cell_set_t.t ; 
   passive_part : Hex_cell_set_t.t ; 
};;