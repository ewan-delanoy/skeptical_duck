(* 

#use"Hex_memory/Hex_final_analysis/hex_decisive_configuration_t.ml";;



*)

type t= {
   beneficiary : Hex_player_t.t;
   active_part : Hex_cell_t.t list; 
   passive_part : Hex_cell_t.t list; 
};;