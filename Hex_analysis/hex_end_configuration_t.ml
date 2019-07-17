(* 

#use"Hex_analysis/hex_end_configuration_t.ml";;


*)

type t= {
   beneficiary : Hax_player_t.t;
   active_part : Hax_cell_t.t list; 
   passive_part : Hax_cell_t.t list; 
};;