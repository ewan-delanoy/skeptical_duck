(* 

#use"Hex_analysis/hex_state_t.ml";;

*)

type t= {
   config_remains : Hex_fles_double_list_t.t ;
   games_remains : Hex_fg_double_list_t.t;
   openings_remains : Hex_untamed_opening_t.t list;
   moves_before : Hex_cell_t.t list;
   strong_moves_before : (Hex_cell_t.t option) * (Hex_cell_t.t list);
};;

