(* 

#use"Hex_analysis/hex_state_t.ml";;

*)

type t= {
   whoami : Hex_player_t.t ; 
   config_remains : Hex_fles_double_list_t.t ;
   games_remains : Hex_fg_double_list_t.t;
   moves_before : Hex_cell_t.t list;
   still_strong : bool;
   strong_moves_before : Hex_cell_t.t list;
};;