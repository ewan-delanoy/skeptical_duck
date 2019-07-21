(* 

#use"Hex_analysis/hex_state_t.ml";;

*)

type t= {
   config_remains : Hex_ec_double_list_t.t ;
   games_remains : Hex_fg_double_list_t.t;
   moves_before : Hex_cell_t.t list 
};;