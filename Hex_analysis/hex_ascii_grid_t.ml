(* 

#use"Hex_analysis/hex_ascii_grid_t.ml";;

*)

type t = {
   beneficiary : Hex_player_t.t ;
   dimension : Hex_dimension_t.t ;
   data : ( (int * int) * string) list;
};;

