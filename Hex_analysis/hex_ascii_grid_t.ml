(* 

#use"Hex_analysis/hex_ascii_grid_t.ml";;

*)

type t = {
   beneficiary : Hex_player_t.t ;
   dimension : int ;
   data : ( (int * int) * string) list;
};;