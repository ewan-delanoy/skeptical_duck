(* 

#use"Hex_analysis/hex_ascii_grid_t.ml";;

*)

type t = {
   dimension : int ;
   data : ( (int * int) * string) list;
};;