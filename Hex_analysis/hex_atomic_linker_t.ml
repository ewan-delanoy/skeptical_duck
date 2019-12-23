(* 
#use"Hex_analysis/hex_atomic_linker_t.ml";;

*)

type t= 
  Pair of Hex_cell_t.t * Hex_cell_t.t 
 |
 C of string * int ;;