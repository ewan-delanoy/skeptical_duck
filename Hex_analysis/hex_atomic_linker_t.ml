(* 
#use"Hex_analysis/hex_atomic_linker_t.ml";;

*)

type t= 
   Pair of Hex_cell_t.t * Hex_cell_t.t 
 | Eyed_claw of Hex_cardinal_direction_t.t * 
                Hex_cardinal_direction_t.t * 
                Hex_cell_t.t ;;