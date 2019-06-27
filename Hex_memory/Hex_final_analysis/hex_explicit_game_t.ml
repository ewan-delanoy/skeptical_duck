(* 

#use"Hex_memory/Hex_final_analysis/hex_explicit_game_t.ml";;

First coordinate is column index, second is row index

*)

type t= FG of (Hex_cell_t.t*Hex_abstract_color_t.t) list;;