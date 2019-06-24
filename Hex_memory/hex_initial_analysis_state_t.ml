(* 

#use"Hex_memory/hex_analysis_state_t.ml";;

First coordinate is column index, second is row index

*)

type t= 
     Foreseen_so_far of Hex_partial_game_t.t 
    |Awaiting_final_outcome of Hex_partial_game_t.t ;;