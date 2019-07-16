(* 

#use"Hex_memory/hex_ina_state_t.ml";;

First coordinate is column index, second is row index

*)

type t= 
     Foreseen_so_far of Hax_checked_initial_game_t.t 
    |Awaiting_final_outcome of Hax_checked_initial_game_t.t ;;