(* 

#use"Hex_analysis/hex_old_finished_game_t.ml";;

*)

type t= {
    dimension : int;
    winner : Hex_player_t.t;
    sequence_of_moves : Hex_cell_t.t list
};;