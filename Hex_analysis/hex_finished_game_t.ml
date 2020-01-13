(* 

#use"Hex_analysis/hex_finished_game_t.ml";;

*)

type t= {
    dimension : Hex_dimension_t.t ;
    winner : Hex_player_t.t;
    sequence_of_moves : Hex_cell_t.t list
};;