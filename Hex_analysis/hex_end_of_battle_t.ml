(* 

#use"Hex_analysis/hex_end_of_battle_t.ml";;

*)

type t= {
    dimension       : Hex_dimension_t.t ;
    winner          : Hex_player_t.t ; 
    ally_territory  : Hex_cell_set_t.t ; 
    enemy_territory : Hex_cell_set_t.t ;
} ;;