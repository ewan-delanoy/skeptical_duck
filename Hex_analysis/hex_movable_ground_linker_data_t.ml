(* 

#use"Hex_analysis/hex_movable_ground_linker_data_t.ml";;

*)

type t= {
    ground                : Hex_cardinal_direction_t.t ;
    distance_from_ground  : int ;
    is_reducible_to_pairs : bool ;
    apex                  : int*int ;
    support               : (int * int) list;
};;

