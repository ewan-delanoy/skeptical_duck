(* 

#use"Hex_analysis/hex_movable_ground_linker_data_t.ml";;

When tha data is reducible to pairs, the list l in the support field is such
that l[2*k-1] and l[2*k] are paired points.

*)

type t= {
    ground                : Hex_cardinal_direction_t.t ;
    distance_from_ground  : int ;
    is_reducible_to_pairs : bool ;
    apex                  : int*int ;
    support               : (int * int) list;
};;

