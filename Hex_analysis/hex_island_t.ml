(* 

#use"Hex_analysis/hex_island_t.ml";;

*)

type t= I of (Hex_cardinal_direction_t.t option) 
             * ( (int, int) Set_of_poly_pairs_t.t);;