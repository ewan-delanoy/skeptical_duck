(* 

#use"Hex_analysis/hex_connector_t.ml";;

*)

type t= {
     entry    : Hex_island_t.t ;
     junction : (int,int) Set_of_poly_pairs_t.t ;
     exit     : Hex_island_t.t ;
};;