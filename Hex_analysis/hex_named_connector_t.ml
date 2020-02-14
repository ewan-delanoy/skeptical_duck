(* 

#use"Hex_analysis/hex_named_connector_t.ml";;

*)

type t= {
     name     : Hex_connector_name_t.t ;
     entry    : Hex_island_t.t ;
     junction : (int * int) list ; (* not ordered because we want to be able to associate pairs *)
     exit     : Hex_island_t.t ;
     apex     : (int * int) option;
};;