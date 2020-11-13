(* 

#use"Hex_analysis/hex_body_movement_t.ml";;

Data describing the border component of a Hex_island_t.t object

*)


type t= {
     reflect : bool ;
     oppose : bool ;
     translation : int * int 
}