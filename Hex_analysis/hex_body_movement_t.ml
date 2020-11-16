(* 

#use"Hex_analysis/hex_body_movement_t.ml";;

Transformation to send a Hex_connector_t.t object to another Hex_connector_t.t object

*)


type t= {
     dimension : Hex_dimension_t.t ;
     reflect : bool ;
     oppose : bool ;
     translation : int * int 
} ;; 