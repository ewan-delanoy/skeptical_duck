(* 

#use"Hex_analysis/hex_springboard_t.ml";;


The first three arguments represent a winning casing with solution explicited 
in the two last arguments of those three.
The last two argument represent a (yet untried) casing. 

*)

type t = Sp of   
   Hex_cell_t.t * Hex_kite_springless_element_t.t list * Hex_molecular_linker_t.t *
   Hex_cell_t.t * Hex_named_connector_t.t
  ;; 