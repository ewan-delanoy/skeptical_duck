(* 

#use"Hex_analysis/hex_springboard_t.ml";;


The first four arguments represent a winning casing with solution explicited 
in the two last arguments of those four.
The last two arguments represent a (yet untried) casing. 
If the last argument is set None, it means that just adding
the cell (next-but-last argument) yields an immediate solution 
and does not need any further elements.

*)

type t = Sp of   
   Hex_cell_t.t * Hex_kite_springless_element_t.t list * 
   Hex_molecular_linker_t.t * Hex_cell_set_t.t * 
   Hex_cell_t.t * (Hex_possibly_final_connector_t.t option)
  ;; 