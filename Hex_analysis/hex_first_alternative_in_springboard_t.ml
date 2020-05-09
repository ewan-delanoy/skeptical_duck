(* 

#use"Hex_analysis/hex_first_alternative_in_springboard_t.ml";;

When the springboard does not start from the last visited island, the first argument is not None 
and describes the starting island.
The last two arguments describe the (molecular) solution .

*)

type t = Fa of   
   Hex_island_t.t * Hex_cell_t.t * Hex_kite_springless_element_t.t list * 
   Hex_molecular_linker_t.t * Hex_cell_set_t.t 
  ;; 
