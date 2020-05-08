(* 

#use"Hex_analysis/hex_first_alternative_in_springboard_t.ml";;

The last two arguments describe the (molecular) solution .

*)

type t = Fa of   
   Hex_cell_t.t * Hex_kite_springless_element_t.t list * 
   Hex_molecular_linker_t.t * Hex_cell_set_t.t 
  ;; 