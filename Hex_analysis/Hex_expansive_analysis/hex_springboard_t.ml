(* 

#use"Hex_analysis/hex_springboard_t.ml";;


The first argument represents a winning casing.
The last two arguments represent a (yet untried) casing. 

*)

type t = Sp of   
   Hex_first_alternative_in_springboard_t. t *  
   Hex_cell_t.t * Hex_island_t.t 
  ;; 
  