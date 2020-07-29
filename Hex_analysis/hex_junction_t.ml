(* 

#use"Hex_analysis/hex_junction_t.ml";;

*)

type t= 
    Paired of  (Hex_cell_set_t.t * Hex_cell_set_t.t) list 
  | Eyed_claw of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t *
          Hex_cell_t.t ;; 


