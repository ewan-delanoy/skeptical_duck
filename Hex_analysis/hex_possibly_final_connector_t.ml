(* 

#use"Hex_analysis/hex_possibly_final_kite_element_t.ml";;


*)

type t = 
    Final of Hex_named_connector_t.t   
   |Nonfinal of Hex_named_connector_t.t   
  ;; 