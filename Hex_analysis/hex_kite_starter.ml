(* 

#use"Hex_analysis/hex_kite_starter.ml";;

*)

let claim_sea (Hex_kite_starter_t.St nc) = nc ;; 

let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Hex_named_connector.to_readable_string elt);;     


let sea nc = Hex_kite_starter_t.St(nc);;

let to_molecular_linker (Hex_kite_starter_t.St nc) 
   = Hex_named_connector.to_molecular_linker nc ;; 


let to_springless (Hex_kite_starter_t.St nc) 
   = Hex_kite_springless_element_t.Sea(nc) ;; 



