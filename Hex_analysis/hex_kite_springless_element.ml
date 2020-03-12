(* 

#use"Hex_analysis/hex_kite_springless_element.ml";;

*)



exception Claim_named_connector_exn of Hex_kite_springless_element_t.t ;;

module Private = struct 


let to_readable_string = function
  Hex_kite_springless_element_t.Earth(island)-> Hex_island.to_readable_string island
   |Sea(nc)-> Hex_named_connector.to_readable_string nc;;

end ;;

let claim_named_connector x = match x with 
    Hex_kite_springless_element_t.Earth(island)-> raise(Claim_named_connector_exn(x))
   |Sea(nc)-> nc ;;


let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     

let wet_earth = function
  Hex_kite_springless_element_t.Earth(island)-> Hex_island.inner_earth island
   |Sea(nc)-> Hex_named_connector.wet_earth nc;;
