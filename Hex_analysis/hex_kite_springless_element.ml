(* 

#use"Hex_analysis/hex_kite_springless_element.ml";;

*)


exception Claim_island_exn of Hex_kite_springless_element_t.t ;;
exception Claim_named_connector_exn of Hex_kite_springless_element_t.t ;;

module Private = struct 

let to_readable_string = function
  Hex_kite_springless_element_t.Earth(island)-> Hex_island.to_readable_string island
   |Sea(nc)-> Hex_named_connector.to_readable_string nc;;

end ;;

let claim_island x = match x with 
    Hex_kite_springless_element_t.Sea(nc)-> raise(Claim_island_exn(x))
   |Earth(island)-> island ;;

let claim_sea x = match x with 
    Hex_kite_springless_element_t.Earth(island)-> raise(Claim_named_connector_exn(x))
   |Sea(nc)-> nc ;;

let opt_island x = match x with 
    Hex_kite_springless_element_t.Earth(island)-> Some island
   |Sea(_)-> None ;;

let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     

let to_molecular_linker = function
  Hex_kite_springless_element_t.Earth(island)-> None
   |Sea(nc)-> Some(Hex_named_connector.to_molecular_linker nc);;

let to_readable_string = Private.to_readable_string ;;

let wet_earth = function
  Hex_kite_springless_element_t.Earth(island)-> Hex_island.inner_earth island
   |Sea(nc)-> Hex_named_connector.wet_earth nc;;
