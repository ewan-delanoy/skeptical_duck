(* 

#use"Hex_analysis/hex_kite_element.ml";;

*)



exception Claim_named_connector_exn of Hex_kite_springless_element_t.t ;;

module Private = struct 


let to_readable_string = function
  Hex_kite_element_t.Earth(island)-> Hex_island.to_readable_string island
   |Sea(nc)-> Hex_named_connector.to_readable_string nc;;

  
let inner_sea = function 
    Hex_kite_element_t.Earth(island)-> Hex_cell_set.empty_set
   |Sea(nc)-> Hex_named_connector.inner_sea nc;;

end ;;

let claim_named_connector_on_springless x = match x with 
    Hex_kite_springless_element_t.Earth(island)-> raise(Claim_named_connector_exn(x))
   |Sea(nc)-> nc ;;


let inner_sea = Private.inner_sea ;;

let is_final initial_side elt = 
   let final_side = Hex_cardinal_direction.oppose initial_side in 
   match elt with  
   Hex_kite_element_t.Sea(_)-> false
   |Earth(island)-> (Hex_island.outer_earth island = Some final_side);;
;;

let join_to_cell_if_possible dim new_cell elt = 
   match elt with  
   Hex_kite_element_t.Sea(_)-> elt
   |Earth(island)-> Earth(Hex_island.join_to_cell_if_possible dim new_cell island);;


let of_springless = function
    Hex_kite_springless_element_t.Earth(island)-> Hex_kite_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_element_t.Sea(nc);;


let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     


let to_molecular_linker = function
  Hex_kite_element_t.Earth(island)-> None
   |Sea(nc)-> Some(Hex_named_connector.to_molecular_linker nc);;

let to_springless = function
    Hex_kite_element_t.Earth(island)-> Hex_kite_springless_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_springless_element_t.Sea(nc);;