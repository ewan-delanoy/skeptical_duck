(* 

#use"Hex_analysis/hex_kite_element.ml";;

*)



exception Claim_island_exn of Hex_kite_element_t.t ;;
exception Claim_sea_exn of Hex_kite_element_t.t ;;
exception Extract_island_exn of Hex_kite_element_t.t ;;
exception To_springless_exn of Hex_kite_element_t.t ;;

module Private = struct 


let to_readable_string = function
  Hex_kite_element_t.Earth(island)-> Hex_island.to_readable_string island
   |Sea(nc)-> Hex_named_connector.to_readable_string nc
   |Springboard(springboard)-> Hex_springboard.to_readable_string springboard;;

  

end ;;

let compress_to_springless elt= match elt with 
    Hex_kite_element_t.Earth(island)-> Hex_kite_springless_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_springless_element_t.Sea(nc)
   |Springboard(springboard)->Hex_kite_springless_element_t.Earth(Hex_springboard.new_island springboard);;

let claim_island x = match x with 
    Hex_kite_element_t.Earth(island)-> island
   |_-> raise(Claim_island_exn(x)) ;;

let claim_sea x = match x with 
    Hex_kite_element_t.Sea(nc)-> nc
   |_-> raise(Claim_sea_exn(x)) ;;

let extract_island elt= match elt with 
    Hex_kite_element_t.Earth(island)-> island
   |Sea(nc)-> raise(Extract_island_exn(elt))
   |Springboard(springboard)->Hex_springboard.new_island springboard;;


let is_final initial_side elt = 
   let final_side = Hex_cardinal_direction.oppose initial_side in 
   match elt with  
   Hex_kite_element_t.Sea(_) -> false 
   |_-> let the_island = extract_island elt in 
   (Hex_island.outer_earth the_island = Some final_side);;
;;

let of_springless = function
    Hex_kite_springless_element_t.Earth(island)-> Hex_kite_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_element_t.Sea(nc);;

let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     


let to_molecular_linker = function
  Hex_kite_element_t.Earth(island)-> None
   |Sea(nc)-> Some(Hex_named_connector.to_molecular_linker nc)
   |Springboard(sb) -> Some(Hex_springboard.to_molecular_linker sb)  ;;

let to_springless elt= match elt with 
    Hex_kite_element_t.Earth(island)-> Hex_kite_springless_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_springless_element_t.Sea(nc)
   |_->raise(To_springless_exn elt);;

 