(* 

#use"Hex_analysis/hex_kite_element.ml";;

*)

exception Extract_island_exn of Hex_kite_element_t.t ;;


module Private = struct 


let to_readable_string = function
  Hex_kite_element_t.Earth(island)-> Hex_island.to_readable_string island
   |Sea(nc)-> Hex_named_connector.to_readable_string nc
   |Springboard(springboard)-> Hex_springboard.to_readable_string springboard;;

let opt_island_component elt = match elt with 
    Hex_kite_element_t.Earth(island)-> Some island
   |Sea(nc)-> None
   |Springboard(springboard)-> Some (Hex_springboard.new_island springboard) ;;
  

end ;;

let compress_to_springless elt= match elt with 
    Hex_kite_element_t.Earth(island)-> Hex_kite_springless_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_springless_element_t.Sea(nc)
   |Springboard(springboard)->Hex_kite_springless_element_t.Earth(Hex_springboard.new_island springboard);;


let extract_island elt= match Private.opt_island_component elt with 
    Some island -> island 
    |None -> raise(Extract_island_exn(elt)) ;;


let opt_island_component = Private.opt_island_component ;;

let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     


let to_molecular_linker = function
  Hex_kite_element_t.Earth(island)-> None
   |Sea(nc)-> Some(Hex_named_connector.to_molecular_linker nc)
   |Springboard(sb) -> Some(Hex_springboard.to_molecular_linker sb)  ;;
