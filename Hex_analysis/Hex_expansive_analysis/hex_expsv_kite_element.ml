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
   |Sea(nc)-> Hex_expsv_named_connector.to_readable_string nc
   |Springboard(springboard)-> Hex_expsv_springboard.to_readable_string springboard;;

let opt_island_component elt = match elt with 
    Hex_kite_element_t.Earth(island)-> Some island
   |Sea(nc)-> None
   |Springboard(springboard)-> Some (Hex_expsv_springboard.new_island springboard) ;;
  

end ;;

let change_island_component elt new_component= match elt with 
    Hex_kite_element_t.Earth(island)-> Hex_kite_element_t.Earth(new_component)
   |Sea(nc)-> elt
   |Springboard(springboard)->
      Hex_kite_element_t.Springboard(Hex_expsv_springboard.change_island_component springboard new_component);;


let compress_to_springless elt= match elt with 
    Hex_kite_element_t.Earth(island)-> Hex_kite_springless_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_springless_element_t.Sea(nc)
   |Springboard(springboard)->Hex_kite_springless_element_t.Earth(Hex_expsv_springboard.new_island springboard);;

let claim_island x = match x with 
    Hex_kite_element_t.Earth(island)-> island
   |_-> raise(Claim_island_exn(x)) ;;

let claim_sea x = match x with 
    Hex_kite_element_t.Sea(nc)-> nc
   |_-> raise(Claim_sea_exn(x)) ;;

let extract_island elt= match Private.opt_island_component elt with 
    Some island -> island 
    |None -> raise(Extract_island_exn(elt)) ;;

let is_final initial_side elt = 
   let final_side = Hex_cardinal_direction.oppose initial_side in 
   match Private.opt_island_component elt with 
   None -> false 
   |Some(the_island)-> 
   (Hex_anchor.touches_side (Hex_island.anchor the_island) final_side);;
;;


let is_two_edged elt = match Private.opt_island_component elt with 
    None -> false
    |Some island -> Hex_anchor.is_two_edged (Hex_island.anchor island ) ;; 


let of_springless = function
    Hex_kite_springless_element_t.Earth(island)-> Hex_kite_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_element_t.Sea(nc);;

let opt_island_component = Private.opt_island_component ;;

let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     


let to_molecular_linker = function
  Hex_kite_element_t.Earth(island)-> None
   |Sea(nc)-> Some(Hex_expsv_named_connector.to_molecular_linker nc)
   |Springboard(sb) -> Some(Hex_expsv_springboard.to_molecular_linker sb)  ;;

let to_springless elt= match elt with 
    Hex_kite_element_t.Earth(island)-> Hex_kite_springless_element_t.Earth(island)
   |Sea(nc)-> Hex_kite_springless_element_t.Sea(nc)
   |_->raise(To_springless_exn elt);;

 