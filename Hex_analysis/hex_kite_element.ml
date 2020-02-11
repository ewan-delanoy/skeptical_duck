(* 

#use"Hex_analysis/hex_kite_element.ml";;

*)




module Private = struct 


let to_readable_string = function
  Hex_kite_element_t.Earth(island)-> Hex_island.to_readable_string island
   |Sea(nc)-> Hex_named_connector.to_readable_string nc;;


let test_for_successor_for_earth island = function 
  Hex_kite_element_t.Earth(_)-> false
   |Sea(nc)-> Hex_connector.check_entry island (Hex_named_connector.forget_name nc);;

let test_for_successor_for_sea nc = function 
  Hex_kite_element_t.Sea(_)-> false
   |Earth(island)-> Hex_connector.check_exit island (Hex_named_connector.forget_name nc);;

let test_for_successor = function 
  Hex_kite_element_t.Earth(island)-> test_for_successor_for_earth island
   |Sea(nc)-> test_for_successor_for_sea nc;;
  
let inner_sea = function 
    Hex_kite_element_t.Earth(island)-> Hex_cell_set.empty_set
   |Sea(nc)-> Hex_named_connector.inner_sea nc;;

end ;;

let check_compatiblity end_of_battle elt = 
   let (ordered_inner_data,expected_result) = (match elt with 
   Hex_kite_element_t.Earth(island)-> (Hex_island.inner_earth island,Hex_eob_result_t.Ally_territory)
   |Sea(nc)-> (Hex_named_connector.inner_sea nc,Hex_eob_result_t.Unoccupied)
   ) in 
   let inner_data = Hex_cell_set.forget_order ordered_inner_data in 
   List.for_all (fun cell -> 
    (Hex_end_of_battle.assess end_of_battle cell)=expected_result ) inner_data ;;

let check_disjointness to_be_avoided elt =
   Hex_cell_set.does_not_intersect to_be_avoided (Private.inner_sea elt);;

let inner_sea = Private.inner_sea ;;

let is_final initial_side elt = 
   let final_side = Hex_cardinal_direction.oppose initial_side in 
   match elt with  
   Hex_kite_element_t.Sea(_)-> false
   |Earth(island)-> (Hex_island.outer_earth island = Some final_side);;
;;

let print_out (fmt:Format.formatter) elt=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string elt);;     

let test_for_successor = Private.test_for_successor ;;

let to_molecular_linker = function
  Hex_kite_element_t.Earth(island)-> None
   |Sea(nc)-> Some(Hex_named_connector.to_molecular_linker nc);;

