(* 

#use"Hex_analysis/hex_partial_kite_field.ml";;

*)



module Private = struct 

let original_side pk =
   Hex_anchor.unique_side (Hex_island.anchor pk.Hex_partial_kite_t.place_of_birth);;

let opt_final_death pk=
   let opt_last_island=(match pk.Hex_partial_kite_t.steps_so_far  with 
    [] -> Some pk.Hex_partial_kite_t.place_of_birth 
   |last_elt::_ -> Hex_kite_element.opt_island_component last_elt
   ) in 
     match opt_last_island with 
     None -> None
     |Some(island) ->  
     if Hex_island.anchor island <> Hex_anchor_t.No_anchor
     then Some island 
     else None ;;

let compute_place_of_death pk=
   match opt_final_death pk with 
   Some(death_already_occurred)-> death_already_occurred 
   |None ->
   let final_side = Hex_cardinal_direction.oppose(original_side pk) in 
   Hex_island.get_side final_side pk.Hex_partial_kite_t.unvisited_islands ;;      


let test_for_finality pk = 
   if Hex_anchor.is_two_edged 
      (Hex_island.anchor(pk.Hex_partial_kite_t.place_of_birth)) 
   then true 
   else 
   let steps = pk.Hex_partial_kite_t.steps_so_far in 
   if List.exists Hex_kite_element.is_two_edged steps 
   then true
   else 
   match steps with 
   [] -> false 
   |last_elt::_->
   match  last_elt with 
   Hex_kite_element_t.Sea(nc) -> 
      let place_of_death = compute_place_of_death pk in 
      Hex_named_connector.check_exit nc place_of_death 
   |_ -> let island = Hex_kite_element.extract_island last_elt in  
         Hex_island.anchor island <> Hex_anchor_t.No_anchor  ;;

let place_of_death pk=
   match opt_final_death pk with 
   Some(death_already_occurred)-> death_already_occurred 
   |None ->
   let final_side = Hex_cardinal_direction.oppose(original_side pk) in 
   Hex_island.get_side final_side pk.Hex_partial_kite_t.unvisited_islands ;;      

 

end ;; 

let place_of_death = Private.place_of_death ;;
let test_for_finality = Private.test_for_finality ;;