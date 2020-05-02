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

let extend_with_island pk new_island = 
        let vague_new_elt = Hex_kite_element_t.Earth(new_island)
        and new_elt = Hex_kite_springless_element_t.Earth(new_island) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.steps_so_far = 
               (vague_new_elt::pk.Hex_partial_kite_t.steps_so_far);
          unvisited_islands = List.filter (fun x->x<>new_island ) 
             (pk.Hex_partial_kite_t.unvisited_islands);
    });;
    

let extend_with_sea pk new_nc = 
        let vague_new_elt = Hex_kite_element_t.Sea(new_nc) 
        and new_elt = Hex_kite_springless_element_t.Sea(new_nc) in 
        let old_steps=pk.Hex_partial_kite_t.steps_so_far in 
        let old_seas = pk.Hex_partial_kite_t.unvisited_seas 
        and old_free_ones = pk.Hex_partial_kite_t.remaining_free_cells in 
        let selector =  List.filter 
              (fun (z,nc)->
                Hex_named_connector.check_disjointness new_nc nc) 
        and remaining_free_ones = Hex_cell_set.setminus old_free_ones
         (Hex_named_connector.inner_sea new_nc) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.steps_so_far = vague_new_elt::old_steps ;
            unvisited_seas = selector old_seas ;
            remaining_free_cells = remaining_free_ones ;
    });;

let winner pk =
   let place_of_birth = pk.Hex_partial_kite_t.place_of_birth in 
   let birth = Hex_anchor.unique_side (Hex_island.anchor place_of_birth) in 
   Hex_cardinal_direction.player_for_side birth ;; 

end ;; 

let extend_with_island = Private.extend_with_island ;;
let extend_with_sea = Private.extend_with_sea ;;
let place_of_death = Private.place_of_death ;;
let test_for_finality = Private.test_for_finality ;;
let winner = Private.winner;;