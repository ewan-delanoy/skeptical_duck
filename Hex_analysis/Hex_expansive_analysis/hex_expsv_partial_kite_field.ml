(* 

#use"Hex_analysis/hex_partial_kite_field.ml";;

*)



module Private = struct 

let original_side pk =
   Hex_anchor.unique_side (Hex_island.anchor pk.Hex_partial_kite_t.place_of_birth);;
   
let is_two_edged elt = match Hex_expsv_kite_element.opt_island_component elt with 
   None -> false 
   |Some island -> Hex_island.is_two_edged island;;

let last_island pk =
   match Option.find_and_stop Hex_expsv_kite_element.opt_island_component pk.Hex_partial_kite_t.steps_so_far with 
   Some island -> island 
   | None -> pk.Hex_partial_kite_t.place_of_birth ;;

let opt_final_death pk=
   let birth =  pk.Hex_partial_kite_t.place_of_birth in 
   if Hex_island.is_two_edged birth then Some birth else 
   match Option.seek is_two_edged pk.Hex_partial_kite_t.steps_so_far with 
   Some(elt) -> Some (Hex_expsv_kite_element.extract_island elt) 
   |None ->
   let late_island = last_island pk 
   and death_side = Hex_cardinal_direction.oppose (original_side pk) in 
   if Hex_island.touches_side late_island death_side
   then Some late_island
   else None;;  


let place_of_death pk=
   match opt_final_death pk with 
   Some(death_already_occurred)-> death_already_occurred 
   |None ->
   let death_side = Hex_cardinal_direction.oppose(original_side pk) in 
   Hex_island.get_side death_side pk.Hex_partial_kite_t.unvisited_islands ;;   

let test_for_finality pk = 
   (* "Almost finished" ones just one island away from the end are also counted as finite *) 
   if (opt_final_death pk)<>None 
   then true 
   else 
   match pk.Hex_partial_kite_t.steps_so_far with 
   [] -> false 
   |last_elt::_->
   match  last_elt with 
   Hex_kite_element_t.Sea(nc) -> 
      let place_of_death = place_of_death pk in 
      Hex_named_connector.check_exit nc place_of_death 
   |_ -> (* A death would already have been detected in opt_final_death *)
         false  ;;


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


let last_stop pk = 
   match pk.Hex_partial_kite_t.steps_so_far with 
    [] -> Hex_kite_element_t.Earth pk.Hex_partial_kite_t.place_of_birth
   |elt::_ -> elt ;;

let constructor  first_island islands seas free_ones =
        {
            Hex_partial_kite_t.place_of_birth = first_island;
            steps_so_far =  [];
            unvisited_islands = List.filter (fun x->x<>first_island ) islands;
            unvisited_seas = seas ;
            added_by_casing = Hex_cell_set.empty_set;
            investment = None ;
            remaining_free_cells = free_ones;
        } ;;

end ;; 

let constructor = Private.constructor ;; 
let extend_with_island = Private.extend_with_island ;;
let extend_with_sea = Private.extend_with_sea ;;
let last_island = Private.last_island ;;
let last_stop = Private.last_stop ;;
let place_of_death = Private.place_of_death ;;
let test_for_finality = Private.test_for_finality ;;
let winner = Private.winner;;