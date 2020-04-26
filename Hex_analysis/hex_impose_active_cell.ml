(* 

#use"Hex_analysis/hex_impose_active_cell.ml";;

*)



module Private = struct 

let enhance dim pk cell =
    let birth = Hex_anchor.unique_side(Hex_island.anchor(pk.Hex_partial_kite_t.place_of_birth)) in 
    let death = Hex_cardinal_direction.oppose birth in 
    Hex_cardinal_direction.enhance dim death cell ;; 


let impose_cell_by_casing_in_contact_case dim new_cell pk (old_islands,old_abc) (old_last_island,old_last_stop,previous_stops)=  
    let sided_cell =  enhance dim pk new_cell 
    and old_free_cells = pk.Hex_partial_kite_t.remaining_free_cells in 
    let (remade_last_island,new_islands) = Hex_island.add_sided_cell_by_casing dim sided_cell (old_last_island::old_islands) 
    and new_abc = Hex_cell_set.insert (snd sided_cell) old_abc in 
    let remade_last_stop = Hex_kite_element.change_island_component old_last_stop remade_last_island in 
   {
      pk with
      Hex_partial_kite_t.stops_so_far = (remade_last_stop::previous_stops);
      unvisited_islands = new_islands;
      added_by_casing = new_abc;
      remaining_free_cells = Hex_cell_set.outsert new_cell old_free_cells;
   };;

let impose_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc)=  
    let sided_cell =  enhance dim pk new_cell 
    and old_free_cells = pk.Hex_partial_kite_t.remaining_free_cells in 
    let (current_island,new_islands) = Hex_island.add_sided_cell_by_casing dim sided_cell old_islands 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
   {
      pk with
      Hex_partial_kite_t.unvisited_islands = new_islands@[current_island];
      added_by_casing = new_abc;
      remaining_free_cells = Hex_cell_set.outsert new_cell old_free_cells;
   };;


let impose_cell_by_casing dim new_cell pk=  
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_abc = pk.Hex_partial_kite_t.added_by_casing  in 
    let rl=pk.Hex_partial_kite_t.stops_so_far in 
    let (last_stop,previous_stops) = Listennou.ht rl in 
    let last_island = Hex_kite_element.extract_island last_stop in 
    if Hex_island.test_for_neighbor dim last_island new_cell 
    then impose_cell_by_casing_in_contact_case    dim new_cell pk (old_islands,old_abc) (last_island,last_stop,previous_stops)
    else impose_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc) ;;



end ;;

let impose_cell_by_casing dim new_cell pk=  
     (Private.impose_cell_by_casing dim new_cell pk);;



