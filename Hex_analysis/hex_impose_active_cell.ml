(* 

#use"Hex_analysis/hex_impose_active_cell.ml";;

*)



module Private = struct 

let enhance dim pk cell =
    let birth = Option.unpack(Hex_island.outer_earth(pk.Hex_partial_kite_t.place_of_birth)) in 
    let death = Hex_cardinal_direction.oppose birth in 
    Hex_cardinal_direction.enhance dim death cell ;; 

let impose_cell_by_casing_in_contact_case dim new_cell pk (old_islands,old_abc) (old_last_island,previous_stops)=  
    let sided_cell =  enhance dim pk new_cell in 
    let (remade_last_island,new_islands) = Hex_island.add_sided_cell_by_casing dim sided_cell (old_last_island::old_islands) 
    and new_abc = Hex_cell_set.insert (snd sided_cell) old_abc in 
    let remade_last_stop = Hex_kite_element_t.Earth(remade_last_island) in 
   {
      pk with
      Hex_partial_kite_t.stops_so_far = (remade_last_stop::previous_stops);
      unvisited_islands = List.tl new_islands;
      added_by_casing = new_abc;
   };;

let impose_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc)=  
    let sided_cell =  enhance dim pk new_cell in  
    let (current_island,new_islands) = Hex_island.add_sided_cell_by_casing dim sided_cell old_islands 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
   {
      pk with
      Hex_partial_kite_t.unvisited_islands = new_islands@[current_island];
      added_by_casing = new_abc;
   };;


let impose_cell_by_casing dim new_cell pk=  
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_abc = pk.Hex_partial_kite_t.added_by_casing  in 
    let rl=pk.Hex_partial_kite_t.stops_so_far in 
    let (last_stop,previous_stops) = Listennou.ht rl in 
    let last_island = Hex_kite_element.extract_island last_stop in 
    if Hex_island.test_for_neighbor dim last_island new_cell 
    then impose_cell_by_casing_in_contact_case    dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)
    else impose_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc) ;;


end ;;

let impose_cell_by_casing dim new_cell pk=  
     (Private.impose_cell_by_casing dim new_cell pk);;



