(* 

#use"Hex_analysis/hex_impose_active_cell.ml";;

*)



module Private = struct 

let impose_cell_by_casing_in_contact_case dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)=  
    let new_islands = Hex_island.add_cell_by_casing dim new_cell (last_island::old_islands) 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
    let remade_last_stop = Hex_kite_element_t.Earth(List.hd new_islands) in 
   {
      pk with
      Hex_partial_kite_t.stops_so_far = (remade_last_stop::previous_stops);
      unvisited_islands = List.tl new_islands;
      added_by_casing = new_abc;
   };;

let impose_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc)=  
    let new_islands = Hex_island.add_cell_by_casing dim new_cell old_islands 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
   {
      pk with
      Hex_partial_kite_t.unvisited_islands = new_islands;
      added_by_casing = new_abc;
   };;


let impose_cell_by_casing_without_normalizing dim new_cell pk=  
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_abc = pk.Hex_partial_kite_t.added_by_casing  in 
    let rl=pk.Hex_partial_kite_t.stops_so_far in 
    let (last_stop,previous_stops) = Listennou.ht rl in 
    let last_island = Hex_kite_element.claim_island last_stop in 
    if Hex_island.test_for_neighbor dim last_island new_cell 
    then impose_cell_by_casing_in_contact_case    dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)
    else impose_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc) ;;

(*
let normalize_after_imposing_a_cell pk =
    let old_stops = pk.Hex_partial_kite_t.stops_so_far in  
    match List.hd old_stops with 
    Hex_kite_element_t.Earth(island) ->
          if Hex_island.outer_earth island = None then pk else
          let old_islands = pk.Hex_partial_kite_t.unvisited_islands in 
          {
             pk with 
             Hex_partial_kite_t.stops_so_far = List.tl old_stops ;
             unvisited_islands = island :: old_islands ;
          }
    | _ -> pk ;;
*)

end ;;

let impose_cell_by_casing dim new_cell pk=  
   (* Private.normalize_after_imposing_a_cell *)
     (Private.impose_cell_by_casing_without_normalizing dim new_cell pk);;



