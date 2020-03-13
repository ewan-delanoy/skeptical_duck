(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)



module Private = struct 




let add_cell_by_casing_in_contact_case dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)=  
    let new_islands = Hex_island.add_cell_by_casing dim new_cell (last_island::old_islands) 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
    let remade_last_stop = Hex_kite_element_t.Earth(List.hd new_islands) in 
   {
      pk with
      Hex_partial_kite_t.stops_so_far = remade_last_stop :: previous_stops;
      unvisited_islands = List.tl new_islands;
      added_by_casing = new_abc;
   };;

let add_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc)=  
    let new_islands = Hex_island.add_cell_by_casing dim new_cell old_islands 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
   {
      pk with
      Hex_partial_kite_t.unvisited_islands = new_islands;
      added_by_casing = new_abc;
   };;


let add_cell_by_casing dim new_cell pk=  
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_abc = pk.Hex_partial_kite_t.added_by_casing  in 
    let old_stops = pk.Hex_partial_kite_t.stops_so_far in 
    let (last_stop,previous_stops) = Listennou.ht old_stops in 
    let last_island = Hex_kite_element.claim_island last_stop in 
    if Hex_island.test_for_neighbor dim last_island new_cell 
    then add_cell_by_casing_in_contact_case    dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)
    else add_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc) ;;

let explore eob pk (cell,nc) = 
      let nbr_of_common_steps = List.length(pk.Hex_partial_kite_t.stops_so_far) in 
      let pk1 = add_cell_by_casing eob.Hex_end_of_battle_t.dimension cell pk in 
      let pk2 = snd(Hex_springless_analysis.extend_with_sea pk1 nc) in 
      let temp = Hex_springless_analysis.finalize eob pk2 in 
      Image.image (fun (stops,mlclr)->
        let ttemp2 = Listennou.big_tail nbr_of_common_steps stops in 
        (Image.image Hex_kite_element.to_springless ttemp2,mlclr)
      ) temp ;;


let extend_with_springboard dim pk new_sb =
    let (Hex_springboard_t.Sp(cell,path,solution,cell2,nc2)) = new_sb in 
    let pk2 = add_cell_by_casing dim cell2 pk in  
    let old_islands = pk2.Hex_partial_kite_t.unvisited_islands 
    and old_seas = pk2.Hex_partial_kite_t.unvisited_seas 
    and old_stops = pk2.Hex_partial_kite_t.stops_so_far in 
    let restricted_islands = List.filter (Hex_springboard.check_island new_sb) old_islands 
    and restricted_seas =  List.filter (fun (_,sea)->Hex_springboard.check_sea new_sb sea) old_seas  in
    let pk3 ={
      pk2 with 
        Hex_partial_kite_t.stops_so_far = 
           (List.hd old_stops)::(Hex_kite_element_t.Springboard new_sb)::(List.tl old_stops) ;
        unvisited_islands = restricted_islands ;
        unvisited_seas = restricted_seas ;
    } in 
    snd(Hex_springless_analysis.extend_with_sea pk3 nc2);;

let casings_from_islands eob pk = 
    let dim = eob.Hex_end_of_battle_t.dimension 
    and last_island = Hex_kite_element.claim_island(List.hd(pk.Hex_partial_kite_t.stops_so_far)) 
    and other_islands = pk.Hex_partial_kite_t.unvisited_islands in 
    let temp1 = Hex_island.short_connections_to_other_islands dim last_island other_islands in 
    let temp2 = List.filter (fun cell->Hex_end_of_battle.assess eob cell = Hex_eob_result_t.Unoccupied) temp1 in 
    Hex_cell_set.safe_set temp2;; 

let casings_from_seas eob pk =
   let currently_added = pk.Hex_partial_kite_t.added_by_casing 
   and casings_with_hooks = pk.Hex_partial_kite_t.unvisited_seas  in 
   let unordered = Option.filter_and_unpack (
     fun (z,nc) -> 
        let d = Hex_cell_set.setminus z currently_added in 
        if Hex_cell_set.length d = 1 
        then Some(Hex_cell_set.min d)
        else None 
   ) casings_with_hooks in 
   Hex_cell_set.safe_set unordered ;; 

let minimal_casings eob pk =
   Hex_cell_set.forget_order (Hex_cell_set.merge
      (casings_from_islands eob pk)
      (casings_from_seas eob pk)
   ) ;; 


let border_casings eob pk =   
    let dim = eob.Hex_end_of_battle_t.dimension in 
    let old_stops = pk.Hex_partial_kite_t.stops_so_far in 
    let last_stop = List.hd old_stops in 
    let last_island = Hex_kite_element.claim_island last_stop 
    and goal_side = Hex_cardinal_direction.oppose pk.Hex_partial_kite_t.original_side in  
    let temp1 = Hex_island.short_connections_to_border dim  last_island goal_side in
    List.filter (
      fun cell-> Hex_end_of_battle.assess eob cell = Hex_eob_result_t.Unoccupied
    )  temp1 ;;    

let cellset_setminus x y =
   let sx = Hex_cell_set.safe_set x 
   and sy = Hex_cell_set.safe_set y in 
   Hex_cell_set.forget_order(Hex_cell_set.setminus sx sy);;         

let explore_minimal_casings eob pk =
   let dim = eob.Hex_end_of_battle_t.dimension in 
   let brdr_casings = border_casings eob pk in
   let minimal_casings = cellset_setminus ( minimal_casings eob pk) brdr_casings in 
   let minimal_casings_with_hooks = List.flatten (
      Image.image (fun cell->
        let pk1 = add_cell_by_casing eob.Hex_end_of_battle_t.dimension cell pk in 
        let ext1 = Hex_springless_analysis.extensions pk1 in 
        Image.image (fun (elt,new_pk)->
          (cell,Hex_kite_springless_element.claim_named_connector elt)) ext1
      ) minimal_casings
   ) in 
   let first_whole = Image.image (fun p->(p,explore eob pk p)) minimal_casings_with_hooks in 
   let temp1 = List.filter (fun (p,l)->l<>[]) first_whole in 
   let temp2 = Image.image (
     fun cell -> let new_pk = add_cell_by_casing dim cell pk in 
       (cell,[],Hex_springless_analysis.to_molecular_linker new_pk)
   ) brdr_casings
   and temp3 = List.flatten (Image.image (fun ((cell,nc),l)->
      Image.image (fun (path,solution)->(cell,path,solution) ) l
   ) temp1) in 
   (temp2@temp3,Image.image fst first_whole);;

let compute_springboards eob pk =
  let (good_casings,all_casings) = explore_minimal_casings eob pk in 
  let temp1 = Cartesian.product good_casings all_casings in 
  let temp2 = Image.image (
    fun ((cell,path,solution),(cell2,nc2))->
       (cell,path,solution,cell2,nc2)
  ) temp1  in 
  Option.filter_and_unpack Hex_springboard.opt_constructor temp2;;

let springful_extensions eob pk =
   let springboards = compute_springboards eob pk  in 
   ([],Image.image (fun sb->extend_with_springboard 
      eob.Hex_end_of_battle_t.dimension pk sb
   ) springboards) ;;


let extensions_finished_and_non_finished eob pk =
   let first_trial = Hex_springless_analysis.extensions_finished_and_non_finished pk in 
   if first_trial <> ([],[])
   then first_trial
   else springful_extensions eob pk;;


end ;;


let extensions  =  Private.extensions_finished_and_non_finished ;; 
