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
      Hex_partial_kite_t.stops_so_far = (remade_last_stop::previous_stops);
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


let add_cell_by_casing_without_normalizing dim new_cell pk=  
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_abc = pk.Hex_partial_kite_t.added_by_casing  in 
    let rl=pk.Hex_partial_kite_t.stops_so_far in 
    let (last_stop,previous_stops) = Listennou.ht rl in 
    let last_island = Hex_kite_element.claim_island last_stop in 
    if Hex_island.test_for_neighbor dim last_island new_cell 
    then add_cell_by_casing_in_contact_case    dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)
    else add_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc) ;;

let normalize_after_adding_a_cell pk =
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

let add_cell_by_casing dim new_cell pk=  
   normalize_after_adding_a_cell (add_cell_by_casing_without_normalizing dim new_cell pk);;

let explore_nonfinal eob pk (cell,nc) = 
      let old_stops = pk.Hex_partial_kite_t.stops_so_far in 
      let nbr_of_common_steps = List.length old_stops in 
      let pk1 = add_cell_by_casing eob.Hex_end_of_battle_t.dimension cell pk in 
      let pk2 = snd(Hex_springless_analysis.extend_with_sea pk1 nc) in 
      let temp = Hex_springless_analysis.finalize eob pk2 in 
      Image.image (fun (a1,a2,stops,mlclr,actv)->
        let ttemp2 = Listennou.big_tail nbr_of_common_steps stops in 
        (Image.image Hex_kite_element.to_springless ttemp2,mlclr,actv)
      ) temp ;;

let explore_final eob pk (cell,nc) = 
      let pk1 = add_cell_by_casing eob.Hex_end_of_battle_t.dimension cell pk in 
      let pk2 = Hex_springless_analysis.extend_with_final_sea pk1 nc in 
      let mlclr = Hex_springless_analysis.to_molecular_linker pk2 
      and actv = Hex_springless_analysis.active_part pk2 in 
      [
         [Hex_kite_springless_element_t.Sea(nc)],mlclr,actv
      ];;

let explore eob pk (cell,pfc) = match pfc with 
    Hex_possibly_final_connector_t.Final(nc) -> explore_final eob pk (cell,nc) 
   |Hex_possibly_final_connector_t.Nonfinal(nc) -> explore_nonfinal eob pk (cell,nc) ;;


let extend_with_springboard dim pk new_sb =
    let (Hex_springboard_t.Sp(cell,path,sol1,sol2,cell2,ke)) = new_sb in 
    let pk2 = add_cell_by_casing dim cell2 pk in  
    let old_islands = pk2.Hex_partial_kite_t.unvisited_islands 
    and old_seas = pk2.Hex_partial_kite_t.unvisited_seas 
    and old_enders = pk2.Hex_partial_kite_t.unvisited_enders
    and old_stops = pk2.Hex_partial_kite_t.stops_so_far in 
    let restricted_islands = List.filter (Hex_springboard.check_island new_sb) old_islands 
    and selector  =  List.filter (fun (_,sea)->Hex_springboard.check_sea new_sb sea)   in
    let pk3 ={
      pk2 with 
        Hex_partial_kite_t.stops_so_far = 
           ((List.hd old_stops)::(Hex_kite_element_t.Springboard new_sb)::(List.tl old_stops)) ;
        unvisited_islands = restricted_islands ;
        unvisited_seas =  selector old_seas;
        unvisited_enders =  selector old_enders ;
    } in 
    match ke with 
     Hex_possibly_final_connector_t.Final(final_nc) -> Hex_springless_analysis.extend_with_final_sea pk3 final_nc 
    |Hex_possibly_final_connector_t.Nonfinal(nc) -> snd(Hex_springless_analysis.extend_with_sea pk3 nc) ;;

let casings_from_islands eob pk = 
    let old_stops = pk.Hex_partial_kite_t.stops_so_far in 
    let dim = eob.Hex_end_of_battle_t.dimension 
    and last_island = Hex_kite_element.claim_island(List.hd old_stops) 
    and other_islands = pk.Hex_partial_kite_t.unvisited_islands in 
    let temp1 = Hex_island.short_connections_to_other_islands dim last_island other_islands in 
    let temp2 = List.filter (fun cell->Hex_end_of_battle.assess eob cell = Hex_eob_result_t.Unoccupied) temp1 in 
    Hex_cell_set.safe_set temp2;; 

let casings_from_seas eob pk =
   let currently_added = pk.Hex_partial_kite_t.added_by_casing 
   and middle_casings_with_hooks = pk.Hex_partial_kite_t.unvisited_seas 
   and end_casings_with_hooks = pk.Hex_partial_kite_t.unvisited_enders       in 
   let selector = (fun l->Hex_cell_set.safe_set(Option.filter_and_unpack (
     fun (z,nc) -> 
        let d = Hex_cell_set.setminus z currently_added in 
        if Hex_cell_set.length d = 1 
        then Some(Hex_cell_set.min d)
        else None 
   )l) ) in 
   Hex_cell_set.merge
   (selector middle_casings_with_hooks)
   (selector end_casings_with_hooks) ;; 


let border_casings eob pk =   
    let dim = eob.Hex_end_of_battle_t.dimension in 
    let last_stop = (match pk.Hex_partial_kite_t.stops_so_far with 
    [] -> pk.Hex_partial_kite_t.first_step 
    |last_elt :: _-> last_elt
     ) in 
    let last_island = Hex_kite_element.claim_island last_stop 
    and goal_side = Hex_cardinal_direction.oppose (Hex_springless_analysis.original_side pk) in  
    let temp1 = Hex_island.short_connections_to_border dim  last_island goal_side in
    List.filter (
      fun cell-> Hex_end_of_battle.assess eob cell = Hex_eob_result_t.Unoccupied
    )  temp1 ;;    

let minimal_casings eob pk =
   Hex_cell_set.forget_order (Hex_cell_set.merge
      (casings_from_islands eob pk)
      (casings_from_seas eob pk)
   ) ;; 

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
        let (ext1,ext2) = Hex_springless_analysis.extensions dim eob pk1 in 
        let part1 = Image.image (fun (elt,new_pk)->
          (cell,Hex_possibly_final_connector_t.Final(Hex_kite_springless_element.claim_named_connector elt))) ext1 
        and part2 = Image.image (fun (elt,new_pk)->
          (cell,Hex_possibly_final_connector_t.Nonfinal(Hex_kite_springless_element.claim_named_connector elt))) ext2 in   
        part1@part2
      ) minimal_casings
   ) in 
   let first_whole = Image.image (fun p->(p,explore eob pk p)) minimal_casings_with_hooks in 
   let temp1 = List.filter (fun (p,l)->l<>[]) first_whole in 
   let short_to_border = Image.image (
     fun cell -> let new_pk = add_cell_by_casing dim cell pk in 
       (cell,[],Hex_springless_analysis.to_molecular_linker new_pk,Hex_springless_analysis.active_part new_pk)
   ) brdr_casings
   and temp3 = List.flatten (Image.image (fun ((cell,nc),l)->
      Image.image (fun (path,sol1,sol2)->(cell,path,sol1,sol2) ) l
   ) temp1) in 
   (short_to_border@temp3,Image.image fst first_whole);;

let compute_springboards eob pk =
  let (good_casings,all_casings) = explore_minimal_casings eob pk in 
  let temp1 = Cartesian.product good_casings all_casings in 
  let temp2 = Image.image (
    fun ((cell,path,sol1,sol2),(cell2,ke))->
       (cell,path,sol1,sol2,cell2,ke)
  ) temp1  in 
  Option.filter_and_unpack Hex_springboard.opt_constructor temp2;;

let springful_extensions eob pk =
   let springboards = compute_springboards eob pk  in 
   let temp1 = Image.image (fun sb->(Hex_springboard.is_final sb,extend_with_springboard 
      eob.Hex_end_of_battle_t.dimension pk sb)
   ) springboards in 
   let (full_sols,partial_sols) = List.partition fst temp1 in 
   let detailed_sols = Image.image 
     (fun (_,pk)->Hex_springless_analysis.solution_details pk) full_sols in 
   (detailed_sols,Image.image snd partial_sols) ;;


let extensions_finished_and_non_finished eob pk =
   let dim = eob.Hex_end_of_battle_t.dimension in 
   let first_trial = Hex_springless_analysis.extensions_finished_and_non_finished dim eob pk in 
   if first_trial <> ([],[])
   then first_trial
   else springful_extensions eob pk;;


end ;;


let extensions  =  Private.extensions_finished_and_non_finished ;; 
