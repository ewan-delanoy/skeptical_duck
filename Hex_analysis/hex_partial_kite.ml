(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)



module Private = struct 


let last_stop pk = match pk.Hex_partial_kite_t.stops_so_far with 
    [] ->pk.Hex_partial_kite_t.first_step 
   |elt::_ -> elt ;;




let extend_with_springboard dim pk new_sb =
    let (Hex_springboard_t.Sp(cell,path,sol1,sol2,cell2,new_island)) = new_sb in  
    let pk2 = Hex_impose_active_cell.impose_cell_by_casing dim cell2 pk in 
    let old_islands = pk2.Hex_partial_kite_t.unvisited_islands 
    and old_seas = pk2.Hex_partial_kite_t.unvisited_seas 
    and old_enders = pk2.Hex_partial_kite_t.unvisited_enders
    and old_stops = pk2.Hex_partial_kite_t.stops_so_far in 
    let restricted_islands = List.filter (Hex_springboard.check_island_after_springboard_insertion new_sb) old_islands 
    and selector  =  List.filter (fun (_,sea)->Hex_springboard.check_sea new_sb sea)   in
    {
      pk2 with 
        Hex_partial_kite_t.stops_so_far = 
           ((Hex_kite_element_t.Springboard new_sb)::old_stops) ;
        unvisited_islands = restricted_islands ;
        unvisited_seas =  selector old_seas;
        unvisited_enders =  selector old_enders ;
    }  ;;


let casings_from_one_step_advances eob pk = 
    let old_stops = pk.Hex_partial_kite_t.stops_so_far in 
    let dim = eob.Hex_end_of_battle_t.dimension 
    and last_island = Hex_kite_element.claim_island(List.hd old_stops)  in 
    let temp1 = Hex_island.neighbors dim last_island  in
    let temp2 = Set_of_poly_pairs.image Hex_cell.of_int_pair temp1 in  
    let temp3 = List.filter (fun cell->Hex_end_of_battle.assess eob cell = Hex_eob_result_t.Unoccupied) temp2 in 
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

let all_casings eob pk =
   Hex_cell_set.forget_order (Hex_cell_set.merge
      (casings_from_one_step_advances eob pk)
      (casings_from_seas eob pk)
   ) ;; 

let data_common_to_both_parts eob pk =
   (* both means : both for the light & heavy part. See definition of compute_springboards *)
   let dim = eob.Hex_end_of_battle_t.dimension in 
   let temp1 = all_casings eob pk in 
    Image.image (
      fun cell -> 
        let new_pk=Hex_impose_active_cell.impose_cell_by_casing dim cell pk in 
        (cell,new_pk) 
   ) temp1 ;;

let light_part common_to_both = 
   Image.image (fun (cell,new_pk)->
      (cell,Hex_kite_element.extract_island (last_stop new_pk))
   ) common_to_both;;

let explore_yet_untried_path eob old_pk (cell,new_pk) =
   let nbr_of_common_steps = List.length old_pk.Hex_partial_kite_t.stops_so_far in 
   let temp = Hex_springless_analysis.finalize eob new_pk in 
   Image.image (fun (_,_,stops,mlclr,actv)->
        let ttemp2 = Listennou.big_tail nbr_of_common_steps stops in 
        (cell,Image.image Hex_kite_element.to_springless ttemp2,mlclr,actv)
   ) temp ;;

let explore_yet_untried_paths eob old_pk paths =
   List.flatten(Image.image (explore_yet_untried_path eob old_pk) paths);;

let heavy_part eob old_pk common_to_both =
   let (final_ones,nonfinal_ones) = List.partition (
       fun (cell,new_pk) -> Hex_springless_analysis.is_final new_pk 
   ) common_to_both in 
   let one_move_solutions= Image.image (fun (cell,new_pk)->
      let mlclr = Hex_springless_analysis.to_molecular_linker new_pk 
      and actv = Hex_springless_analysis.active_part new_pk in 
      (cell,[],mlclr,actv)) final_ones in 
   one_move_solutions@(explore_yet_untried_paths eob old_pk nonfinal_ones)  ;;

let cellset_setminus x y =
   let sx = Hex_cell_set.safe_set x 
   and sy = Hex_cell_set.safe_set y in 
   Hex_cell_set.forget_order(Hex_cell_set.setminus sx sy);;         



let compute_springboards eob pk =
  let common_to_both =  data_common_to_both_parts eob pk in 
  let heavy = heavy_part eob pk common_to_both 
  and light = light_part        common_to_both in 
  let temp1 = Cartesian.product heavy light in 
  let temp2 = Image.image (
    fun ((cell,path,sol1,sol2),(cell2,new_island))->
       (cell,path,sol1,sol2,cell2,new_island)
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
