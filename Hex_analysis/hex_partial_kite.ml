(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)



module Private = struct 


let last_stop pk = 
   match pk.Hex_partial_kite_t.steps_so_far with 
    [] -> Hex_kite_element_t.Earth pk.Hex_partial_kite_t.place_of_birth
   |elt::_ -> elt ;;




let extend_with_springboard dim pk new_sb =
    let (Hex_springboard_t.Sp(cell,path,sol1,sol2,cell2,new_island)) = new_sb in  
    let pk2 = Hex_impose_active_cell.impose_cell_by_casing dim cell2 pk in 
    let old_islands = pk2.Hex_partial_kite_t.unvisited_islands 
    and old_seas = pk2.Hex_partial_kite_t.unvisited_seas
    and old_steps = pk2.Hex_partial_kite_t.steps_so_far 
    and old_free_ones = pk2.Hex_partial_kite_t.remaining_free_cells
    and requisitionned_territory = Hex_molecular_linker.support(Hex_springboard.to_molecular_linker new_sb) in 
    let restricted_islands = List.filter (Hex_springboard.check_island_after_springboard_insertion new_sb) old_islands 
    and selector  =  List.filter (fun (_,sea)->Hex_springboard.check_sea new_sb sea)   
    and remaining_free_ones = Hex_cell_set.setminus old_free_ones requisitionned_territory in
    {
      pk2 with 
        Hex_partial_kite_t.steps_so_far = 
           (Hex_kite_element_t.Springboard new_sb)::old_steps ;
        unvisited_islands = restricted_islands ;
        unvisited_seas =  selector old_seas;
        remaining_free_cells = remaining_free_ones ;
    }  ;;

let close_future_seas pk =
   let currently_added = pk.Hex_partial_kite_t.added_by_casing 
   and seas = pk.Hex_partial_kite_t.unvisited_seas  in 
   Option.filter_and_unpack (
     fun (z,nc) -> 
        let d = Hex_cell_set.setminus z currently_added in 
        if Hex_cell_set.length d = 1
        then Some(Hex_cell_set.min d,nc)
        else None 
   ) seas ;;   


let casings_from_one_step_advances dim pk cl_seas= 
    let old_steps=pk.Hex_partial_kite_t.steps_so_far  in 
    let last_island = Hex_kite_element.extract_island(List.hd old_steps)  in 
    let close_islands = last_island :: 
         (List.flatten(Image.image (fun (z,nc)->
           [nc.Hex_named_connector_t.entry;nc.Hex_named_connector_t.exit]) (close_future_seas pk))) in
    let temp1 = Set_of_poly_pairs.fold_merge (Image.image (Hex_island.neighbors dim) close_islands)  in
    let temp2 = Hex_cell_set.safe_set(Set_of_poly_pairs.image Hex_cell.of_int_pair temp1) in  
    Hex_cell_set.intersect temp2 pk.Hex_partial_kite_t.remaining_free_cells ;;

let casings_from_seas cl_seas = Hex_cell_set.safe_set (Image.image fst cl_seas) ;;   


let all_casings dim pk =
   let cl_seas = close_future_seas pk in 
   Hex_cell_set.forget_order (Hex_cell_set.fold_merge
      [
       casings_from_one_step_advances dim pk cl_seas;
       casings_from_seas cl_seas
      ] 
   ) ;; 

let data_common_to_both_parts dim pk =
   (* both means : both for the light & heavy part. See definition of compute_springboards *)
   let temp1 = all_casings dim pk in 
    Image.image (
      fun cell -> 
        let new_pk=Hex_impose_active_cell.impose_cell_by_casing dim cell pk in 
        (cell,new_pk) 
   ) temp1 ;;

let light_part common_to_both = 
   Image.image (fun (cell,new_pk)->
      (cell,Hex_kite_element.extract_island (last_stop new_pk))
   ) common_to_both;;

let explore_yet_untried_path dim old_pk (cell,new_pk) =
   let nbr_of_common_steps = List.length (new_pk.Hex_partial_kite_t.steps_so_far)-1 in 
   let temp = Hex_springless_analysis.finalize dim new_pk in 
   Image.image (fun (_,_,stops,mlclr,actv)->
        let ttemp2 = Listennou.big_tail nbr_of_common_steps stops in 
        (cell,Image.image Hex_kite_element.to_springless ttemp2,mlclr,actv)
   ) temp ;;

let explore_yet_untried_paths dim old_pk paths =
   List.flatten(Image.image (explore_yet_untried_path dim old_pk) paths);;

let heavy_part dim old_pk common_to_both =
   let (final_ones,nonfinal_ones) = List.partition (
       fun (cell,new_pk) -> Hex_partial_kite_field.test_for_finality new_pk 
   ) common_to_both in 
   let one_move_solutions= Image.image (fun (cell,new_pk)->
      let mlclr = Hex_finished_kite.to_molecular_linker new_pk 
      and actv = Hex_finished_kite.active_part new_pk in 
      (cell,[],mlclr,actv)) final_ones in 
   one_move_solutions@(explore_yet_untried_paths dim old_pk nonfinal_ones)  ;;

let cellset_setminus x y =
   let sx = Hex_cell_set.safe_set x 
   and sy = Hex_cell_set.safe_set y in 
   Hex_cell_set.forget_order(Hex_cell_set.setminus sx sy);;         


let compute_springboards dim pk =
  let common_to_both =  data_common_to_both_parts dim pk in 
  let heavy = heavy_part dim pk common_to_both 
  and light = light_part        common_to_both in 
  let temp1 = Cartesian.product heavy light in 
  let temp2 = Image.image (
    fun ((cell,path,sol1,sol2),(cell2,new_island))->
       (cell,path,sol1,sol2,cell2,new_island)
  ) temp1  in 
  Option.filter_and_unpack Hex_springboard.opt_constructor temp2;;

let springful_extensions dim pk =
   let springboards = compute_springboards dim pk  in 
   let temp1 = Image.image (fun sb->
     let new_pk = extend_with_springboard dim pk sb in
    (Hex_partial_kite_field.test_for_finality new_pk,new_pk)
   ) springboards in 
   let (full_sols,partial_sols) = List.partition fst temp1 in 
   let detailed_sols = Image.image 
     (fun (_,pk)->Hex_finished_kite.solution_details pk) full_sols in 
   (detailed_sols,Image.image snd partial_sols) ;;


let extensions_finished_and_non_finished dim pk =
   let first_trial = Hex_springless_analysis.extensions_finished_and_non_finished dim pk in 
   if first_trial <> ([],[])
   then first_trial
   else springful_extensions dim pk;;


end ;;


let extensions  =  Private.extensions_finished_and_non_finished ;; 
