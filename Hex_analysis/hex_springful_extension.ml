(* 

#use"Hex_analysis/hex_springful_extension.ml";;

*)



module Private = struct 



let extend_with_first_alternative dim pk new_fa =
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_seas = pk.Hex_partial_kite_t.unvisited_seas
    and old_free_ones = pk.Hex_partial_kite_t.remaining_free_cells
    and requisitioned_territory = Hex_molecular_linker.support(Hex_springboard.to_molecular_linker new_sb) in 
    let restricted_islands = List.filter (Hex_springboard.check_island_after_springboard_insertion new_sb) old_islands 
    and selector  =  List.filter (fun (_,sea)->Hex_springboard.check_sea new_sb sea)   
    and remaining_free_ones = Hex_cell_set.setminus old_free_ones requisitioned_territory in
    {
      pk with 
        Hex_partial_kite_t.unvisited_islands = restricted_islands ;
        unvisited_seas =  selector old_seas;
        remaining_free_cells = remaining_free_ones ; 
        investment = Some new_fa ;
    }  ;;


let extend_with_springboard dim pk new_sb =
    let (Hex_springboard_t.Sp(fa,cell2,new_island)) = new_sb in  
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
        investment = None ;
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
    let last_island = Hex_partial_kite_field.last_island pk  in 
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
      (cell,Hex_kite_element.extract_island (Hex_partial_kite_field.last_stop new_pk))
   ) common_to_both;;

let explore_yet_untried_path dim (cell,new_pk) =
   let nbr_of_common_steps = List.length new_pk.Hex_partial_kite_t.steps_so_far in 
   let temp = Hex_springless_extension.finalize dim new_pk in 
   Image.image (fun (_,fst_stop,other_stops,mlclr,actv)->
        let ttemp2 = Listennou.big_tail nbr_of_common_steps (fst_stop::other_stops) in 
        Hex_first_alternative_in_springboard_t.Fa(None,cell,Image.image Hex_kite_element.to_springless ttemp2,mlclr,actv)
   ) temp ;;

let explore_yet_untried_paths dim paths =
   List.flatten(Image.image (explore_yet_untried_path dim ) paths);;



let heavy_part dim common_to_both =
   let (final_ones,nonfinal_ones) = List.partition (
       fun (cell,new_pk) -> Hex_partial_kite_field.test_for_finality new_pk 
   ) common_to_both in 
   let one_move_solutions= Image.image (fun (cell,new_pk)->
      let mlclr = Hex_finished_kite.to_molecular_linker new_pk 
      and actv = Hex_finished_kite.active_part new_pk in 
      Hex_first_alternative_in_springboard_t.Fa(None,cell,[],mlclr,actv)) final_ones in 
   one_move_solutions@(explore_yet_untried_paths dim nonfinal_ones)  ;;

let cellset_setminus x y =
   let sx = Hex_cell_set.safe_set x 
   and sy = Hex_cell_set.safe_set y in 
   Hex_cell_set.forget_order(Hex_cell_set.setminus sx sy);;         


let compute_springboards dim pk =
  let common_to_both =  data_common_to_both_parts dim pk in 
  let heavy = heavy_part dim common_to_both 
  and light = light_part     common_to_both in 
  let temp1 = Cartesian.product heavy light in 
  Option.filter_and_unpack (
    fun (fa,(cell2,new_island))->
       Hex_springboard.opt_constructor(fa,cell2,new_island)
  ) temp1  ;;


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

let extensions_by_springboard_first_halves pk common_to_both= 
  if pk.Hex_partial_kite_t.investment <> None then [] else 
  let first_halves = heavy_part dim common_to_both in 
  Image.image (extend_with_first_alternative dim pk) first_halves ;;

let extensions_finished_and_non_finished dim pk =
   let common_to_both =  data_common_to_both_parts dim pk in 
   (* (Hex_springless_extension.extensions_finished_and_non_finished dim pk)
   @ *)
   (extensions_by_springboard_first_halves pk common_to_both)
   @ 
   (extensions_by_springboard_second_halves pk);;


end ;;


let extensions  =  Private.extensions_finished_and_non_finished ;; 
