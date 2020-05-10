(* 

#use"Hex_analysis/hex_springful_extension.ml";;

*)



module Private = struct 



let extend_with_first_alternative pk new_fa =
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_seas = pk.Hex_partial_kite_t.unvisited_seas
    and old_free_ones = pk.Hex_partial_kite_t.remaining_free_cells
    and requisitioned_territory = Hex_first_alternative_in_springboard.requisitioned_territory new_fa in 
    let restricted_islands = List.filter (Hex_first_alternative_in_springboard.check_island_after_fa_insertion new_fa) old_islands 
    and selector  =  List.filter (fun (_,sea)->Hex_first_alternative_in_springboard.check_sea_after_fa_insertion new_fa sea)   
    and remaining_free_ones = Hex_cell_set.setminus old_free_ones requisitioned_territory in
    {
      pk with 
        Hex_partial_kite_t.unvisited_islands = restricted_islands ;
        unvisited_seas =  selector old_seas;
        remaining_free_cells = remaining_free_ones ; 
        investment = Some new_fa ;
    }  ;;

let extend_with_second_alternative dim pk (cell2,new_island) =
    let fa = Option.unpack pk.Hex_partial_kite_t.investment in 
    let new_sb =(Hex_springboard_t.Sp(fa,cell2,new_island))  in  
    let pk2 = Hex_impose_active_cell.impose_cell_by_casing dim cell2 pk in 
    let old_steps = pk2.Hex_partial_kite_t.steps_so_far 
    and old_seas = pk.Hex_partial_kite_t.unvisited_seas
    and old_free_ones = pk2.Hex_partial_kite_t.remaining_free_cells in 
    let select_seas  =  List.filter (fun (_,sea)->
       not (Hex_cell_set.mem cell2 (Hex_named_connector.wet_earth sea))
    )   
    and remaining_free_ones = Hex_cell_set.outsert cell2 old_free_ones  in
    {
      pk2 with 
        Hex_partial_kite_t.steps_so_far = 
           (Hex_kite_element_t.Springboard new_sb)::old_steps ;
        unvisited_seas =  select_seas old_seas;
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



let extensions_by_springboard_first_halves dim pk common_to_both= 
  if pk.Hex_partial_kite_t.investment <> None then [] else 
  let first_halves = heavy_part dim common_to_both in 
  Image.image (extend_with_first_alternative pk) first_halves ;;

let extensions_by_springboard_second_halves dim pk common_to_both fa= 
  if pk.Hex_partial_kite_t.investment = None then [] else 
  let possible_second_halves = light_part common_to_both in 
  let second_halves = List.filter (
      fun (cell2,new_island) ->
         Hex_springboard.opt_constructor(fa,cell2,new_island) <> None
  ) possible_second_halves in 
  Image.image (extend_with_second_alternative dim pk) second_halves ;;

let extensions_by_springboard_halves dim pk =
   let last_stop = Hex_partial_kite_field.last_stop pk in 
   if Hex_kite_element.opt_island_component last_stop = None 
   then []
   else 
   let common_to_both =  data_common_to_both_parts dim pk in 
   match pk.Hex_partial_kite_t.investment with 
   None -> extensions_by_springboard_first_halves dim pk common_to_both 
   |Some(fa) -> extensions_by_springboard_second_halves dim pk common_to_both fa ;;

let extract_solutions l=
   let (final_ones,nonfinal_ones) = List.partition (
    Hex_partial_kite_field.test_for_finality 
   ) l in 
   let detailed_sols = Image.image Hex_finished_kite.solution_details  final_ones in 
   (detailed_sols,nonfinal_ones) ;;

let extensions_finished_and_non_finished dim pk =
   let (f1,u1) = Hex_springless_extension.extensions_finished_and_non_finished dim pk 
   and (f2,u2) = extract_solutions (extensions_by_springboard_halves dim pk) in 
   (f1@f2,u1@u2) ;;


end ;;


let extensions  =  Private.extensions_finished_and_non_finished ;; 
