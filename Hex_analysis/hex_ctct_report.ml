(* 

#use"Hex_analysis/hex_ctct_report.ml";;

*)


exception Short_circuit ;; 

module Private = struct 

let step1_during_initialization eob = 
  let active_cells = eob.Hex_end_of_battle_t.ally_territory 
  and w = eob.Hex_end_of_battle_t.winner in 
  let (side1,side2) = Hex_cardinal_direction.sides_for_player w in 
  (Some side1,[])::(Some side2,[])::(Hex_cell_set.image (fun cell->
     (None, [cell])
  ) active_cells) ;;



let step2_during_initialization eob = 
  let dim = eob.Hex_end_of_battle_t.dimension  in 
  let l = step1_during_initialization eob in 
  Image.image (
    fun (opt_side,opt_cell)->
      let nghbrs = (match opt_side with 
          None -> Hex_cell.neighbors dim (List.hd opt_cell)
          |Some(side)-> Hex_cardinal_direction.Border.enumerate_all dim side
      ) in 
      (opt_side,Hex_cell_set.safe_set opt_cell,nghbrs)
  ) l;;

let classify_neighbors eob (opt_side,active_dwellers,nghbrs) = 
  let ref_for_actives = ref Hex_cell_set.empty_set
  and ref_for_passives = ref Hex_cell_set.empty_set in 
  let _= List.iter (
     fun nghbr -> match Hex_end_of_battle.assess eob nghbr with 
      Hex_eob_result_t. Ally_territory -> ref_for_actives := Hex_cell_set.insert nghbr (!ref_for_actives) 
      | Unoccupied -> ref_for_passives := Hex_cell_set.insert nghbr  (!ref_for_passives) 
      | _ -> ()
  ) nghbrs in 
  (opt_side,active_dwellers,!ref_for_actives,!ref_for_passives);;

let initialize eob = 
  let  l = step2_during_initialization eob in 
  Image.image (classify_neighbors eob) l;;

let rec find_untreated_item (already_looked_up,to_be_looked_up) =
   match to_be_looked_up with  
   [] -> None 
   | item :: other_items ->
     let (opt_side,active_dwellers,active_neighbors,passive_neighbors) = item in 
      if Hex_cell_set.length active_neighbors = 0
      then find_untreated_item (item::already_looked_up,other_items) 
      else Some(List.rev already_looked_up,item,other_items);;
 
let join_two_opts opt1 opt2 =
   if opt1 = None then opt2 else 
   if opt2 = None then opt1 else
   raise(Short_circuit);; 

type elt_type = 
  (Hex_cardinal_direction_t.t option * Hex_cell_set_t.t * Hex_cell_set_t.t *
   Hex_cell_set_t.t);;

type walker_type = 
  elt_type list *
  (elt_type list * elt_type * elt_type list)
  option ;;

let pusher ((l,search_result):walker_type)=
  let (before,item,after) = Option.unpack search_result  in 
  let (_,_,active_neighbors0,_) = item in 
  let (touched_after,untouched_after) = List.partition (
      fun (_,active_dwellers1,active_neighbors1,_) ->  
        (Hex_cell_set.intersects active_neighbors0 active_dwellers1)||
        (Hex_cell_set.intersects active_neighbors0 active_neighbors1) 
  ) after in 
  let all_touched_ones = item::touched_after in 
  let ref_for_opt_side = ref None
  and ref_for_active_dwellers = ref (Hex_cell_set.empty_set)
  and ref_for_active_neighbors = ref (Hex_cell_set.empty_set)
  and ref_for_passive_neighbors = ref (Hex_cell_set.empty_set) in 
  let _= List.iter (
      fun (opt_side,active_dwellers,active_neighbors,passive_neighbors)->
         ref_for_opt_side := join_two_opts opt_side (!ref_for_opt_side);
         ref_for_active_dwellers:= Hex_cell_set.merge active_dwellers (!ref_for_active_dwellers);
         ref_for_active_neighbors:= Hex_cell_set.merge active_neighbors (!ref_for_active_neighbors);
         ref_for_passive_neighbors:= Hex_cell_set.merge passive_neighbors (!ref_for_passive_neighbors);
  ) all_touched_ones in 
  let new_opt_side = (!ref_for_opt_side) 
  and new_active_dwellers = (!ref_for_active_dwellers) in 
  let new_active_neighbors = Hex_cell_set.setminus (!ref_for_active_neighbors) new_active_dwellers 
  and new_passive_neighbors = (!ref_for_passive_neighbors) in 
  let new_item = (new_opt_side,new_active_dwellers,new_active_neighbors,new_passive_neighbors) in 
  let new_l = before @ (new_item::untouched_after) in 
  let new_search_result = find_untreated_item ([],new_l) in 
  ((new_l,new_search_result):walker_type);;

   
let rec iterator walker =
  if snd walker = None 
  then  Image.image (
          fun (opt_side,active_dwellers,_,passive_neighbors)->
            {
               Hex_ctct_report_item_t.opt_side =opt_side ;  
               active_dwellers = active_dwellers ;
               passive_neighbors = passive_neighbors ;
           }
       ) (fst walker)
  else iterator(pusher walker);;


let compute_all_items eob =
   let l=initialize eob in 
   let search_result = find_untreated_item ([],l) in 
   iterator (l,search_result);;

let constructor fg =
     let eob = Hex_end_of_battle.of_finished_game fg in 
     {
      Hex_ctct_report_t.dimension = eob.Hex_end_of_battle_t.dimension ;
         winner = eob.Hex_end_of_battle_t.winner ;
         ally_territory = eob.Hex_end_of_battle_t.ally_territory ;
         enemy_territory = eob.Hex_end_of_battle_t.enemy_territory ;
         free_territory = eob.Hex_end_of_battle_t.free_territory ;
         items  = compute_all_items eob;
   } ;; 
   
let pattern_is_included_in_report (Hex_pattern_t.Pat l) ctct_report =
     List.for_all (
       fun ((i,j),lbl) ->
         let cell = Hex_cell.of_int_pair (i,j) in 
         if lbl 
         then  Hex_cell_set.mem cell (ctct_report.Hex_ctct_report_t.enemy_territory)
         else not(
              (Hex_cell_set.mem cell (ctct_report.Hex_ctct_report_t.enemy_territory))
              ||
              (Hex_cell_set.mem cell (ctct_report.Hex_ctct_report_t.ally_territory))
         )
     ) l;;


let test_composite_pattern ctct_report uc (Hex_composite_pattern_t.C(patt,uc2)) =
   let dim = ctct_report.Hex_ctct_report_t.dimension in   
   let temp1 = Hex_body_movement.between_unified_connectors dim uc2 uc in 
   Option.seek (
      fun mv ->
          let patt2 = Hex_body_movement.apply_on_pattern mv patt in 
          pattern_is_included_in_report patt2 ctct_report
   ) temp1;;

let approach_composite_pattern_set ctct_report l uc=
   Option.find_and_stop (test_composite_pattern ctct_report uc) l;;

end ;; 

let approach_composite_pattern_set = Private.approach_composite_pattern_set;; 

let constructor = Private.constructor ;;

let to_end_of_battle ctct_report = {
   Hex_end_of_battle_t. dimension = ctct_report.Hex_ctct_report_t.dimension ;
                       winner = ctct_report.Hex_ctct_report_t.winner ;
                       ally_territory = ctct_report.Hex_ctct_report_t.ally_territory;
                       enemy_territory = ctct_report.Hex_ctct_report_t.enemy_territory;
                       free_territory = ctct_report.Hex_ctct_report_t.free_territory
} ;;