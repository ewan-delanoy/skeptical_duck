(* 

#use"Hex_analysis/hex_connctcmpnt_report.ml";;

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
      else Some(List.rev other_items,item,already_looked_up);;
 
let join_two_opts opt1 opt2 =
   if opt1 = None then opt2 else 
   if opt2 = None then opt1 else
   raise(Short_circuit);; 

let pusher (l,search_result)=
  let (before,item,after) = Option.unpack search_result  in 
  let (_,_,active_neighbors0,_) = item in 
  let (touched_before,untouched_before) = List.partition (
      fun (_,active_cells,_,_) ->  
        Hex_cell_set.intersects active_cells active_neighbors0 
  ) before in 
  let all_touched_ones = item::touched_before in 
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
  let new_l = untouched_before @ (new_item::after) in 
  let new_search_result = find_untreated_item ([],List.rev new_l) in 
  (new_l,new_search_result);;

   
let rec iterator walker =
  if snd walker = None 
  then Hex_ctct_report_t.R( Image.image (
          fun (opt_side,active_dwellers,_,passive_neighbors)->
            {
               Hex_ctct_report_item_t.opt_side =opt_side ;  
               members = active_dwellers ;
               passive_neighbors = passive_neighbors ;
           }
       ) (fst walker))
  else iterator(pusher walker);;


let main eob =
   let l=initialize eob in 
   let search_result = find_untreated_item ([],List.rev l) in 
   iterator (l,search_result);;

end ;; 

let about_end_of_battle = Private.main ;;
