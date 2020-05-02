(* 

#use"Hex_analysis/hex_springless_analysis.ml";;

*)


module Private = struct 


let extend_with_island pk new_island = 
        let vague_new_elt = Hex_kite_element_t.Earth(new_island)
        and new_elt = Hex_kite_springless_element_t.Earth(new_island) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.steps_so_far = 
               (vague_new_elt::pk.Hex_partial_kite_t.steps_so_far);
          unvisited_islands = List.filter (fun x->x<>new_island ) 
             (pk.Hex_partial_kite_t.unvisited_islands);
    });;
    

let extend_with_sea pk new_nc = 
        let vague_new_elt = Hex_kite_element_t.Sea(new_nc) 
        and new_elt = Hex_kite_springless_element_t.Sea(new_nc) in 
        let old_steps=pk.Hex_partial_kite_t.steps_so_far in 
        let old_seas = pk.Hex_partial_kite_t.unvisited_seas 
        and old_free_ones = pk.Hex_partial_kite_t.remaining_free_cells in 
        let selector =  List.filter 
              (fun (z,nc)->
                Hex_named_connector.check_disjointness new_nc nc) 
        and remaining_free_ones = Hex_cell_set.setminus old_free_ones
         (Hex_named_connector.inner_sea new_nc) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.steps_so_far = vague_new_elt::old_steps ;
            unvisited_seas = selector old_seas ;
            remaining_free_cells = remaining_free_ones ;
    });;


let springless_extensions_after_island dim partial_kite last_island =
   let remaining_islands = partial_kite.Hex_partial_kite_t.unvisited_islands in
   let unchecked_islanders = Hex_named_connector.islanders dim last_island remaining_islands in 
   let islanders = Option.filter_and_unpack (
      fun nc-> 
      let impossibilities = Hex_cell_set.setminus (Hex_named_connector.inner_sea nc) 
                           partial_kite.Hex_partial_kite_t.remaining_free_cells in 
      if impossibilities = Hex_cell_set.empty_set
      then Some(Hex_cell_set.empty_set,nc)
      else None 
   )  unchecked_islanders in  
   let abc = partial_kite.Hex_partial_kite_t.added_by_casing in 
   let selector  = Option.filter_and_unpack (
      fun (z,nc)->
        if (Hex_named_connector.check_entry last_island nc)
            &&(Hex_cell_set.is_included_in z abc) 
           &&(List.exists (Hex_named_connector.check_exit nc) remaining_islands) 
        then Some (extend_with_sea partial_kite nc) 
        else None   
   )   in 
   let unclear_items = selector ((partial_kite.Hex_partial_kite_t.unvisited_seas)@islanders) in
   let (subtly_final,nonfinal) = List.partition (fun (_,pk2)->Hex_partial_kite_field.test_for_finality pk2) unclear_items  in 
   (subtly_final,nonfinal) ;;

let springless_extensions_after_sea partial_kite last_nc =
   (* if a two-edged is created, it will never be in the unvisited_islands field,
      so the extensions created here are always non-final 
    *)
   let compatible_islands  = List.filter (
      Hex_named_connector.check_exit last_nc  
   )  partial_kite.Hex_partial_kite_t.unvisited_islands in 
   ([],Image.image (extend_with_island partial_kite) compatible_islands);;

let extensions_from_last_elt dim partial_kite last_elt = match last_elt with
    Hex_kite_element_t.Sea(last_nc) ->  springless_extensions_after_sea partial_kite last_nc 
   | _ -> let last_island = Hex_kite_element.extract_island last_elt in 
          springless_extensions_after_island dim partial_kite last_island ;;

let springless_extensions dim pk =
   let last_elt = (match pk.Hex_partial_kite_t.steps_so_far with 
     []->Hex_kite_element_t.Earth(pk.Hex_partial_kite_t.place_of_birth)
     |x::_-> x ) in 
   extensions_from_last_elt dim pk last_elt ;;


let extensions_finished_and_non_finished dim partial_kite =
      let (finished1,unfinished1) = springless_extensions dim partial_kite in 
      let finished2 = Image.image (fun (_,pk)->Hex_finished_kite.solution_details pk) finished1 
      and unfinished2 = Image.image snd unfinished1 in 
      (finished2,unfinished2);; 

let determine_winner pk =
   let place_of_birth = pk.Hex_partial_kite_t.place_of_birth in 
   let birth = Hex_anchor.unique_side (Hex_island.anchor place_of_birth) in 
   Hex_cardinal_direction.player_for_side birth ;;

(* Old copy of H_ex_kite_factory starts here *)


let late_starter dim pk= 
      (dim,
       determine_winner pk,
       [],[],[pk]);;

let pusher (factory,_) = 
   let (d,wi,fi,fa,uf) = factory in 
   let raw_result=Image.image (
         fun pk->
         (pk,extensions_finished_and_non_finished d pk) 
   ) uf in  
   let (failures1,nonfailures1) = List.partition (fun (_,p)->p=([],[]) ) raw_result in 
   let new_failures = List.rev_append (Image.image fst failures1) fa in 
   let new_moleculars = List.flatten (Image.image (fun (_,p)->fst p) nonfailures1)
   and new_partial_kites = List.flatten (Image.image (fun (_,p)->snd p) nonfailures1) in 
   let ordered_new_moleculars = Ordered.sort Total_ordering.standard new_moleculars in 
   let new_finished_ones = Ordered.merge Total_ordering.standard
          ordered_new_moleculars fi in   
   let new_factory = (d,wi,new_finished_ones,new_failures,new_partial_kites) in           
   (new_factory,new_partial_kites=[]);;

let rec main walker =
   let (factory,computation_has_finished) = walker in 
   if computation_has_finished 
   then let (d,wi,fi,fa,uf) = factory in 
        (fi,fa)
   else main (pusher walker) ;; 


(* Old copy of H_ex_kite_factory ends here *)


end ;; 


let extensions_finished_and_non_finished = Private.extensions_finished_and_non_finished ;; 
let finalize dim pk= fst(Private.main (Private.late_starter dim pk,false));;
