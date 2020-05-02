(* 

#use"Hex_analysis/hex_springless_analysis.ml";;

*)


module Private = struct 


let to_molecular_linker  pk =
   (* The kite is assumed to be finished *) 
   Hex_molecular_linker.fold_merge 
   (Option.filter_and_unpack 
      Hex_kite_element.to_molecular_linker 
        (pk.Hex_partial_kite_t.steps_so_far));;

let original_side pk =
   Hex_anchor.unique_side (Hex_island.anchor pk.Hex_partial_kite_t.place_of_birth);;

let opt_final_death pk=
   let opt_last_island=(match pk.Hex_partial_kite_t.steps_so_far  with 
    [] -> Some pk.Hex_partial_kite_t.place_of_birth 
   |last_elt::_ -> Hex_kite_element.opt_island_component last_elt
   ) in 
     match opt_last_island with 
     None -> None
     |Some(island) ->  
     if Hex_island.anchor island <> Hex_anchor_t.No_anchor
     then Some island 
     else None ;;

let compute_place_of_death pk=
   match opt_final_death pk with 
   Some(death_already_occurred)-> death_already_occurred 
   |None ->
   let final_side = Hex_cardinal_direction.oppose(original_side pk) in 
   Hex_island.get_side final_side pk.Hex_partial_kite_t.unvisited_islands ;;      

   

let test_for_finality pk = 
   if Hex_anchor.is_two_edged 
      (Hex_island.anchor(pk.Hex_partial_kite_t.place_of_birth)) 
   then true 
   else 
   let steps = pk.Hex_partial_kite_t.steps_so_far in 
   if List.exists Hex_kite_element.is_two_edged steps 
   then true
   else 
   match steps with 
   [] -> false 
   |last_elt::_->
   match  last_elt with 
   Hex_kite_element_t.Sea(nc) -> 
      let place_of_death = compute_place_of_death pk in 
      Hex_named_connector.check_exit nc place_of_death 
   |_ -> let island = Hex_kite_element.extract_island last_elt in  
         Hex_island.anchor island <> Hex_anchor_t.No_anchor  ;;

let helper2_for_removing_redundant_islands treated pending1 pending2  = 
   if (Hex_kite_springless_element.is_an_island pending1)
          &&
          (Hex_kite_springless_element.is_an_island pending2)
   then List.rev (pending2::treated)
   else List.rev (pending2::pending1::treated) ;;

let rec helper_for_removing_redundant_islands (treated,pending1,pending2,to_be_treated) = 
   match to_be_treated with 
    [] -> helper2_for_removing_redundant_islands treated pending1 pending2 
   |pending3::others ->
       if (Hex_kite_springless_element.is_an_island pending1)
          &&
          (Hex_kite_springless_element.is_an_island pending2)
       then helper_for_removing_redundant_islands (treated,pending2,pending3,others)
       else helper_for_removing_redundant_islands (pending1::treated,pending2,pending3,others) ;;
      
let remove_redundant_islands l=match l with 
  [] -> []
  |pending1::others1 ->
    (
      match others1 with 
      [] -> [pending1]
      |pending2::others2 -> helper_for_removing_redundant_islands ([],pending1,pending2,others2)
    );; 


exception Deduce_boarded_islands_exn of (Hex_kite_springless_element_t.t list) * int ;;

let deduce_boarded_islands  l (birth,death) = 
    let n = ((List.length l)-1)/2  in 
    let gl = (fun j->List.nth l (j-1)) in 
    let sea_entry = (fun x->(Hex_kite_springless_element.claim_sea (x)).Hex_named_connector_t.entry )
    and sea_exit = (fun x->(Hex_kite_springless_element.claim_sea (x)).Hex_named_connector_t.exit ) in 
    let first_in_triple =(fun k->
       if k=1 then Hex_island.eviscerate birth else sea_exit(gl(2*k-3))
    ) 
    and second_in_triple = (fun k->
       if k=1   then birth else 
       if k=n+2 then death else Hex_kite_springless_element.claim_island(gl (2*k-2))
    )   
    and third_in_triple = (fun k->
       if k=n+2 then Hex_island.eviscerate death else sea_entry(gl(2*k-1))
    ) in  
    let naive_trier=(fun k ->
         (first_in_triple k,second_in_triple k,third_in_triple k) ) in 
    let  trier=(fun k->try naive_trier k with _->raise(Deduce_boarded_islands_exn(l,k))) in     
    Ennig.doyle trier 1 (n+2);;

let active_part  pk =
   (* The kite is assumed to be finished *)  
   let birth = pk.Hex_partial_kite_t.place_of_birth 
   and death = compute_place_of_death pk   in 
    let unfiltered_l= List.rev pk.Hex_partial_kite_t.steps_so_far in 
    let l = remove_redundant_islands (Image.image Hex_kite_element.compress_to_springless  unfiltered_l)  in 
    let boarded_islands = deduce_boarded_islands l (birth,death)  in 
    let contribution_from_seas = Hex_cell_set.fold_merge(Option.filter_and_unpack (
       function (Hex_kite_springless_element_t.Sea(nc)) -> Some(Hex_named_connector.outer_earth nc)
       |_->None
    ) l) 
    and contribution_from_islands = Hex_cell_set.fold_merge(Image.image 
    (function (island1,island,island2)-> 
         Hex_island.minimal_connection (island1,island2) island
    ) boarded_islands) 
    and contribution_from_springboards = Option.filter_and_unpack (
       function (Hex_kite_element_t.Springboard(spr))-> Some(Hex_springboard.active_part spr)
        | _ -> None 
   ) unfiltered_l  in 
   let possibly_too_large = Hex_cell_set.fold_merge 
     (contribution_from_islands::contribution_from_seas::contribution_from_springboards) in 
   let molecular_part = Hex_molecular_linker.support(to_molecular_linker pk) in 
   Hex_cell_set.setminus possibly_too_large molecular_part;;


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
   let (subtly_final,nonfinal) = List.partition (fun (_,pk2)->test_for_finality pk2) unclear_items  in 
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

let solution_details pk = 
    let steps_in_order=List.rev(pk.Hex_partial_kite_t.steps_so_far) in
    let (fst_step,other_steps)=Listennou.ht steps_in_order in     
    let birth = pk.Hex_partial_kite_t.place_of_birth in 
    (birth,fst_step,other_steps,to_molecular_linker pk,active_part pk);;

let extensions_finished_and_non_finished dim partial_kite =
      let (finished1,unfinished1) = springless_extensions dim partial_kite in 
      let finished2 = Image.image (fun (_,pk)->solution_details pk) finished1 
      and unfinished2 = Image.image snd unfinished1 in 
      (finished2,unfinished2);; 

let determine_winner pk =
   let place_of_birth = pk.Hex_partial_kite_t.place_of_birth in 
   let birth = Hex_anchor.unique_side (Hex_island.anchor place_of_birth) in 
   Hex_cardinal_direction.player_for_side birth ;;

let nonredundant_list_of_visited_islands pk =
   let unfiltered_l=List.rev pk.Hex_partial_kite_t.steps_so_far in  
   let l=remove_redundant_islands 
    (Image.image Hex_kite_element.compress_to_springless  unfiltered_l) in  
   Option.filter_and_unpack (
        function 
        Hex_kite_springless_element_t.Sea(_)->None 
        |Earth(island)->Some island
   ) l;;
   

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

let active_part = Private.active_part ;;
let extensions_finished_and_non_finished = Private.extensions_finished_and_non_finished ;; 
let finalize dim pk= fst(Private.main (Private.late_starter dim pk,false));;
let is_final = Private.test_for_finality ;;
let solution_details = Private.solution_details ;;
let to_molecular_linker = Private.to_molecular_linker;;
