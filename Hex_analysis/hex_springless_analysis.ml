(* 

#use"Hex_analysis/hex_springless_analysis.ml";;

*)


module Private = struct 


let to_molecular_linker  pk =
   (* The kite is assumed to be finished *) 
   let fst_step = pk.Hex_partial_kite_t.first_step 
   and rl=pk.Hex_partial_kite_t.stops_so_far in 
   Hex_molecular_linker.fold_merge 
   (Option.filter_and_unpack 
      Hex_kite_element.to_molecular_linker 
        (fst_step::rl));;

let original_side pk =
   Option.unpack (Hex_island.outer_earth pk.Hex_partial_kite_t.place_of_birth);;

let compute_place_of_death pk=
   let final_side = Hex_cardinal_direction.oppose(original_side pk) in 
   Hex_island.get_side final_side pk.Hex_partial_kite_t.unvisited_islands ;;      

let test_for_finality pk = 
   let place_of_death = compute_place_of_death pk in 
   match pk.Hex_partial_kite_t.stops_so_far with 
   [] -> false 
   |last_elt::_->match  last_elt with 
   Hex_kite_element_t.Sea(nc) -> Hex_named_connector.check_exit nc place_of_death 
   | _ -> false;;

exception Deduce_boarded_islands_exn of (Hex_kite_element_t.t list) * int ;;

let deduce_boarded_islands  l (birth,death) (fst_step,lst_step)=
     let original_side = Hex_island.eviscerate birth
     and final_side =  Hex_island.eviscerate death in  
    let n = (List.length l)/2  in 
    let gl = (fun j->List.nth l (j-1)) in 
    let sea_entry = (fun x->(Hex_kite_element.claim_sea (x)).Hex_named_connector_t.entry )
    and sea_exit = (fun x->(Hex_kite_element.claim_sea (x)).Hex_named_connector_t.exit ) in 
    let starter_entry = sea_entry   
    and starter_exit = sea_exit   
    and ender_entry = (fun x->(Hex_kite_element.claim_sea (x)).Hex_named_connector_t.entry )  
    and ender_exit = (fun x->(Hex_kite_element.claim_sea (x)).Hex_named_connector_t.exit )   in 
    let exits_in_triple = (fun k->
       if k=1 then original_side else 
       if k=2 then starter_exit(fst_step) else 
       if k=n+2 then ender_exit(lst_step) else sea_exit(gl (2*k-4))  ) 
    and middle_in_triple = (fun k->
       if k=1   then birth else 
       if k=n+2 then death else Hex_kite_element.claim_island(gl (2*k-3))  )
    and entries_in_triple = (fun k->
       if k=1 then starter_entry(fst_step) else 
       if k=n+1 then ender_entry(lst_step) else 
       if k=n+2 then final_side else sea_entry(gl (2*k-2))  ) in     
    let naive_trier=(fun k ->
         (exits_in_triple k,middle_in_triple k,entries_in_triple k) ) in 
    let  trier=(fun k->try naive_trier k with _->raise(Deduce_boarded_islands_exn(l,k))) in     
    Ennig.doyle trier 1 (n+2);;

let active_part  pk =
   (* The kite is assumed to be finished *)  
   let birth = pk.Hex_partial_kite_t.place_of_birth 
   and death = compute_place_of_death pk
   and rl=pk.Hex_partial_kite_t.stops_so_far 
   and fst_step = pk.Hex_partial_kite_t.first_step  in 
   let lst_step = List.hd rl in 
    let unfiltered_l=List.rev rl in  
    let l = List.filter ( 
       function (Hex_kite_element_t.Earth(_))
           |(Hex_kite_element_t.Sea(_)) -> true
           |_->false
    )  unfiltered_l  in 
    let boarded_islands = deduce_boarded_islands l (birth,death) (fst_step,lst_step) in 
    let contribution_from_seas = Hex_cell_set.fold_merge(Option.filter_and_unpack (
       function (Hex_kite_element_t.Sea(nc)) -> Some(Hex_named_connector.outer_earth nc)
       |_->None
    ) l) 
    and contribution_from_islands = Hex_cell_set.fold_merge(Image.image 
    (function (island1,island,island2)-> 
         Hex_island.minimal_connection (island1,island2) island
    ) boarded_islands) in 
    let possibly_too_large = Hex_cell_set.merge contribution_from_islands contribution_from_seas in 
   let molecular_part = Hex_molecular_linker.support(to_molecular_linker pk) in 
   Hex_cell_set.setminus possibly_too_large molecular_part;;


let extend_with_island pk new_island = 
        let vague_new_elt = Hex_kite_element_t.Earth(new_island)
        and new_elt = Hex_kite_springless_element_t.Earth(new_island) in 
        let old_stops=pk.Hex_partial_kite_t.stops_so_far in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.stops_so_far = vague_new_elt::old_stops;
          unvisited_islands = List.filter (fun x->x<>new_island ) 
             (pk.Hex_partial_kite_t.unvisited_islands);
    });;
    

let extend_with_sea pk new_nc = 
        let vague_new_elt = Hex_kite_element_t.Sea(new_nc) 
        and new_elt = Hex_kite_springless_element_t.Sea(new_nc) in 
        let old_stops=pk.Hex_partial_kite_t.stops_so_far in 
        let old_middle_seas = pk.Hex_partial_kite_t.unvisited_seas 
        and old_end_seas = pk.Hex_partial_kite_t.unvisited_enders in 
        let selector =  List.filter 
              (fun (z,nc)->
                Hex_named_connector.check_disjointness new_nc nc) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.stops_so_far = vague_new_elt::old_stops ;
            unvisited_seas = selector old_middle_seas ;
            unvisited_enders = selector old_end_seas ;
    });;

let extend_with_final_sea pk final_nc = 
        let vague_new_elt = Hex_kite_element_t.Sea(final_nc)  in 
        let old_stops=pk.Hex_partial_kite_t.stops_so_far in 
        let old_middle_seas = pk.Hex_partial_kite_t.unvisited_seas 
        and old_end_seas = pk.Hex_partial_kite_t.unvisited_enders in 
        let selector =  List.filter 
              (fun (z,nc)->
                Hex_named_connector.check_disjointness final_nc nc) in 
     
     {
         pk with 
          Hex_partial_kite_t.stops_so_far = vague_new_elt::old_stops ;
          unvisited_seas = selector old_middle_seas ;
            unvisited_enders = selector old_end_seas ;
    };;


let springless_extensions_after_island dim partial_kite last_island =
   let remaining_islands = partial_kite.Hex_partial_kite_t.unvisited_islands in
   let islanders = Image.image (fun nc->(Hex_cell_set.empty_set,nc)) 
      (Hex_named_connector.islanders dim last_island remaining_islands) in 
   let abc = partial_kite.Hex_partial_kite_t.added_by_casing in 
   let selector  = Option.filter_and_unpack (
      fun (z,nc)->
        if (Hex_named_connector.check_entry last_island nc)
            &&(Hex_cell_set.is_included_in z abc) 
           &&(List.exists (Hex_named_connector.check_exit nc) remaining_islands) 
        then Some (extend_with_sea partial_kite nc) 
        else None   
   )   in 
   let clearly_final = selector partial_kite.Hex_partial_kite_t.unvisited_enders 
   and unclear_items = selector ((partial_kite.Hex_partial_kite_t.unvisited_seas)@islanders) in
   let (subtly_final,nonfinal) = List.partition (fun (_,pk2)->test_for_finality pk2) unclear_items  in 
   (clearly_final@subtly_final,nonfinal) ;;

let springless_extensions_after_sea partial_kite last_nc =
   let candidates = partial_kite.Hex_partial_kite_t.unvisited_islands in
   let retained_ones  = List.filter (
      Hex_named_connector.check_exit last_nc  
   )  candidates in 
   ([],Image.image (extend_with_island partial_kite) retained_ones);;

let extensions_from_springless_last_elt dim partial_kite = function 
    Hex_kite_springless_element_t.Earth(last_island) ->  springless_extensions_after_island dim partial_kite last_island 
   |Hex_kite_springless_element_t.Sea(last_nc) ->  springless_extensions_after_sea partial_kite last_nc ;;

let springless_extensions dim pk =
   let fst_step = pk.Hex_partial_kite_t.first_step 
   and rl=pk.Hex_partial_kite_t.stops_so_far in 
   let last_elt = (match rl with 
     []->Hex_kite_element.to_springless fst_step 
     |x::_-> Hex_kite_element.to_springless x ) in 
   extensions_from_springless_last_elt dim pk last_elt ;;

let solution_details pk = 
        let a1 = pk.Hex_partial_kite_t.place_of_birth 
        and a2 = pk.Hex_partial_kite_t.first_step 
        and rl=pk.Hex_partial_kite_t.stops_so_far in 
        let l=List.rev rl in 
        (a1,a2,l,to_molecular_linker pk,active_part pk);;

let extensions_finished_and_non_finished dim partial_kite =
      let (finished1,unfinished1) = springless_extensions dim partial_kite in 
      let finished2 = Image.image (fun (_,pk)->solution_details pk) finished1 
      and unfinished2 = Image.image snd unfinished1 in 
      (finished2,unfinished2);; 

(* Old copy of H_ex_kite_factory starts here *)

let starters eob = 
    
      (eob.Hex_end_of_battle_t.dimension,
       eob.Hex_end_of_battle_t.winner,
       eob,
       [],[],
       Hex_starters_for_kite.starters eob);;

let late_starter eob pk= 
      (eob.Hex_end_of_battle_t.dimension,
       eob.Hex_end_of_battle_t.winner,
       eob,
       [],[],[pk]);;

let pusher (factory,_) = 
   let (d,wi,i,fi,fa,uf) = factory in 
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
   let new_factory = (d,wi,i,new_finished_ones,new_failures,new_partial_kites) in           
   (new_factory,new_partial_kites=[]);;

let rec main walker =
   let (factory,computation_has_finished) = walker in 
   if computation_has_finished 
   then let (d,wi,i,fi,fa,uf) = factory in 
        (fi,fa)
   else main (pusher walker) ;; 

let compute eob = main (starters eob,false);;

(* Old copy of H_ex_kite_factory ends here *)

end ;; 

let active_part = Private.active_part ;;
let extend_with_sea = Private.extend_with_sea ;;
let extend_with_final_sea = Private.extend_with_final_sea ;;
let extensions = Private.springless_extensions;;
let extensions_finished_and_non_finished = Private.extensions_finished_and_non_finished ;; 
let finalize eob pk= fst(Private.main (Private.late_starter eob pk,false));;
let original_side = Private.original_side ;;
let solution_details = Private.solution_details ;;
let to_molecular_linker = Private.to_molecular_linker;;
