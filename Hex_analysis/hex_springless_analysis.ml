(* 

#use"Hex_analysis/hex_springless_analysis.ml";;

*)

exception Kite_is_not_started;;

module Private = struct 




let to_molecular_linker  pk =
   (* The kite is assumed to be finished *)  
   Hex_molecular_linker.fold_merge 
     (Option.filter_and_unpack Hex_kite_element.to_molecular_linker pk.Hex_partial_kite_t.stops_so_far);;

exception Deduce_boarded_islands_exn of Hex_kite_element_t.t list ;;

let deduce_boarded_islands l =
    let n = ((List.length l)-1)/2 
    and naive_trier=(fun k ->
         let nc1 = Hex_kite_element.claim_sea (List.nth l (2*k-1)) 
         and island = Hex_kite_element.claim_island (List.nth l (2*k) )
         and nc2 = Hex_kite_element.claim_sea (List.nth l (2*k+1))  in 
         (nc1,island,nc2) ) in 
    let  trier=(fun k->try naive_trier k with _->raise(Deduce_boarded_islands_exn(l))) in     
    Ennig.doyle trier 1 (n-1);;

let active_part  pk =
   (* The kite is assumed to be finished *)  
    let unfiltered_l = pk.Hex_partial_kite_t.stops_so_far in 
    let l = List.filter ( 
       function (Hex_kite_element_t.Earth(_))
           |(Hex_kite_element_t.Sea(_)) -> true
           |_->false
    )  unfiltered_l in 
    let boarded_islands = deduce_boarded_islands l in 
    let contribution_from_seas = Hex_cell_set.fold_merge(Option.filter_and_unpack (
       function (Hex_kite_element_t.Sea(nc)) -> Some(Hex_named_connector.outer_earth nc)
       |_->None
    ) l) 
    and contribution_from_islands = Hex_cell_set.fold_merge(Image.image 
    (
       function (nc1,island,nc2)-> 
         let island1 = nc1.Hex_named_connector_t.exit 
         and island2 = nc2.Hex_named_connector_t.entry in 
         Hex_island.minimal_connection (island1,island2) island
    ) boarded_islands) in 
    let possibly_too_large = Hex_cell_set.merge contribution_from_islands contribution_from_seas in 
   let molecular_part = Hex_molecular_linker.support(to_molecular_linker pk) in 
   Hex_cell_set.setminus possibly_too_large molecular_part;;

let extend_with_island pk new_island = 
        let vague_new_elt = Hex_kite_element_t.Earth(new_island)
        and new_elt = Hex_kite_springless_element_t.Earth(new_island) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.stops_so_far = vague_new_elt :: (pk.Hex_partial_kite_t.stops_so_far);
          unvisited_islands = List.filter (fun x->x<>new_island ) 
             (pk.Hex_partial_kite_t.unvisited_islands);
    });;
    

let extend_with_sea pk new_nc = 
        let vague_new_elt = Hex_kite_element_t.Sea(new_nc) 
        and new_elt = Hex_kite_springless_element_t.Sea(new_nc) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.stops_so_far = vague_new_elt :: (pk.Hex_partial_kite_t.stops_so_far);
            unvisited_seas = List.filter 
              (fun (z,nc)->
                Hex_named_connector.check_disjointness new_nc nc) 
                (pk.Hex_partial_kite_t.unvisited_seas);
    });;



let springless_extensions_after_island partial_kite last_island =
   let candidates = partial_kite.Hex_partial_kite_t.unvisited_seas in
   let remaining_islands = partial_kite.Hex_partial_kite_t.unvisited_islands in
   let abc = partial_kite.Hex_partial_kite_t.added_by_casing in 
   let retained_ones  = Option.filter_and_unpack (
      fun (z,nc)->
        if (Hex_named_connector.check_entry last_island nc)
            &&(Hex_cell_set.is_included_in z abc) 
           &&(List.exists (Hex_named_connector.check_exit nc) remaining_islands) 
        then Some nc 
        else None   
   )  candidates in 
   Image.image (extend_with_sea partial_kite) retained_ones ;;

let springless_extensions_after_sea partial_kite last_nc =
   let candidates = partial_kite.Hex_partial_kite_t.unvisited_islands in
   let retained_ones  = List.filter (
      Hex_named_connector.check_exit last_nc  
   )  candidates in 
   Image.image (extend_with_island partial_kite) retained_ones ;;

let extensions_from_springless_last_elt partial_kite = function 
    Hex_kite_springless_element_t.Earth(last_island) ->  springless_extensions_after_island partial_kite last_island 
   |Hex_kite_springless_element_t.Sea(last_nc) ->  springless_extensions_after_sea partial_kite last_nc ;;

let springless_extensions partial_kite =
   match partial_kite.Hex_partial_kite_t.stops_so_far with 
    []->raise(Kite_is_not_started)
   |last_elt_in_vague_form::_-> 
       let last_elt = Hex_kite_element.to_springless last_elt_in_vague_form in 
       extensions_from_springless_last_elt partial_kite last_elt ;;

let extensions_finished_and_non_finished partial_kite =
      let base = springless_extensions partial_kite 
      and orig_side = partial_kite.Hex_partial_kite_t.original_side in 
      let (finished1,unfinished1) =List.partition (fun (last_elt,_)->
          Hex_kite_element.is_final orig_side (Hex_kite_element.of_springless last_elt)) base in 
      let finished2 = Image.image (fun (_,pk)->
        (List.rev(pk.Hex_partial_kite_t.stops_so_far),to_molecular_linker pk,active_part pk)) finished1 
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
         (pk,extensions_finished_and_non_finished pk) 
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
let extensions = Private.springless_extensions;;
let extensions_finished_and_non_finished = Private.extensions_finished_and_non_finished ;; 
let finalize eob pk= fst(Private.main (Private.late_starter eob pk,false));;
let to_molecular_linker = Private.to_molecular_linker;;
