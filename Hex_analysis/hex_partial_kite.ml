(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)

exception Kite_is_not_started;;

module Private = struct 


let add_cell_by_casing_in_contact_case dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)=  
    let new_islands = Hex_island.add_cell_by_casing dim new_cell (last_island::old_islands) 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
    let remade_last_stop = Hex_kite_element_t.Earth(List.hd new_islands) in 
   {
      pk with
      Hex_partial_kite_t.stops_so_far = remade_last_stop :: previous_stops;
      unvisited_islands = List.tl new_islands;
      added_by_casing = new_abc;
   };;

let add_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc)=  
    let new_islands = Hex_island.add_cell_by_casing dim new_cell old_islands 
    and new_abc = Hex_cell_set.insert new_cell old_abc in 
   {
      pk with
      Hex_partial_kite_t.unvisited_islands = new_islands;
      added_by_casing = new_abc;
   };;


let add_cell_by_casing dim new_cell pk=  
    let old_islands = pk.Hex_partial_kite_t.unvisited_islands 
    and old_abc = pk.Hex_partial_kite_t.added_by_casing  in 
    let old_stops = pk.Hex_partial_kite_t.stops_so_far in 
    let (last_stop,previous_stops) = Listennou.ht old_stops in 
    let last_island = Hex_kite_element.claim_island last_stop in 
    if Hex_island.test_for_neighbor dim last_island new_cell 
    then add_cell_by_casing_in_contact_case    dim new_cell pk (old_islands,old_abc) (last_island,previous_stops)
    else add_cell_by_casing_in_no_contact_case dim new_cell pk (old_islands,old_abc) ;;


let local_cmp = Total_ordering.product 
    Hex_cell_set.length_first_cmp Total_ordering.standard;;

let starters_for_side end_of_battle side =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let clean = List.filter (
      Hex_named_connector.check_compatiblity end_of_battle ) in 
   let base1 = clean (Hex_named_connector.starters_for_side dim side)
   and pre_base2 = clean ((Hex_named_connector.middlers dim)@
                       (Hex_named_connector.enders_for_side dim side)
                      ) in 
   let unordered_base2 = Image.image (
     fun nc -> (Hex_named_connector.missing_earth end_of_battle nc,nc)
   )  pre_base2 in                  
   let base2 = Ordered.sort local_cmp unordered_base2 in                    
   let islands = Hex_island.decompose end_of_battle in 
   let first_island = Hex_island.get_side side islands  in                    
   let constructor = (
      fun first_nc ->
        let new_base=List.filter ( 
         fun (z,other_nc) -> 
         Hex_named_connector.check_disjointness first_nc other_nc ) base2 in
        let elt1 = Hex_kite_element_t.Earth(first_island)
        and elt2 = Hex_kite_element_t.Sea(first_nc) in 
        {
            Hex_partial_kite_t.stops_so_far = [elt2;elt1];
            original_side = side ;
            unvisited_islands = List.filter (fun x->x<>first_island ) islands;
            unvisited_seas = new_base ;
            added_by_casing = Hex_cell_set.empty_set;
        }
   ) in 
   let conditional_constructor = (fun 
     first_nc -> 
        if List.exists (Hex_named_connector.check_exit first_nc) islands
        then Some(constructor first_nc)
        else None 
   ) in 
   Option.filter_and_unpack conditional_constructor  base1 ;; 

let to_molecular_linker  pk =
   (* The kite is assumed to be finished *)  
   Hex_molecular_linker.fold_merge 
     (Option.filter_and_unpack Hex_kite_element.to_molecular_linker pk.Hex_partial_kite_t.stops_so_far);;



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

let extend_with_springboard dim pk new_sb =
    let (Hex_springboard_t.Sp(cell,path,solution,cell2,nc2)) = new_sb in 
    let pk2 = add_cell_by_casing dim cell2 pk in  
    let old_islands = pk2.Hex_partial_kite_t.unvisited_islands 
    and old_seas = pk2.Hex_partial_kite_t.unvisited_seas 
    and old_stops = pk2.Hex_partial_kite_t.stops_so_far in 
    let restricted_islands = List.filter (Hex_springboard.check_island new_sb) old_islands 
    and restricted_seas =  List.filter (fun (_,sea)->Hex_springboard.check_sea new_sb sea) old_seas  in
    let pk3 ={
      pk2 with 
        Hex_partial_kite_t.stops_so_far = 
           (List.hd old_stops)::(Hex_kite_element_t.Springboard new_sb)::(List.tl old_stops) ;
        unvisited_islands = restricted_islands ;
        unvisited_seas = restricted_seas ;
    } in 
    extend_with_sea pk3 nc2 ;;


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

let rinsed_springless_extensions partial_kite =
      let base = springless_extensions partial_kite 
      and orig_side = partial_kite.Hex_partial_kite_t.original_side in 
      let (finished1,unfinished1) =List.partition (fun (last_elt,_)->
          Hex_kite_element.is_final orig_side (Hex_kite_element.of_springless last_elt)) base in 
      let finished2 = Image.image (fun (_,pk)->
        (List.rev(pk.Hex_partial_kite_t.stops_so_far),to_molecular_linker pk)) finished1 
      and unfinished2 = Image.image snd unfinished1 in 
      (finished2,unfinished2);; 

let starters end_of_battle = 
   let sides = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   List.flatten (Image.image (starters_for_side end_of_battle) sides);;


module Springless_Search = struct 

let starters eob = 
    
      (eob.Hex_end_of_battle_t.dimension,
       eob.Hex_end_of_battle_t.winner,
       eob,
       [],[],
       starters eob);;

let late_starter eob pk= 
      (eob.Hex_end_of_battle_t.dimension,
       eob.Hex_end_of_battle_t.winner,
       eob,
       [],[],[pk]);;

let pusher (factory,_) = 
   let (d,wi,i,fi,fa,uf) = factory in 
   let raw_result=Image.image (
         fun pk->
         (pk,rinsed_springless_extensions pk) 
   ) uf in  
   let (failures1,nonfailures1) = List.partition (fun (_,p)->p=([],[]) ) raw_result in 
   let new_failures = List.rev_append (Image.image fst failures1) fa in 
   let new_moleculars = List.flatten (Image.image (fun (_,p)->fst p) nonfailures1)
   and new_partial_kites = List.flatten (Image.image (fun (_,p)->snd p) nonfailures1) in 
   let ordered_new_moleculars = Ordered.sort Total_ordering.standard2 new_moleculars in 
   let new_finished_ones = Ordered.merge Total_ordering.standard2 
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


let finalize eob pk= fst(main (late_starter eob pk,false));;

let explore eob pk (cell,nc) = 
      let nbr_of_common_steps = List.length(pk.Hex_partial_kite_t.stops_so_far) in 
      let pk1 = add_cell_by_casing eob.Hex_end_of_battle_t.dimension cell pk in 
      let pk2 = snd(extend_with_sea pk1 nc) in 
      let temp = finalize eob pk2 in 
      Image.image (fun (stops,mlclr)->
        let ttemp2 = Listennou.big_tail nbr_of_common_steps stops in 
        (Image.image Hex_kite_element.to_springless ttemp2,mlclr)
      ) temp ;;

end ;;  

let casings_from_islands eob pk = 
    let dim = eob.Hex_end_of_battle_t.dimension 
    and last_island = Hex_kite_element.claim_island(List.hd(pk.Hex_partial_kite_t.stops_so_far)) 
    and other_islands = pk.Hex_partial_kite_t.unvisited_islands in 
    let temp1 = Hex_island.short_connections dim last_island other_islands in 
    let temp2 = List.filter (fun cell->Hex_end_of_battle.assess eob cell = Hex_eob_result_t.Unoccupied) temp1 in 
    Hex_cell_set.safe_set temp2;; 

let casings_from_seas eob pk =
   let currently_added = pk.Hex_partial_kite_t.added_by_casing 
   and casings_with_hooks = pk.Hex_partial_kite_t.unvisited_seas  in 
   let unordered = Option.filter_and_unpack (
     fun (z,nc) -> 
        let d = Hex_cell_set.setminus z currently_added in 
        if Hex_cell_set.length d = 1 
        then Some(Hex_cell_set.min d)
        else None 
   ) casings_with_hooks in 
   Hex_cell_set.safe_set unordered ;; 

let minimal_casings eob pk =
   Hex_cell_set.forget_order (Hex_cell_set.merge
      (casings_from_islands eob pk)
      (casings_from_seas eob pk)
   ) ;; 

let explore_minimal_casings eob pk =
   let minimal_casings = minimal_casings eob pk in 
   let minimal_casings_with_hooks = List.flatten (
      Image.image (fun cell->
        let pk1 = add_cell_by_casing eob.Hex_end_of_battle_t.dimension cell pk in 
        let ext1 = springless_extensions pk1 in 
        Image.image (fun (elt,new_pk)->
          (cell,Hex_kite_springless_element.claim_named_connector elt)) ext1
      ) minimal_casings
   ) in 
   let first_whole = Image.image (fun p->(p,Springless_Search.explore eob pk p)) minimal_casings_with_hooks in 
   let temp1 = List.filter (fun (p,l)->l<>[]) first_whole in 
   let temp2 = List.flatten (Image.image (fun ((cell,nc),l)->
      Image.image (fun (path,solution)->(cell,path,solution) ) l
   ) temp1) in 
   (temp2,Image.image fst first_whole);;

let compute_springboards eob pk =
  let (good_casings,all_casings) = explore_minimal_casings eob pk in 
  let temp1 = Cartesian.product good_casings all_casings in 
  let temp2 = Image.image (
    fun ((cell,path,solution),(cell2,nc2))->
       (cell,path,solution,cell2,nc2)
  ) temp1  in 
  Option.filter_and_unpack Hex_springboard.opt_constructor temp2;;

end ;;

(* let extensions  = Private.rinsed_springless_extensions ;;  *)
exception Shortlived_exn ;; 
let extensions eob x = raise Shortlived_exn ;; 
let starters = Private.starters ;;