(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)

exception Kite_is_not_started;;

module Private = struct 

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
   Image.image constructor base1 ;; 

let to_molecular_linker  pk =
   (* The kite is assumed to be finished *)  
   Hex_molecular_linker.fold_merge 
     (Option.filter_and_unpack Hex_kite_element.to_molecular_linker pk.Hex_partial_kite_t.stops_so_far);;



let extend_with_island pk new_island = 
        let new_elt = Hex_kite_element_t.Earth(new_island) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.stops_so_far = new_elt :: (pk.Hex_partial_kite_t.stops_so_far);
          unvisited_islands = List.filter (fun x->x<>new_island ) 
             (pk.Hex_partial_kite_t.unvisited_islands);
    });;
    

let extend_with_sea pk new_nc = 
        let new_elt = Hex_kite_element_t.Sea(new_nc) in 
     (new_elt,   
     {
         pk with 
          Hex_partial_kite_t.stops_so_far = new_elt :: (pk.Hex_partial_kite_t.stops_so_far);
            unvisited_seas = List.filter 
              (fun (z,nc)->
                Hex_named_connector.check_disjointness new_nc nc) 
                (pk.Hex_partial_kite_t.unvisited_seas);
    });;



let extensions_after_island partial_kite last_island =
   let candidates = partial_kite.Hex_partial_kite_t.unvisited_seas in
   let retained_ones  = Option.filter_and_unpack (
      fun (z,nc)->
        if (Hex_named_connector.check_entry last_island nc)&&(Hex_cell_set.length z=0) 
        then Some nc 
        else None   
   )  candidates in 
   Image.image (extend_with_sea partial_kite) retained_ones ;;

let extensions_after_sea partial_kite last_nc =
   let candidates = partial_kite.Hex_partial_kite_t.unvisited_islands in
   let retained_ones  = List.filter (
      Hex_named_connector.check_exit last_nc  
   )  candidates in 
   Image.image (extend_with_island partial_kite) retained_ones ;;

let extensions partial_kite = function 
    Hex_kite_element_t.Earth(last_island) ->  extensions_after_island partial_kite last_island 
   |Hex_kite_element_t.Sea(last_nc) ->  extensions_after_sea partial_kite last_nc ;;

end ;;


let extensions partial_kite =
   match partial_kite.Hex_partial_kite_t.stops_so_far with 
    []->raise(Kite_is_not_started)
   |last_elt::_->
      let base = Private.extensions partial_kite last_elt 
      and orig_side = partial_kite.Hex_partial_kite_t.original_side in 
      let (finished1,unfinished1) =List.partition (fun (last_elt,_)->
          Hex_kite_element.is_final orig_side last_elt) base in 
      let finished2 = Image.image (fun (_,pk)->Private.to_molecular_linker pk) finished1 
      and unfinished2 = Image.image snd unfinished1 in 
      (finished2,unfinished2);; 


let starters end_of_battle = 
   let sides = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   List.flatten (Image.image (Private.starters_for_side end_of_battle) sides);;

 