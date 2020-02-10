(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)

exception Kite_is_not_started;;

module Elderly = struct 



module Private = struct 

let initial_atom (initial_side,elt) =
   Hex_partial_kite_t.P ([elt],(Hex_kite_element.Elderly.support elt),initial_side);;


let to_molecular_linker  l =
   (* The kite is assumed to be finished *)  
   Hex_molecular_linker.fold_merge (Option.filter_and_unpack Hex_kite_element.Elderly.to_molecular_linker l);;


let unchecked_extend (Hex_partial_kite_t.P (l,old_support,initial_side)) elt =
   Hex_partial_kite_t.P (elt::l,Hex_cell_set.merge (Hex_kite_element.Elderly.support elt) old_support,initial_side);;


end;;


let extensions end_of_battle partial_kite =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let (Hex_partial_kite_t.P (l,old_supp,starting_direction)) =partial_kite in 
   match l with 
    []->raise(Kite_is_not_started)
   |last_elt::_->
      let candidates = Hex_kite_element.Elderly.neighbors_for_element dim last_elt in 
      let retained_ones= List.filter (fun elt->
         (Hex_cell_set.does_not_intersect (Hex_kite_element.Elderly.support elt) old_supp) &&
         (Hex_kite_element.Elderly.check_compatiblity end_of_battle elt)
      ) candidates in
      let (finished1,unfinished1) =List.partition (Hex_kite_element.Elderly.is_final (dim,starting_direction)) retained_ones in 
      let finished2 = Image.image (fun elt->Private.to_molecular_linker(elt::l)) finished1 
      and unfinished2 = Image.image (Private.unchecked_extend partial_kite) unfinished1 in 
      (finished2,unfinished2);; 


let starters end_of_battle = 
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let sides = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   let candidates = List.flatten(Image.image (fun side -> 
      Image.image (fun neighbor->(side,neighbor))
   (Hex_kite_element.Elderly.neighbors_for_side dim side)) sides) in 
   let retained_ones= List.filter (fun (side,elt)->
         (Hex_kite_element.Elderly.check_compatiblity end_of_battle elt)
      ) candidates in 
   Image.image Private.initial_atom retained_ones;;
      

end ;; 


module Private = struct 


let starters_for_side end_of_battle side =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let clean = List.filter (
      Hex_named_connector.check_compatiblity end_of_battle ) in 
   let base1 = clean (Hex_named_connector.starters_for_side dim side)
   and base2 = clean ((Hex_named_connector.middlers dim)@
                       (Hex_named_connector.enders_for_side dim side)
                      ) in 
   let islands = Hex_island.decompose end_of_battle in 
   let first_island = Hex_island.get_side side islands  in                    
   let constructor = (
      fun first_nc ->
        let new_base=List.filter (Hex_named_connector.check_disjointness first_nc) base2 in
        let elt1 = Hex_kite_element_t.Earth(first_island)
        and elt2 = Hex_kite_element_t.Sea(first_nc) in 
        {
            Hex_partial_kite_t.stops_so_far = [elt2;elt1];
            original_side = side ;
            unvisited_islands = List.filter (fun x->x<>first_island ) islands;
            unvisited_seas = new_base ;
        }

   ) in 
   Image.image constructor base1 ;; 

(*   
let extensions_after_island end_of_battle partial_kite last_island =
   let candidates = partial_kite.Hex_partial_kite_t.unvisited_seas in
   let retained_ones  = List.filter (
      Hex_named_connector.c    
   )  candidates in 
*)

end ;;

(*
let extensions end_of_battle partial_kite =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   match partial_kite.Hex_partial_kite_t.stops_so_far with 
    []->raise(Kite_is_not_started)
   |last_elt::_->
      let candidates = partial_kite.Hex_partial_kite_t. in 
      let retained_ones= List.filter (fun elt->
         (Hex_cell_set.does_not_intersect (Hex_kite_element.Elderly.support elt) old_supp) &&
         (Hex_kite_element.Elderly.check_compatiblity end_of_battle elt)
      ) candidates in
      let (finished1,unfinished1) =List.partition (Hex_kite_element.Elderly.is_final (dim,starting_direction)) retained_ones in 
      let finished2 = Image.image (fun elt->Private.to_molecular_linker(elt::l)) finished1 
      and unfinished2 = Image.image (Private.unchecked_extend partial_kite) unfinished1 in 
      (finished2,unfinished2);; 
*)

let starters end_of_battle = 
   let sides = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   List.flatten (Image.image (Private.starters_for_side end_of_battle) sides);;

 