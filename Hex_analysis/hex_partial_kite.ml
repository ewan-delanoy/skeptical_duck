(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)

exception Kite_is_not_started;;

module Private = struct 

let initial_atom (initial_side,elt) elt =
   Hex_partial_kite_t.P ([elt],(Hex_kite_element.support elt),initial_side);;


let to_molecular_linker  l =
   (* The kite is assumed to be finished *)  
   Hex_molecular_linker.fold_merge (Option.filter_and_unpack Hex_kite_element.to_molecular_linker l);;


let unchecked_extend (Hex_partial_kite_t.P (l,old_support,initial_side)) elt =
   Hex_partial_kite_t.P (elt::l,Hex_cell_set.merge (Hex_kite_element.support elt) old_support,initial_side);;


end;;


let extensions end_of_battle partial_kite =
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let (Hex_partial_kite_t.P (l,old_supp,starting_direction)) =partial_kite in 
   match l with 
    []->raise(Kite_is_not_started)
   |last_elt::_->
      let candidates = Hex_kite_element.neighbors_for_element dim last_elt in 
      let retained_ones= List.filter (fun elt->
         (Hex_cell_set.does_not_intersect (Hex_kite_element.support elt) old_supp) &&
         (Hex_kite_element.check_compatiblity end_of_battle elt)
      ) candidates in
      let (finished1,unfinished1) =List.partition (Hex_kite_element.is_final (dim,starting_direction)) retained_ones in 
      let finished2 = Image.image (fun elt->Private.to_molecular_linker(elt::l)) finished1 
      and unfinished2 = Image.image (Private.unchecked_extend partial_kite) unfinished1 in 
      (finished2,unfinished2);; 


let starters end_of_battle = 
   let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
   let sides = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   let candidates = List.flatten(Image.image (fun side -> 
      Image.image (fun neighbor->(side,neighbor))
   (Hex_kite_element.neighbors_for_side dim side)) sides) in 
   let retained_ones= List.filter (fun (side,elt)->
         (Hex_kite_element.check_compatiblity end_of_battle elt)
      ) candidates in 
   Image.image Private.initial_atom retained_ones;;
      

