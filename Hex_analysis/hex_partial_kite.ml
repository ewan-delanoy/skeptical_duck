(* 

#use"Hex_analysis/hex_partial_kite.ml";;

*)

exception Kite_is_not_started;;

module Private = struct 

let unchecked_extend (Hex_partial_kite_t.P (l,old_support,starting_direction)) elt =
   Hex_partial_kite_t.P (elt::l,Hex_cell_set.merge (Hex_kite_element.support elt) old_support,starting_direction);;

let to_molecular_linker  l =
   (* The kite is assumed to be finished *)  
   Hex_molecular_linker.constructor (Option.filter_and_unpack Hex_kite_element.to_atomic_linker l);;



let potential_active_cells_for_side (Hex_dimension_t.D dim) side=
   Ennig.doyle (Hex_cardinal_direction.border_enumerator (Hex_dimension_t.D dim) side) 1 dim;;
   

let potential_bridges_for_side (Hex_dimension_t.D dim) side=
   let be = Hex_cardinal_direction.border_enumerator (Hex_dimension_t.D dim) side in 
   Ennig.doyle (fun k->(be k,be (k+1))) 1 (dim-1);;
   

let active_cells_for_side end_of_battle side=
    let candidates = potential_active_cells_for_side end_of_battle.Hex_end_of_battle_t.dimension side in 
    Option.filter_and_unpack (
      fun cell -> if (Hex_end_of_battle.assess end_of_battle cell) = Hex_eob_result_t.Ally_territory 
                  then Some(Hex_kite_element.active_cell cell)
                  else None
    ) candidates;;

let bridges_for_side end_of_battle side=
    let candidates = potential_bridges_for_side end_of_battle.Hex_end_of_battle_t.dimension side in 
    Option.filter_and_unpack (
      fun (cell1,cell2) -> 
                  if ((Hex_end_of_battle.assess end_of_battle cell1) = Hex_eob_result_t.Unoccupied) &&
                     ((Hex_end_of_battle.assess end_of_battle cell2) = Hex_eob_result_t.Unoccupied)   
                  then Some(Hex_kite_element.bridge (cell1,cell2))
                  else None
    ) candidates;;

let starting_elements_for_side end_of_battle side=
   (active_cells_for_side end_of_battle side)
   @(bridges_for_side end_of_battle side);;    

let starters_for_side end_of_battle side =
   Image.image (
     fun elt -> 
       Hex_partial_kite_t.P ([elt],Hex_kite_element.support elt,side)
   ) (starting_elements_for_side end_of_battle side);;

end;;

let empty_one = Hex_partial_kite_t.P ([],Hex_cell_set_t.S[],Hex_cardinal_direction_t.Down);;

let extensions dim partial_kite =
   let (Hex_partial_kite_t.P (l,old_supp,starting_direction)) =partial_kite in 
   match l with 
    []->raise(Kite_is_not_started)
   |last_elt::_->
      let candidates = Hex_kite_element.neighbors dim last_elt in 
      let retained_ones= List.filter (fun elt->
         Hex_cell_set.does_not_intersect (Hex_kite_element.support elt) old_supp
      ) candidates in
      let (finished1,unfinished1) =List.partition (Hex_kite_element.is_final (dim,starting_direction)) retained_ones in 
      let finished2 = Image.image (fun elt->Private.to_molecular_linker(elt::l)) finished1 
      and unfinished2 = Image.image (Private.unchecked_extend partial_kite) unfinished1 in 
      (finished2,unfinished2);; 


let starters end_of_battle = 
   let sides = Hex_cardinal_direction.sides_for_player end_of_battle.Hex_end_of_battle_t.winner in 
   List.flatten(Image.image (Private.starters_for_side end_of_battle) sides);;
      

