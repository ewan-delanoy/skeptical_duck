(* 

#use"Hex_analysis/hex_fles_double_list.ml";;

*)

exception Dimension_exn ;;

let dimension (Hex_fles_double_list_t.DL(l1,l2))=
   if l1<>[] then (List.hd l1).Hex_flattened_end_strategy_t.dimension else 
   if l2<>[] then (List.hd l1).Hex_flattened_end_strategy_t.dimension else 
   raise(Dimension_exn);; 

let empty_one = Hex_fles_double_list_t.DL([],[]);;

let enemy_strategies player (Hex_fles_double_list_t.DL(l1,l2))=
  match player with 
   Hex_player_t.First_player ->  l2
  |Hex_player_t.Second_player -> l1;;
      


let immediate_dangers player dl=
    Hex_flattened_end_strategy.immediate_opportunities (enemy_strategies player dl);;


let test_for_surrender (player,cell) dl=
   let (cells,indices,mandatory_compound) = immediate_dangers player dl in 
   let condition = Hex_mandatory_compound.global_escape_set mandatory_compound in 
   match condition with 
   None->None
   |Some(full_set) -> 
               if not(Hex_cell_set.mem cell full_set) 
               then Some(indices)
               else None;;


exception Surrender of Hex_player_t.t * Hex_cell_t.t * (int list) ;; 

let simplify_by_move (player,cell) dl=
  match test_for_surrender (player,cell) dl with 
  Some(killers)->raise(Surrender(player,cell,killers))
  |None->
  let  (Hex_fles_double_list_t.DL(l1,l2))= dl in 
  let (new_l1,new_l2)=
  (match player with 
   Hex_player_t.First_player -> 
      (Hex_flattened_end_strategy.use_ally_move_to_simplify_several cell l1,
       Hex_flattened_end_strategy.use_enemy_move_to_simplify_several cell l2)
  |Hex_player_t.Second_player -> 
      (Hex_flattened_end_strategy.use_enemy_move_to_simplify_several cell l1,
       Hex_flattened_end_strategy.use_ally_move_to_simplify_several cell l2)
     ) in  
  Hex_fles_double_list_t.DL(new_l1,new_l2)      ;; 
    

let iterated_largest_nonsurrendering_beginning fgame (Hex_fles_double_list_t.DL(l1,l2))=
    Hex_finished_game.largest_nonsurrendering_beginning fgame (l1@l2);;


let number_of_enemy_strategies player (Hex_fles_double_list_t.DL(l1,l2))=
   let l=(match player with 
   Hex_player_t.First_player ->l2
  |Hex_player_t.Second_player ->l1) in  
    List.length l;; 

let of_concrete_object crobj=
   let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
   Hex_fles_double_list_t.DL(
      Crobj_converter_combinator.to_list Hex_flattened_end_strategy.of_concrete_object arg1,
      Crobj_converter_combinator.to_list Hex_flattened_end_strategy.of_concrete_object arg2
   );;

let to_concrete_object (Hex_fles_double_list_t.DL(l1,l2))=
   Concrete_object_t.Variant("Hex_"^"fles_double_list_t.DL",
     [Crobj_converter_combinator.of_list Hex_flattened_end_strategy.to_concrete_object l1;
      Crobj_converter_combinator.of_list Hex_flattened_end_strategy.to_concrete_object l2
     ]);;


