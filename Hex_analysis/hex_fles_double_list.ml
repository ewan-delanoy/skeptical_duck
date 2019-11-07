(* 

#use"Hex_analysis/hex_ec_double_indexed_list.ml";;

*)

let empty_one = Hex_fles_double_list_t.DL([],[]);;


let simplify_by_move (player,cell) (Hex_fles_double_list_t.DL(l1,l2))=
  match player with 
   Hex_player_t.First_player -> 
      Hex_fles_double_list_t.DL
      (Hex_flattened_end_strategy.use_ally_move_to_simplify_several cell l1,
       Hex_flattened_end_strategy.use_enemy_move_to_simplify_several cell l2)
  |Hex_player_t.Second_player -> 
      Hex_fles_double_list_t.DL
      (Hex_flattened_end_strategy.use_ally_move_to_simplify_several cell l1,
       Hex_flattened_end_strategy.use_enemy_move_to_simplify_several cell l2)
     ;; 
    
let immediate_dangers player (Hex_fles_double_list_t.DL(l1,l2))=
  match player with 
   Hex_player_t.First_player -> 
      Hex_flattened_end_strategy.immediate_opportunities l2
  |Hex_player_t.Second_player -> 
      Hex_flattened_end_strategy.immediate_opportunities l1;; 
    
let iterated_largest_unconclusive_beginning fgame (Hex_fles_double_list_t.DL(l1,l2))=
    Hex_finished_game.iterated_largest_unconclusive_beginning fgame (l1@l2);;

let sizes (Hex_fles_double_list_t.DL(l1,l2))=(List.length l1,List.length l2);;

let number_of_enemy_strategies player (Hex_fg_double_list_t.DL(l1,l2))=
   let l=(match player with 
   Hex_player_t.First_player ->l2
  |Hex_player_t.Second_player ->l1) in  
    List.length l;; 

let of_concrete_object crobj=
   let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_fles_double_list_t.DL(
      Concrete_object_field.to_list Hex_flattened_end_strategy.of_concrete_object arg1,
      Concrete_object_field.to_list Hex_flattened_end_strategy.of_concrete_object arg2
   );;

let to_concrete_object (Hex_fles_double_list_t.DL(l1,l2))=
   Concrete_object_t.Variant("Hex_"^"fles_double_list_t.DL",
     [Concrete_object_field.of_list Hex_flattened_end_strategy.to_concrete_object l1;
      Concrete_object_field.of_list Hex_flattened_end_strategy.to_concrete_object l2
     ]);;


