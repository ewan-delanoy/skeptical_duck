(* 

#use"Hex_analysis/hex_fg_double_list.ml";;

*)


let of_concrete_object crobj=
   let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_fg_double_list_t.DL(
      Concrete_object_field.to_list Hex_finished_game.of_concrete_object arg1,
      Concrete_object_field.to_list Hex_finished_game.of_concrete_object arg2
   );;

let to_concrete_object (Hex_fg_double_list_t.DL(l1,l2))=
   Concrete_object_t.Variant("Hex_"^"fg_double_list_t.DL",
     [Concrete_object_field.of_list Hex_finished_game.to_concrete_object l1;
      Concrete_object_field.of_list Hex_finished_game.to_concrete_object l2
     ]);;

let empty_one = Hex_fg_double_list_t.DL([],[]);;

let add_finished_game fgame (Hex_fg_double_list_t.DL(l1,l2))=
   match fgame.Hex_finished_game_t.winner with 
   Hex_player_t.First_player -> 
        let new_l1=Hex_fg_list.add_finished_game fgame l1 in 
        Hex_fg_double_list_t.DL(new_l1,l2)
  |Hex_player_t.Second_player -> 
        let new_l2=Hex_fg_list.add_finished_game fgame l2 in 
        Hex_fg_double_list_t.DL(l1,new_l2) ;;

let take_end_config_into_account end_config (Hex_fg_double_list_t.DL(l1,l2))=
    let new_l1=Hex_fg_list.take_new_end_strategy_into_account end_config l1 
    and new_l2=Hex_fg_list.take_new_end_strategy_into_account end_config l2 in  
    Hex_fg_double_list_t.DL(new_l1,new_l2) ;;

let simplify_by_move move (Hex_fg_double_list_t.DL(l1,l2))=
   (Hex_fg_double_list_t.DL(
      Hex_fg_list.simplify_by_move move l1,
      Hex_fg_list.simplify_by_move move l2
    ));;    

let suggested_moves player (Hex_fg_double_list_t.DL(l1,l2))=
   match player with 
   Hex_player_t.First_player -> 
       (Hex_fg_list.first_moves l1,Hex_fg_list.first_moves l2)
  |Hex_player_t.Second_player -> 
       (Hex_fg_list.first_moves l2,Hex_fg_list.first_moves l1);;

let lookup_dimension (Hex_fg_double_list_t.DL(l1,l2))=
  match Hex_fg_list.lookup_dimension l1 with 
  Some(dim)->Some(dim)
  |None -> Hex_fg_list.lookup_dimension l2 ;;


 


