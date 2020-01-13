(* 

#use"Hex_analysis/hex_old_fg_double_list.ml";;

*)


let of_concrete_object crobj=
   let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_old_fg_double_list_t.DL(
      Concrete_object_field.to_list Hex_old_finished_game.of_concrete_object arg1,
      Concrete_object_field.to_list Hex_old_finished_game.of_concrete_object arg2
   );;

let to_concrete_object (Hex_old_fg_double_list_t.DL(l1,l2))=
   Concrete_object_t.Variant("Hex_"^"fg_double_list_t.DL",
     [Concrete_object_field.of_list Hex_old_finished_game.to_concrete_object l1;
      Concrete_object_field.of_list Hex_old_finished_game.to_concrete_object l2
     ]);;

let empty_one = Hex_old_fg_double_list_t.DL([],[]);;

let add_finished_game fgame (Hex_old_fg_double_list_t.DL(l1,l2))=
   match fgame.Hex_old_finished_game_t.winner with 
   Hex_player_t.First_player -> 
        let new_l1=Hex_old_fg_list.add_finished_game fgame l1 in 
        Hex_old_fg_double_list_t.DL(new_l1,l2)
  |Hex_player_t.Second_player -> 
        let new_l2=Hex_old_fg_list.add_finished_game fgame l2 in 
        Hex_old_fg_double_list_t.DL(l1,new_l2) ;;

let remove_finished_game fgame (Hex_old_fg_double_list_t.DL(l1,l2))=
   match fgame.Hex_old_finished_game_t.winner with 
   Hex_player_t.First_player -> 
        let new_l1=Hex_old_fg_list.remove_finished_game fgame l1 in 
        Hex_old_fg_double_list_t.DL(new_l1,l2)
  |Hex_player_t.Second_player -> 
        let new_l2=Hex_old_fg_list.remove_finished_game fgame l2 in 
        Hex_old_fg_double_list_t.DL(l1,new_l2) ;;



let take_end_config_into_account end_config (Hex_old_fg_double_list_t.DL(l1,l2))=
    let new_l1=Hex_old_fg_list.take_new_end_strategy_into_account end_config l1 
    and new_l2=Hex_old_fg_list.take_new_end_strategy_into_account end_config l2 in  
    Hex_old_fg_double_list_t.DL(new_l1,new_l2) ;;

let simplify_by_move move (Hex_old_fg_double_list_t.DL(l1,l2))=
   (Hex_old_fg_double_list_t.DL(
      Hex_old_fg_list.simplify_by_move move l1,
      Hex_old_fg_list.simplify_by_move move l2
    ));;    

let first_moves player (Hex_old_fg_double_list_t.DL(l1,l2))=
   match player with 
   Hex_player_t.First_player -> Hex_old_fg_list.first_moves l1
  |Hex_player_t.Second_player -> Hex_old_fg_list.first_moves l2;;

let familiar_moves player (Hex_old_fg_double_list_t.DL(l1,l2))=
   match player with 
   Hex_player_t.First_player -> Hex_old_fg_list.first_moves l2
  |Hex_player_t.Second_player -> Hex_old_fg_list.first_moves l1;;

let lookup_dimension (Hex_old_fg_double_list_t.DL(l1,l2))=
  match Hex_old_fg_list.lookup_dimension l1 with 
  Some(dim)->Some(dim)
  |None -> Hex_old_fg_list.lookup_dimension l2 ;;

let seek_companions 
  (Hex_old_fg_double_list_t.DL(games1,games2)) 
   (Hex_fles_double_list_t.DL(flesses1,flesses2))=
   (
      Hex_old_fg_list.seek_companions_for_games games1 flesses1,
      Hex_old_fg_list.seek_companions_for_games games2 flesses2,
      Hex_old_fg_list.seek_companions_for_strategies flesses1 games1,
      Hex_old_fg_list.seek_companions_for_strategies flesses2 games2 
   );;

let check_companions  fg_dl fles_dl = 
   let (a1,a2,a3,a4) = seek_companions fg_dl fles_dl 
   and finder =(fun l->Option.filter_and_unpack (
       fun (x,opt)->match opt with 
        None -> Some(x)
       | _ ->None
   ) l) in 
   (finder a1,finder a2,finder a3,finder a4);;

 


