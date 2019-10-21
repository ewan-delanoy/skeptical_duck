(* 

#use"Hex_analysis/hex_persistent.ml";;

*)




let games_ref = ref (Hex_fg_double_list.empty_one);;

let winning_end_strategies_for_first_player_ref 
      = ref (Hex_strategy_factory.empty_one Hex_player_t.First_player);;

let winning_end_strategies_for_second_player_ref 
      = ref (Hex_strategy_factory.empty_one Hex_player_t.Second_player);;

let wes_pair = 
   (
     winning_end_strategies_for_first_player_ref,
     winning_end_strategies_for_second_player_ref
   );;

let persist_strategies ()=
    let assignment1=
      "\n\n\n Hex_"^"strategy_factory.fill_with_string Hex_"^"persistent.winning_end_strategies_for_first_player_ref \n"^
      "\n(\""^
      (Hex_strategy_factory.to_string (fst wes_pair))^"\");;\n\n\n" 
    and assignment2=
      "\n\n\n Hex_"^"strategy_factory.fill_with_string Hex_"^"persistent.winning_end_strategies_for_second_player_ref \n"^
      "\n(\""^
      (Hex_strategy_factory.to_string (snd wes_pair))^"\");;\n\n\n" in 
    let assignment = assignment1 ^ "\n\n\n" ^ assignment2 in    
    let ap=Absolute_path.of_string "Hex_analysis/hex_initializer.ml" in 
    Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string assignment) ("(* End strategies start here *)","(* End strategies end here *)") ap;;



let persist_games ()=
    let assignment=
      "\n\n\n Hex_"^"persistent.games_ref:=Hex_"^"fg_double_list.of_string(\n\""^
      (Hex_fg_double_list.to_string (!games_ref))^"\");;\n\n\n" in 
    let ap=Absolute_path.of_string "Hex_analysis/hex_initializer.ml" in 
    Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string assignment) ("(* Games start here *)","(* Games end here *)") ap;;


let add_end_strategy_without_persisting (player,static_constructor,indices) =
   let ec = Hex_strategy_factory.create_new_strategy wes_pair player static_constructor indices in 
   (
    games_ref:=Hex_fg_double_list.take_end_config_into_account ec (!games_ref)
   );;



let add_finished_game_without_persisting fgame =
    let checked_fgame=Hex_ec_double_indexed_list.iterated_largest_unconclusive_beginning fgame (!configs_ref) in 
   (
    games_ref:=Hex_fg_double_list.add_finished_game checked_fgame (!games_ref)
   );;

let add_end_config ec =
   (
     add_end_strategy_without_persisting ec;
     persist_strategies();
     persist_games()
   );;

let add_finished_game fgame =
   (
     add_finished_game_without_persisting fgame;
     persist_games()
   );;

