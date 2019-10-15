(* 

#use"Hex_analysis/hex_persistent.ml";;

*)


let configs_ref = ref (Hex_ec_double_indexed_list.empty_one);;

let games_ref = ref (Hex_fg_double_list.empty_one);;

let persist_configs ()=
    let assignment=
      "\n\n\n Hex_"^"persistent.configs_ref:=Hex_"^"ec_double_list.of_string(\n\""^
      (Hex_ec_double_indexed_list.to_string (!configs_ref))^"\");;\n\n\n" in 
    let ap=Absolute_path.of_string "Hex_analysis/hex_initializer.ml" in 
    Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string assignment) ("(* Configs start here *)","(* Configs end here *)") ap;;

let persist_games ()=
    let assignment=
      "\n\n\n Hex_"^"persistent.games_ref:=Hex_"^"fg_double_list.of_string(\n\""^
      (Hex_fg_double_list.to_string (!games_ref))^"\");;\n\n\n" in 
    let ap=Absolute_path.of_string "Hex_analysis/hex_initializer.ml" in 
    Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string assignment) ("(* Games start here *)","(* Games end here *)") ap;;


let add_end_config_without_persisting ec =
   (
    configs_ref:=Hex_ec_double_indexed_list.add_end_config ec (!configs_ref);
    games_ref:=Hex_fg_double_list.take_end_config_into_account ec (!games_ref)
   );;

let add_finished_game_without_persisting fgame =
    let checked_fgame=Hex_ec_double_indexed_list.iterated_largest_unconclusive_beginning fgame (!configs_ref) in 
   (
    games_ref:=Hex_fg_double_list.add_finished_game checked_fgame (!games_ref)
   );;

let add_end_config ec =
   (
     add_end_config_without_persisting ec;
     persist_configs();
     persist_games()
   );;

let add_finished_game fgame =
   (
     add_finished_game_without_persisting fgame;
     persist_games()
   );;

