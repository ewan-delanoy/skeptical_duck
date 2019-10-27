(* 

#use"Hex_analysis/hex_persistent.ml";;

*)

let games_ref = ref (Hex_fg_double_list.empty_one);;

let winning_end_strategies_for_first_player_ref 
      = ref (Hex_end_strategy_factory.empty_one Hex_player_t.First_player);;

let winning_end_strategies_for_second_player_ref 
      = ref (Hex_end_strategy_factory.empty_one Hex_player_t.Second_player);;

let strong_openings_ref = ref [];;
      

module Private = struct 



let wes_pair = 
   (
     winning_end_strategies_for_first_player_ref,
     winning_end_strategies_for_second_player_ref
   );;


let fst_player_strat_at_idx k=Hex_end_strategy_factory.get_elt_at_idx 
       winning_end_strategies_for_first_player_ref k;;

let snd_player_strat_at_idx k=Hex_end_strategy_factory.get_elt_at_idx 
       winning_end_strategies_for_second_player_ref k;;       


let path_for_fp_strats = Absolute_path.of_string "Hex_analysis/hex_end_strategies_for_first_player.txt";;
let path_for_sp_strats = Absolute_path.of_string "Hex_analysis/hex_end_strategies_for_second_player.txt";;
let path_for_fgames =   Absolute_path.of_string "Hex_analysis/hex_finished_games.txt";;
let path_for_openings = Absolute_path.of_string "Hex_analysis/hex_boomerangs.txt";;


let persist_strategies ()=
   (
     Io.overwrite_with path_for_fp_strats (Hex_end_strategy_factory.to_string (fst wes_pair));
     Io.overwrite_with path_for_sp_strats (Hex_end_strategy_factory.to_string (snd wes_pair));
   );;


let persist_games ()=
     Io.overwrite_with path_for_fgames (Hex_fg_double_list.to_string (!games_ref));;   

let persist_openings ()=
    Io.overwrite_with path_for_openings (Hex_so_list.to_string(!strong_openings_ref));;

let reset_all_to_empty ()=
   let _=(
    games_ref := (Hex_fg_double_list.empty_one);
    fst(wes_pair) :=  (Hex_end_strategy_factory.empty_one Hex_player_t.First_player);
    snd(wes_pair) :=  (Hex_end_strategy_factory.empty_one Hex_player_t.Second_player);
    strong_openings_ref := []) in 
    (persist_games();persist_strategies();persist_openings())
    ;;

let data_has_been_initialized_already=ref(false);;

let initialize_all_data_if_necessary ()=
  if (!data_has_been_initialized_already)
  then ()
  else 
  (
    Hex_end_strategy_factory.fill_with_string (fst(wes_pair)) (Io.read_whole_file path_for_fp_strats);
    Hex_end_strategy_factory.fill_with_string (snd(wes_pair)) (Io.read_whole_file path_for_sp_strats);
    games_ref:=(Hex_fg_double_list.of_string(Io.read_whole_file path_for_fgames));
    strong_openings_ref:=(Hex_so_list.of_string(Io.read_whole_file path_for_openings));
    data_has_been_initialized_already:=true;
  );;


let add_end_strategy_without_persisting (player,static_constructor,comment,indices) =
   let ec = Hex_end_strategy_factory.create_new_strategy wes_pair player static_constructor comment indices in 
   (
    games_ref:=Hex_fg_double_list.take_end_config_into_account ec (!games_ref)
   );;


let add_finished_game_without_persisting fgame =
    let checked_fgame=Hex_fles_double_list.iterated_largest_unconclusive_beginning 
             fgame (Hex_end_strategy_factory.compute_all_end_configs wes_pair) in 
   (
    games_ref:=Hex_fg_double_list.add_finished_game checked_fgame (!games_ref)
   );;

let add_strong_opening_without_persisting opng=
   let old_ones=(!strong_openings_ref) in 
   let new_ones = Hex_so_list.insert_in opng old_ones in 
   strong_openings_ref := new_ones;;

let add_end_strategy ec =
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

let add_strong_opening opng=
   (
     add_strong_opening_without_persisting opng;
     persist_openings()
   );;

end ;;

let add_end_strategy = Private.add_end_strategy;;
let add_finished_game = Private.add_finished_game;;
let add_strong_opening = Private.add_strong_opening;;
let reset_all_to_empty = Private.reset_all_to_empty;;
let initialize_all_data_if_necessary = Private.initialize_all_data_if_necessary;;
let wes_pair = Private.wes_pair;;



