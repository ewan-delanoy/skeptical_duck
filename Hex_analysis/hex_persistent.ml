(* 

#use"Hex_analysis/hex_persistent.ml";;

*)

let games_ref = ref (Hex_fg_double_list.empty_one);;

let winning_end_strategies_for_first_player_ref 
      = ref (Hex_end_strategy_factory.empty_one Hex_player_t.First_player);;

let winning_end_strategies_for_second_player_ref 
      = ref (Hex_end_strategy_factory.empty_one Hex_player_t.Second_player);;

let untamed_openings_ref = ref [];;
exception Dimension_could_not_be_found;;      

let data_has_been_initialized_already=ref(false);;

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



let persist_strategies ()=
   (
     Io.overwrite_with path_for_fp_strats (Hex_end_strategy_factory.to_string (fst wes_pair));
     Io.overwrite_with path_for_sp_strats (Hex_end_strategy_factory.to_string (snd wes_pair));
   );;


let persist_games ()=
     Io.overwrite_with path_for_fgames (Crobj_parsing.unparse(Hex_fg_double_list.to_concrete_object (!games_ref)));;   



let reset_all_to_empty ()=
   let _=(
    games_ref := (Hex_fg_double_list.empty_one);
    fst(wes_pair) :=  (Hex_end_strategy_factory.empty_one Hex_player_t.First_player);
    snd(wes_pair) :=  (Hex_end_strategy_factory.empty_one Hex_player_t.Second_player);
    untamed_openings_ref := []) in 
    (* (persist_games();persist_strategies()) *)
    ();;


let dimension_ref = ref(11);;

let compute_dim_the_first_time ()=
   match Hex_fg_double_list.lookup_dimension (!games_ref) with 
   None->(!dimension_ref) (* raise(Dimension_could_not_be_found) *)
   |Some(dim)->dim;;



let initialize_all_data_if_necessary ()=
  if (!data_has_been_initialized_already)
  then ()
  else 
    let new_games = (Hex_fg_double_list.of_concrete_object(Crobj_parsing.parse(Io.read_whole_file path_for_fgames))) in 
  (
    Hex_end_strategy_factory.fill_with_string (fst(wes_pair)) (Io.read_whole_file path_for_fp_strats);
    Hex_end_strategy_factory.fill_with_string (snd(wes_pair)) (Io.read_whole_file path_for_sp_strats);
    games_ref:=new_games;
    untamed_openings_ref:=(Hex_uog_list.extract_untamed_openings new_games);
    dimension_ref:=compute_dim_the_first_time();
    data_has_been_initialized_already:=true;
  );;


let add_end_strategy_without_persisting (player,static_constructor,comment,indices) =
   let fles = Hex_end_strategy_factory.create_new_strategy true wes_pair player static_constructor comment indices in 
   let new_games = Hex_fg_double_list.take_end_config_into_account fles (!games_ref) in 
   (
    games_ref:=new_games;
    untamed_openings_ref:=(Hex_uog_list.extract_untamed_openings new_games);
   );;


let last_game_added=ref(Hex_finished_game.empty_one);;


let add_finished_game_without_persisting fgame =
    
    let checked_fgame=Hex_fles_double_list.iterated_largest_unconclusive_beginning 
             fgame (Hex_end_strategy_factory.compute_all_end_configs wes_pair) in 
    let _=(last_game_added:=checked_fgame) in 
    let new_games = Hex_fg_double_list.add_finished_game checked_fgame (!games_ref) in 
   (
    games_ref:=new_games;
    untamed_openings_ref:=(Hex_uog_list.extract_untamed_openings new_games);
   );;

let remove_last_finished_game_without_persisting () =
    let fgame=(!last_game_added) in 
    let new_games = Hex_fg_double_list.remove_finished_game fgame (!games_ref) in 
   (
    games_ref:=new_games;
    untamed_openings_ref:=(Hex_uog_list.extract_untamed_openings new_games);
   );;

let add_end_strategy fles =
   (
     add_end_strategy_without_persisting fles;
     persist_strategies();
     persist_games()
   );;

let add_finished_game fgame =
   (
     add_finished_game_without_persisting fgame;
     persist_games()
   );;

let remove_last_finished_game fgame =
   (
     remove_last_finished_game_without_persisting fgame;
     persist_games()
   );;


end ;;

let add_end_strategy = Private.add_end_strategy;;
let add_finished_game = Private.add_finished_game;;
let cancel_last_game = Private.remove_last_finished_game;;
let dimension ()=(!(Private.dimension_ref));;
let reset_all_to_empty = Private.reset_all_to_empty;;
let initialize_all_data_if_necessary = Private.initialize_all_data_if_necessary;;
let wes_pair = Private.wes_pair;;



