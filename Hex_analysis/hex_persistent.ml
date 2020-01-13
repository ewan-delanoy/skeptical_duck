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


let dimension_ref = ref(Hex_dimension.eleven);;

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

let remove_strats_with_indices_without_persisting (l1_idx,l2_idx) =
   (
    Hex_end_strategy_factory.remove_strats_with_indices (fst wes_pair) l1_idx;
    Hex_end_strategy_factory.remove_strats_with_indices (snd wes_pair) l2_idx;
   );;

let last_game_added=ref(Hex_finished_game.empty_one);;


let add_finished_game_without_persisting fgame =
    
    let checked_fgame=Hex_fles_double_list.iterated_largest_nonsurrendering_beginning
             fgame (Hex_end_strategy_factory.compute_all_end_configs wes_pair) in 
    let _=(last_game_added:=checked_fgame) in 
    let new_games = Hex_fg_double_list.add_finished_game checked_fgame (!games_ref) in 
    let old_untamed_openings =(!untamed_openings_ref) 
    and candidate = Hex_uog_list.compute_maximal_jockeyed_opening checked_fgame new_games in 
    let s_candidate = (Hex_untamed_opening.to_readable_string candidate)  in 
    let msg = (if not(List.mem candidate old_untamed_openings) 
           then "New untamed : "^s_candidate^"\n" 
           else "Jockeying on : "^s_candidate^" (Nothing new)\n" ) in 
    let _=(print_string msg; flush stdout) in        
   (
    games_ref:=new_games;
    untamed_openings_ref:=(Hex_uog_list.extract_untamed_openings new_games);
   );;

let remove_finished_game_without_persisting fgame =
    let new_games = Hex_fg_double_list.remove_finished_game fgame (!games_ref) in 
   (
    games_ref:=new_games;
    untamed_openings_ref:=(Hex_uog_list.extract_untamed_openings new_games);
   );;


let remove_last_finished_game_without_persisting () =
    remove_finished_game_without_persisting (!last_game_added);;
    

let add_end_strategy fles =
   (
     add_end_strategy_without_persisting fles;
     persist_strategies();
     persist_games()
   );;

let remove_strats_with_indices pair =
   (
      remove_strats_with_indices_without_persisting pair;
      persist_strategies();
   );;

let add_finished_game fgame =
   (
     add_finished_game_without_persisting fgame;
     persist_games()
   );;

let remove_finished_game fgame =
   (
     remove_finished_game_without_persisting fgame;
     persist_games()
   );;


let remove_last_finished_game () =
   (
     remove_last_finished_game_without_persisting ();
     persist_games()
   );;


end ;;

let add_end_strategy = Private.add_end_strategy;;
let add_finished_game = Private.add_finished_game;;
let cancel_last_game = Private.remove_last_finished_game;;
let dimension ()=(!(Private.dimension_ref));;
let remove_strats_with_indices = Private.remove_strats_with_indices;;
let reset_all_to_empty = Private.reset_all_to_empty;;
let initialize_all_data_if_necessary = Private.initialize_all_data_if_necessary;;
let wes_pair = Private.wes_pair;;



