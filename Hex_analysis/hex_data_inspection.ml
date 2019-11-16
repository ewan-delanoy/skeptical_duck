(* 

#use"Hex_analysis/hex_data_inspection.ml";;

*)




let best_fits_for_strategy fles =
  let (Hex_fg_double_list_t.DL(l1,l2))=(!(Hex_persistent.games_ref)) in 
  let fgames = (match fles.Hex_flattened_end_strategy_t.beneficiary with 
      Hex_player_t.First_player -> l1
     |Hex_player_t.Second_player -> l2
  ) in 
  Hex_finished_game.best_fits_for_strategy fles fgames;;

let relevancies ()=
   Hex_fg_double_list.relevancies 
    (!(Hex_persistent.games_ref)) 
   (Hex_end_strategy_factory.compute_all_end_configs Hex_persistent.wes_pair);;

let check_relevancies ()=
   Hex_fg_double_list.check_relevancies 
    (!(Hex_persistent.games_ref)) 
   (Hex_end_strategy_factory.compute_all_end_configs Hex_persistent.wes_pair);;
