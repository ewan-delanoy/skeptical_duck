(* 

#use"Hex_analysis/hex_secondary_persistent.ml";;

Use this module when you need to experiment with
the persistent values from the Hex_persistent module.

Call push before the experiment, and pop after it

*)


module Private = struct

let games_ref = ref (Hex_fg_double_list.empty_one);;

let winning_end_strategies_for_first_player_ref 
      = ref (Hex_end_strategy_factory.empty_one Hex_dimension.eleven Hex_player_t.First_player);;

let winning_end_strategies_for_second_player_ref 
      = ref (Hex_end_strategy_factory.empty_one Hex_dimension.eleven Hex_player_t.Second_player);;

let untamed_openings_ref = ref [];;

let pop () = 
    Hex_persistent.set_state 
    (
    (!games_ref),
    (!winning_end_strategies_for_first_player_ref),
    (!winning_end_strategies_for_second_player_ref),
    (!untamed_openings_ref)
  );;

let push () =
    let (g,w1,w2,uops) = Hex_persistent.current_state () in 
    (
     games_ref := g;
     winning_end_strategies_for_first_player_ref := w1 ;
     winning_end_strategies_for_second_player_ref := w2 ;
     untamed_openings_ref := uops ;
   );;   

end ;;

let pop = Private.pop ;;
let push = Private.push ;;