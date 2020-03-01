(* 

#use"Hex_analysis/hex_finished_game.ml";;

*)

module Private = struct


let empty_one = {
   Hex_finished_game_t.dimension = Hex_dimension.eleven;
   winner = Hex_player_t.First_player;
   sequence_of_moves = [];
};;


let ref_for_lnb_computations = ref [];;


let largest_nonsurrendering_beginning_for_one fgame fles = 
    match Hex_meeting_result.meet fles fgame with 
     Hex_meeting_result_t.Player_surrenders(moves_before,_,_)->
       let d=List.length(fgame.Hex_finished_game_t.sequence_of_moves)-List.length(moves_before) 
       and color = Hex_player.color(Hex_flattened_end_strategy_field.beneficiary fles) 
       and s_idx = string_of_int(Hex_flattened_end_strategy_field.index fles) in 
       let msg = "Surrender to "^color^" strategy number "^s_idx^" detected, "^
                 (string_of_int d)^" moves deleted\n" in 
       let new_fgame = {fgame with Hex_finished_game_t.sequence_of_moves = moves_before} in           
       let _=(ref_for_lnb_computations:=(fgame,fles,new_fgame)::(!ref_for_lnb_computations)) in           
       let _=(print_string msg;flush stdout) in 
       new_fgame
    |_->fgame;;

let largest_nonsurrendering_beginning_for_several fgame flesses =
   List.fold_left largest_nonsurrendering_beginning_for_one fgame flesses;;     


let extends fgame1 fgame2=
   Listennou.extends 
     fgame1.Hex_finished_game_t.sequence_of_moves 
     fgame2.Hex_finished_game_t.sequence_of_moves ;;


let simplify_by_move new_move fgame=match fgame.Hex_finished_game_t.sequence_of_moves with 
  []->None
  |first_move::other_moves ->
    if (first_move = new_move)&&(other_moves <> [])
    then Some({fgame with Hex_finished_game_t.sequence_of_moves = other_moves})
    else None;;

let first_move fgame = List.hd(fgame.Hex_finished_game_t.sequence_of_moves);;

let partial_unveil fgame=
  (
     fgame.Hex_finished_game_t.winner,
     fgame.Hex_finished_game_t.sequence_of_moves
  );;

let cmp = 
  let cmp_for_cell_lists = Total_ordering.lex_compare Hex_cell.cmp in 
 ((fun fgame1 fgame2 ->
   (Total_ordering.product 
     Total_ordering.standard
     cmp_for_cell_lists)
   (partial_unveil fgame1) (partial_unveil fgame2)  
) :> Hex_finished_game_t.t Total_ordering.t);;

let seek_companion_for_strategy fles fgames =
   Option.find_and_stop (
     fun fgame -> match Hex_meeting_result.meet fles fgame with
       Hex_meeting_result_t.Player_is_attacked_but_fights_back(_,_,_)->Some(fgame)
       |_->None
   ) fgames ;;

let seek_companion_for_game fgame flesses =
  Option.find_and_stop (
     fun fles -> match Hex_meeting_result.meet fles fgame with
       Hex_meeting_result_t.Player_is_attacked_but_fights_back(moves_before,_,nbr_of_moves_remaining)->
           if nbr_of_moves_remaining = 0 
           then Some(fles)
           else None
       |_->None
   ) flesses ;;

let enumerate_companions_for_game fgame flesses =
  List.filter (
     fun fles -> match Hex_meeting_result.meet fles fgame with
       Hex_meeting_result_t.Player_is_attacked_but_fights_back(moves_before,_,nbr_of_moves_remaining)->
           nbr_of_moves_remaining = 0 
       |_-> false
   ) flesses ;;

let salt = "Hex_"^"finished_game_t.";;

let dimension_label          = salt ^ "dimension";;
let winner_label             = salt ^ "winner";;
let sequences_of_moves_label = salt ^ "sequences_of_moves";;



let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Hex_finished_game_t.dimension = Hex_dimension.of_concrete_object (g dimension_label);
      winner = Hex_player.of_concrete_object (g winner_label);
      sequence_of_moves = Concrete_object_field.to_list Hex_cell.of_concrete_object (g sequences_of_moves_label);
   };;

let to_concrete_object fgame =
 
   Concrete_object_t.Record([
     dimension_label,Hex_dimension.to_concrete_object(fgame.Hex_finished_game_t.dimension);
     winner_label, Hex_player.to_concrete_object(fgame.Hex_finished_game_t.winner);
     sequences_of_moves_label, Concrete_object_field.of_list Hex_cell.to_concrete_object(fgame.Hex_finished_game_t.sequence_of_moves);
   ]);;

end;;

let cmp = Private.cmp;;
let empty_one = Private.empty_one;;
let enumerate_companions_for_game = Private.enumerate_companions_for_game ;;
let extends = Private.extends;;
let first_move = Private.first_move;;
let largest_nonsurrendering_beginning = Private.largest_nonsurrendering_beginning_for_several;;
let of_concrete_object = Private.of_concrete_object;;
let seek_companion_for_game = Private.seek_companion_for_game;;
let seek_companion_for_strategy = Private.seek_companion_for_strategy;;
let simplify_by_move = Private.simplify_by_move;;
let to_concrete_object = Private.to_concrete_object;;


