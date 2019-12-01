(* 

#use"Hex_analysis/hex_finished_game.ml";;

*)

module Private = struct


let empty_one = {
   Hex_finished_game_t.dimension = 11;
   winner = Hex_player_t.First_player;
   sequence_of_moves = [];
};;




let largest_nonsurrendering_beginning_for_one fgame fles = 
    match Hex_meeting_result.meet fles fgame with 
     Hex_meeting_result_t.Surrender(moves_before,_,_)->{fgame with Hex_finished_game_t.sequence_of_moves = moves_before}
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

let compute_optional_fit fles fgame =
   match Hex_meeting_result.meet fles fgame with
     Hex_meeting_result_t.Separation(_,_) ->None
    |Hex_meeting_result_t.Incomplete(remaining_ones)->Some(Hex_cell_set.length remaining_ones,(fgame,fles))
    |Hex_meeting_result_t.Relevance (shorter_seq,_)->Some(1,(fgame,fles));; 


let seek_companion_for_strategy fles fgames =
   Option.find_and_stop (
     fun fgame -> match Hex_meeting_result.meet fles fgame with
       Hex_meeting_result_t.Attack_but_no_surrender(moves_before,pivot)
   ) fgames ;;

let best_fits_for_game fgame flesses =
   let temp1=Option.filter_and_unpack (fun fles->compute_optional_fit fles fgame) flesses in 
   let (found_min,sols)=Min.minimize_it_with_care fst temp1 in 
   (found_min,Image.image (fun (_,x)->snd x) sols);;


let salt = "Hex_"^"finished_game_t.";;

let dimension_label          = salt ^ "dimension";;
let winner_label             = salt ^ "winner";;
let sequences_of_moves_label = salt ^ "sequences_of_moves";;



let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Hex_finished_game_t.dimension = Concrete_object_field.unwrap_int (g dimension_label);
      winner = Hex_player.of_concrete_object (g winner_label);
      sequence_of_moves = Concrete_object_field.to_list Hex_cell.of_concrete_object (g sequences_of_moves_label);
   };;

let to_concrete_object fgame =
 
   Concrete_object_t.Record([
     dimension_label,Concrete_object_t.Int(fgame.Hex_finished_game_t.dimension);
     winner_label, Hex_player.to_concrete_object(fgame.Hex_finished_game_t.winner);
     sequences_of_moves_label, Concrete_object_field.of_list Hex_cell.to_concrete_object(fgame.Hex_finished_game_t.sequence_of_moves);
   ]);;

end;;

let cmp = Private.cmp;;
let extends = Private.extends;;
let empty_one = Private.empty_one;;
let first_move = Private.first_move;;
let simplify_by_move = Private.simplify_by_move;;
let best_fits_for_game = Private.best_fits_for_game;;
let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;
let best_fits_for_strategy = Private.best_fits_for_strategy;;
let largest_nonsurrendering_beginning = Private.largest_nonsurrendering_beginning_for_several;;

