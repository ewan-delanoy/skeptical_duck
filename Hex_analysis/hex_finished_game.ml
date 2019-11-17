(* 

#use"Hex_analysis/hex_finished_game.ml";;

*)

exception End_strategy_is_too_fast of Hex_flattened_end_strategy_t.t;;

let empty_one = {
   Hex_finished_game_t.dimension = 11;
   winner = Hex_player_t.First_player;
   sequence_of_moves = [];
};;

let rec helper1_for_relevant_strategy_seeking fles seq_moves=
   let fgame={
     Hex_finished_game_t.winner = fles.Hex_flattened_end_strategy_t.beneficiary;
     dimension = 11;
     sequence_of_moves = seq_moves;
   } in   
   match Hex_meeting_result.meet fles fgame with
     Hex_meeting_result_t.Separation(_,_) 
    |Hex_meeting_result_t.Incomplete(_)->None
    |Hex_meeting_result_t.Relevance (shorter_seq,_)->Some(shorter_seq);; 
 
let rec helper2_for_relevant_strategy_seeking 
   (seq_of_moves,already_found_relevants,flesses) = 
    match flesses with 
     []->(seq_of_moves,List.rev already_found_relevants)
    |fles::others ->
       (match helper1_for_relevant_strategy_seeking fles seq_of_moves with 
        None -> helper2_for_relevant_strategy_seeking 
               (seq_of_moves,already_found_relevants,others)
       |Some(shorter_seq)->
             if shorter_seq = seq_of_moves 
             then  helper2_for_relevant_strategy_seeking 
                   (seq_of_moves,fles::already_found_relevants,others)
             else  helper2_for_relevant_strategy_seeking 
                   (shorter_seq,[fles],others)        
       );;

let seek_relevant_strategies fgame flesses = 
    let (seq,relvs)= helper2_for_relevant_strategy_seeking 
   (fgame.Hex_finished_game_t.sequence_of_moves,[],flesses)  in 
   ({fgame with Hex_finished_game_t.sequence_of_moves=seq},relvs);;

let largest_unconclusive_beginning fgame flesses =
   let (shortened_fgame,relvs) = seek_relevant_strategies fgame flesses in 
   if relvs=[]
   then fgame 
   else shortened_fgame;;


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

let best_fits_for_strategy fles fgames =
   let temp1=Option.filter_and_unpack (compute_optional_fit fles) fgames in 
   let (found_min,sols)=Min.minimize_it_with_care fst temp1 in 
   (found_min,Image.image (fun (_,x)->fst x) sols);;

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
