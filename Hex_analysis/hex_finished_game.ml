(* 

#use"Hex_analysis/hex_finished_game.ml";;

*)

exception End_strategy_is_too_fast of Hex_flattened_end_strategy_t.t;;

let empty_one = {
   Hex_finished_game_t.dimension = 11;
   winner = Hex_player_t.First_player;
   sequence_of_moves = [];
};;

let rec helper1_for_relevant_strategy_seeking (moves_before,player,moves_after,fles)=
   if Hex_cell_set.length(fles.Hex_flattened_end_strategy_t.active_part)=1
   then  (* Here the next player is forced to move in a certain set,
         so the strategy is relevant indeed *)
         Some(List.rev(moves_before)) 
   else (
          match moves_after with 
          []->None 
          |move::following_moves ->
             (match Hex_flattened_end_strategy.use_move_to_simplify_one (player,move) fles with
               None -> None 
               |Some(new_fles) ->
               helper1_for_relevant_strategy_seeking 
                 (move::moves_before,Hex_player.other_player player,following_moves,new_fles) 
             ) 
        );;

let rec helper2_for_relevant_strategy_seeking 
   (seq_of_moves,already_found_relevants,flesses) = 
    match flesses with 
     []->(seq_of_moves,List.rev already_found_relevants)
    |fles::others ->
       (match helper1_for_relevant_strategy_seeking 
        ([],Hex_player_t.First_player,seq_of_moves,fles) with 
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




let compute_largest_unconclusive_beginning fgame fles =
   if Hex_cell_set.length(fles.Hex_flattened_end_strategy_t.active_part)<2
   then raise(End_strategy_is_too_fast(fles))
   else 
   let temp1= Ennig.index_everything (fgame.Hex_finished_game_t.sequence_of_moves) in 
   let temp2= Image.image (
       fun (k,move)->
         let player=(if k mod 2 = 1 
                     then Hex_player_t.First_player 
                     else Hex_player_t.Second_player )  in 
         (player,move)            
   ) temp1 in 
   let rec tempf=(fun (treated,walker,to_be_treated)->
      match to_be_treated with 
      []->{fgame with Hex_finished_game_t.sequence_of_moves = (List.rev treated)}
      |next_move::other_moves->
       (
         match Hex_flattened_end_strategy.use_move_to_simplify_one next_move walker with 
         None->fgame
         |Some(walker2)->
         if Hex_cell_set.length(walker2.Hex_flattened_end_strategy_t.active_part)<2
         then {fgame with Hex_finished_game_t.sequence_of_moves = (List.rev treated)}
         else tempf((snd next_move)::treated,walker2,other_moves)
       )
   ) in 
   tempf([],fles,temp2);;

let iterated_largest_unconclusive_beginning fgame flesses=
  List.fold_left compute_largest_unconclusive_beginning fgame flesses;;

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
  let (fst_player_moves,snd_player_moves) = 
      Listennou.split_list_in_half (fgame.Hex_finished_game_t.sequence_of_moves) in 
  let (ally_moves,enemy_moves) = (
     match fles.Hex_flattened_end_strategy_t.beneficiary with 
      Hex_player_t.First_player -> (fst_player_moves,snd_player_moves) 
     |Hex_player_t.Second_player -> (snd_player_moves,fst_player_moves) 
  ) in 
  let ally_set = Hex_cell_set.safe_set ally_moves 
  and enemy_set = Hex_cell_set.safe_set ally_moves in 
  if (not(Hex_cell_set.does_not_intersect enemy_set (Hex_flattened_end_strategy.support fles))) 
  then None
  else 
  let remaining_cells = Hex_cell_set.setminus fles.Hex_flattened_end_strategy_t.active_part ally_set in 
  Some(Hex_cell_set.length remaining_cells,fgame);;

let best_fits_for_strategy fles fgames =
   let temp1=Option.filter_and_unpack (compute_optional_fit fles) fgames in 
   let (found_min,sols)=Min.minimize_it_with_care fst temp1 in 
   (found_min,Image.image snd sols)

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
