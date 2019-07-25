(* 

#use"Hex_analysis/hex_finished_game.ml";;

*)

exception End_config_is_too_fast of Hex_end_configuration_t.t;;

let compute_largest_unconclusive_beginning fgame end_config =
   if Hex_cell_set.length(end_config.Hex_end_configuration_t.active_part)<2
   then raise(End_config_is_too_fast(end_config))
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
         match Hex_end_configuration.use_move_to_simplify_one next_move walker with 
         None->fgame
         |Some(walker2)->
         if Hex_cell_set.length(walker2.Hex_end_configuration_t.active_part)<2
         then {fgame with Hex_finished_game_t.sequence_of_moves = (List.rev treated)}
         else tempf((snd next_move)::treated,walker2,other_moves)
       )
   ) in 
   tempf([],end_config,temp2);;

let iterated_largest_unconclusive_beginning fgame end_configs=
  List.fold_left compute_largest_unconclusive_beginning fgame end_configs;;

let is_more_detailed_than fgame1 fgame2=
   let l1=fgame1.Hex_finished_game_t.sequence_of_moves
   and l2=fgame2.Hex_finished_game_t.sequence_of_moves in 
   let (_,_,r2)=Listennou.factor (l1,l2) in r2=[];;



let announce_winner ="\nBeneficiary : \n";;
let announce_moves ="\nActive part : \n";;

let absorb_move new_move fgame=match fgame.Hex_finished_game_t.sequence_of_moves with 
  []->None
  |first_move::other_moves ->
    if (first_move = new_move)&&(other_moves <> [])
    then Some({fgame with Hex_finished_game_t.sequence_of_moves = other_moves})
    else None;;

let to_end_configuration fgame=
   let winner = fgame.Hex_finished_game_t.winner in 
   let (fp_moves,sp_moves)=Hex_common.split_list_in_half fgame.Hex_finished_game_t.sequence_of_moves in
   let ordered_fp_moves = Hex_cell_set.safe_set fp_moves 
   and ordered_sp_moves = Hex_cell_set.safe_set sp_moves in 
   let (actives,rejected_ones)=(
      match winner with 
       Hex_player_t.First_player -> (ordered_fp_moves,ordered_sp_moves) 
      |Hex_player_t.Second_player ->(ordered_sp_moves,ordered_fp_moves) 
   )  in 
   let all_cells = Hex_common.all_cells 11 in 
   let passives= Hex_cell_set.setminus (Hex_cell_set.setminus all_cells rejected_ones) actives in 
   {
     Hex_end_configuration_t.beneficiary=winner;
     Hex_end_configuration_t.active_part=actives;
     Hex_end_configuration_t.passive_part=passives;
     Hex_end_configuration_t.index=0;
   };;



let to_string fgame=
  let descr1=Hex_player.to_string(fgame.Hex_finished_game_t.winner) 
  and descr2=Hex_common.cell_list_to_string(fgame.Hex_finished_game_t.sequence_of_moves) in 
  announce_winner^descr1^announce_moves^descr2;;

let of_string s =
   let s1=Cull_string.cobeginning 
     (String.length announce_winner) s in 
   let i1=Substring.leftmost_index_of_in announce_moves s1 in
   let j1=i1+(String.length announce_moves)-1 in 
   let descr1=Cull_string.interval s1 1 (i1-1) 
   and descr2=Cull_string.interval s1 (j1+1) (String.length s1) in 
   {
     Hex_finished_game_t.winner=Hex_player.of_string descr1;
     Hex_finished_game_t.sequence_of_moves=Hex_common.cell_list_of_string descr2;
   };;

let first_move fgame = List.hd(fgame.Hex_finished_game_t.sequence_of_moves);;

let unveil fgame=
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
   (unveil fgame1) (unveil fgame2)  
) :> Hex_finished_game_t.t Total_ordering.t);;

