(* 

#use"Hex_analysis/hex_finished_game.ml";;

*)

exception End_strategy_is_too_fast of Hex_flattened_end_strategy_t.t;;

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
   let l1=fgame1.Hex_finished_game_t.sequence_of_moves
   and l2=fgame2.Hex_finished_game_t.sequence_of_moves in 
   let (_,_,r2)=Listennou.factor (l1,l2) in r2=[];;




let simplify_by_move new_move fgame=match fgame.Hex_finished_game_t.sequence_of_moves with 
  []->None
  |first_move::other_moves ->
    if (first_move = new_move)&&(other_moves <> [])
    then Some({fgame with Hex_finished_game_t.sequence_of_moves = other_moves})
    else None;;


let announce_dimension ="\nDimension : \n";;
let announce_winner ="\nBeneficiary : \n";;
let announce_moves ="\nActive part : \n";;


let to_string fgame=
  let descr1=string_of_int(fgame.Hex_finished_game_t.dimension)
  and descr2=Hex_player.to_string(fgame.Hex_finished_game_t.winner) 
  and descr3=Hex_common.cell_list_to_string(fgame.Hex_finished_game_t.sequence_of_moves) in 
  announce_dimension^descr1^announce_winner^descr2^announce_moves^descr3;;

let of_string s =
   let s1=Cull_string.cobeginning 
     (String.length announce_dimension) s in 
   let i1=Substring.leftmost_index_of_in announce_winner s1 
   and i2=Substring.leftmost_index_of_in announce_moves s1 in
   let j1=i1+(String.length announce_winner)-1 
   and j2=i2+(String.length announce_moves)-1 in 
   let descr1=Cull_string.interval s1 1 (i1-1) 
   and descr2=Cull_string.interval s1 (j1+1) (i2-1)
   and descr3=Cull_string.interval s1 (j2+1) (String.length s1) in 
   {
     Hex_finished_game_t.dimension=int_of_string descr1;
     Hex_finished_game_t.winner=Hex_player.of_string descr2;
     Hex_finished_game_t.sequence_of_moves=Hex_common.cell_list_of_string descr3;
   };;

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

