(* 

#use"Hex_analysis/hex_end_of_battle.ml";;

*)

let assess eob cell =
   if Hex_cell_set.mem cell eob.Hex_end_of_battle_t.ally_territory  then Hex_eob_result_t.Ally_territory else 
   if Hex_cell_set.mem cell eob.Hex_end_of_battle_t.enemy_territory then Hex_eob_result_t.Enemy_territory else 
   Hex_eob_result_t.Unoccupied;;

let of_finished_game fgame =
     let (black_moves,white_moves)=Listennou.split_list_in_half (fgame.Hex_finished_game_t.sequence_of_moves) in 
     let w = fgame.Hex_finished_game_t.winner in 
     let (winner_moves,loser_moves) =  (
         match w with 
         Hex_player_t.First_player -> (black_moves,white_moves)
                                |_ -> (white_moves,black_moves)
     ) in 
     {
         Hex_end_of_battle_t.dimension       
                         = fgame.Hex_finished_game_t.dimension ;
         winner          = w ; 
         ally_territory  = Hex_cell_set.safe_set winner_moves ; 
         enemy_territory = Hex_cell_set.safe_set loser_moves ;
     } ;;
