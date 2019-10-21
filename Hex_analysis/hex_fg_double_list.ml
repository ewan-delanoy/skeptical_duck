(* 

#use"Hex_analysis/hex_fg_double_list.ml";;

*)

let empty_one = Hex_fg_double_list_t.DL([],[]);;

let joiner_for_two="\n<[<>]>\n";;

let of_string s=
   let n=String.length s 
   and i1=Substring.leftmost_index_of_in joiner_for_two s in 
   let j1=i1+(String.length joiner_for_two)-1 in 
   let part1=Cull_string.interval s 1 (i1-1) 
   and part2=Cull_string.interval s (j1+1) n in 
   Hex_fg_double_list_t.DL(
     (Hex_fg_list.of_string part1),
     (Hex_fg_list.of_string part2)
   ) ;;        

let to_string (Hex_fg_double_list_t.DL(l1,l2))=
  (Hex_fg_list.to_string l1)^joiner_for_two^(Hex_fg_list.to_string l2);;

let add_finished_game fgame (Hex_fg_double_list_t.DL(l1,l2))=
   match fgame.Hex_finished_game_t.winner with 
   Hex_player_t.First_player -> 
        let new_l1=Hex_fg_list.add_finished_game fgame l1 in 
        Hex_fg_double_list_t.DL(new_l1,l2)
  |Hex_player_t.Second_player -> 
        let new_l2=Hex_fg_list.add_finished_game fgame l2 in 
        Hex_fg_double_list_t.DL(l1,new_l2) ;;

let take_end_config_into_account end_config (Hex_fg_double_list_t.DL(l1,l2))=
    let new_l1=Hex_fg_list.take_new_end_strategy_into_account end_config l1 
    and new_l2=Hex_fg_list.take_new_end_strategy_into_account end_config l2 in  
    Hex_fg_double_list_t.DL(new_l1,new_l2) ;;

let absorb_move move (Hex_fg_double_list_t.DL(l1,l2))=
   (Hex_fg_double_list_t.DL(
      Hex_fg_list.absorb_move move l1,
      Hex_fg_list.absorb_move move l2
    ));;    

let suggested_moves player (Hex_fg_double_list_t.DL(l1,l2))=
   let (winning_moves,other_moves)=(match player with 
   Hex_player_t.First_player -> 
       (Hex_fg_list.first_moves l1,Hex_fg_list.first_moves l2)
  |Hex_player_t.Second_player -> 
       (Hex_fg_list.first_moves l2,Hex_fg_list.first_moves l1) ) in 
  (winning_moves,Hex_cell_set.setminus other_moves winning_moves);;






