(* 

#use"Hex_analysis/hex_end_of_battle.ml";;

*)

let assess eob cell =
   if Hex_cell_set.mem cell eob.Hex_end_of_battle_t.ally_territory  then Hex_eob_result_t.Ally_territory else 
   if Hex_cell_set.mem cell eob.Hex_end_of_battle_t.enemy_territory then Hex_eob_result_t.Enemy_territory else 
   Hex_eob_result_t.Unoccupied;;

let compatible_border_triangles eob = 
   let base = Hex_cardinal_direction.all_border_triangles (eob.Hex_end_of_battle_t.dimension) 
   and (side1,side2) = Hex_cardinal_direction.sides_for_player (eob.Hex_end_of_battle_t.winner) in 
   let sides = [side1;side2] in 
   let evl = assess eob in 
   List.filter (
      function (side,cell1,cell2,cell3) -> 
        (evl cell1 = Hex_eob_result_t.Ally_territory) &&
        (evl cell2 = Hex_eob_result_t.Unoccupied) &&
        (evl cell3 = Hex_eob_result_t.Unoccupied) && 
        (List.mem side sides)
   ) base ;;

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

let of_activated_molecular (dim,the_winner) (active_part,mlclr) = 
    let passive_part = Hex_molecular_linker.support mlclr in 
    let needed_cells = Hex_cell_set.merge active_part passive_part in 
    let all_cells = Hex_common.all_cells dim in 
    let unneeded_cells = Hex_cell_set.setminus all_cells needed_cells in 
    {
    Hex_end_of_battle_t.dimension       
                         = dim ;
         winner          = the_winner ; 
         ally_territory  = active_part ; 
         enemy_territory = unneeded_cells ;
     } ;;

let remaining_free_cells end_of_battle = 
    let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
    let all_cells = Hex_common.all_cells dim in 
    let already_used_cells =
        Hex_cell_set.merge 
           end_of_battle.Hex_end_of_battle_t.ally_territory 
            end_of_battle.Hex_end_of_battle_t.enemy_territory in 
    Hex_cell_set.setminus all_cells already_used_cells ;;          