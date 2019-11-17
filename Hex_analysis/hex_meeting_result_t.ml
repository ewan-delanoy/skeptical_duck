(* 

#use"Hex_analysis/hex_meeting_result_t.ml";;

In the Relevance field, the first argument is the sequence of moves
that reached the position , and the second argument is the single
remaining unplayed active cell.

In the Incomplete field, the argument is the set of remaining unplayed active cells.

*)

type t= 
   Separation of Hex_player_t.t * Hex_cell_t.t 
  |Relevance of (Hex_cell_t.t list) * (Hex_cell_t.t) 
  |Incomplete of Hex_cell_set_t.t;;