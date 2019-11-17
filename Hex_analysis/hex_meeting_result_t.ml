(* 

#use"Hex_analysis/hex_meeting_result_t.ml";;

In the Relevance field, the first argument is the sequence of moves
that reached the position , and the second argument is the single
remaining unplayed active cell.

*)

type t= 
   Separation of Hex_player_t.t * Hex_cell_t.t 
  |Relevance of (Hex_cell_t.t list) * (Hex_cell_t.t) ;;