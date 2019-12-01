(* 

#use"Hex_analysis/hex_meeting_result_t.ml";;

In the Attack_but_no_surrender variant, the first argument is the sequence of moves
that reached the position , and the second argument is the single
remaining unplayed active cell.

In the Surrender variant, the first two argument are as in the Attack_but_no_surrender variant
and the third is the "surrendering" move.

In the Stalemate variant, the argument is the set of remaining unplayed active cells.

*)

type t= 
   Victory of Hex_player_t.t * Hex_cell_t.t 
  |Attack_but_no_surrender of (Hex_cell_t.t list) * (Hex_cell_t.t) * int
  |Surrender of (Hex_cell_t.t list) * (Hex_cell_t.t) * (Hex_cell_t.t)  
  |Stalemate of Hex_cell_set_t.t;;