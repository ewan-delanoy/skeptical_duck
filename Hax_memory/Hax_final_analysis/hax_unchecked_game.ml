(* 

#use"Hex_memory/Hex_final_analysis/hex_unchecked_game.ml";;

*)

let get (Hax_unchecked_game_t.FG l_moves) cell=
   match Option.seek (fun (cell1,player)->cell1=cell) l_moves with 
   None->None
   |Some(_,player)->Some(player);;

   