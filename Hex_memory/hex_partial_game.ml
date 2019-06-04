(* 

#use"Hex_memory/hex_partial_game.ml";;

First coordinate is column index, second is row index

*)

let cmp=((fun 
 (Hex_partial_game_t.PG(l1)) (Hex_partial_game_t.PG(l2)) ->
   (Total_ordering.lex_compare 
       Hex_cell.cmp l1 l2) :> 
       Hex_partial_game_t.t Total_ordering.t) );;

let of_string s =
  let temp1=Str.split (Str.regexp_string " - ") s in 
  Hex_partial_game_t.PG(
      Image.image Hex_cell.of_string temp1
  );;

let to_string (Hex_partial_game_t.PG(l))=
  String.concat " - " (Image.image Hex_cell.to_string l);;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     