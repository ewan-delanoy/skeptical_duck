(* 

#use"Hex_memory/hex_partial_game.ml";;

First coordinate is column index, second is row index

*)

let cmp=((fun 
 (Hex_partial_game_t.PG(l1)) (Hex_partial_game_t.PG(l2)) ->
   (Total_ordering.lex_compare 
       Hex_cell.cmp l1 l2) :> 
       Hex_partial_game_t.t Total_ordering.t) );;

let joiner = " - ";;

let starts_with (Hex_partial_game_t.PG(l1)) (Hex_partial_game_t.PG(l2))=
  let (common_left,right1,right2)=Listennou.factor (l1,l2) in
  right2=[];; 

let of_string s =
  let temp1=Str.split (Str.regexp_string joiner) s in 
  Hex_partial_game_t.PG(
      Image.image Hex_cell.of_string temp1
  );;

let to_string (Hex_partial_game_t.PG(l))=
  String.concat joiner (Image.image Hex_cell.to_string l);;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     