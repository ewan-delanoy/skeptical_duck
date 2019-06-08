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

let cut_by (Hex_partial_game_t.PG(l1)) (Hex_partial_game_t.PG(l2))=
  let (common_left,right1,right2)=Listennou.factor (l1,l2) in
  if (right2=[])&&(right1<>[]) then Some(Hex_partial_game_t.PG(right1)) else None;; 

let empty_one = Hex_partial_game_t.PG  [];;
let singleton cell = Hex_partial_game_t.PG [cell];;

let one_move_more (Hex_partial_game_t.PG(l)) cell = 
  Hex_partial_game_t.PG(l@[cell]);;

let last_one_to_play (Hex_partial_game_t.PG(l)) =
   let d=(List.length l) mod 2 in 
   if d=0 then Hex_player_t.Second_player else Hex_player_t.First_player;;

let of_string s =
  let temp1=Str.split (Str.regexp_string joiner) s in 
  Hex_partial_game_t.PG(
      Image.image Hex_cell.of_string temp1
  );;

let to_string (Hex_partial_game_t.PG(l))=
  String.concat joiner (Image.image Hex_cell.to_string l);;

let print_out (fmt:Format.formatter) ap=
   Format.fprintf fmt "@[%s@]" (to_string ap);;     