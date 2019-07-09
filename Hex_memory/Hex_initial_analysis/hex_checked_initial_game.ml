(* 

#use"Hex_memory/hex_partial_game.ml";;

First coordinate is column index, second is row index

*)

let cmp=((fun 
 (Hex_checked_initial_game_t.PG(l1)) (Hex_checked_initial_game_t.PG(l2)) ->
   (Total_ordering.lex_compare 
       Hex_cell.cmp l1 l2) :> 
       Hex_checked_initial_game_t.t Total_ordering.t) );;

let joiner = " - ";;

let starts_with (Hex_checked_initial_game_t.PG(l1)) (Hex_checked_initial_game_t.PG(l2))=
  let (common_left,right1,right2)=Listennou.factor (l1,l2) in
  right2=[];; 

let cut_by (Hex_checked_initial_game_t.PG(l1)) (Hex_checked_initial_game_t.PG(l2))=
  let (common_left,right1,right2)=Listennou.factor (l1,l2) in
  if (right2=[])&&(right1<>[]) then Some(Hex_checked_initial_game_t.PG(right1)) else None;; 

let empty_one = Hex_checked_initial_game_t.PG  [];;
let singleton cell = Hex_checked_initial_game_t.PG [cell];;

let one_move_more (Hex_checked_initial_game_t.PG(l)) cell = 
  Hex_checked_initial_game_t.PG(l@[cell]);;

let last_one_to_play (Hex_checked_initial_game_t.PG(l)) =
   let d=(List.length l) mod 2 in 
   if d=0 then Hex_player_t.Second_player else Hex_player_t.First_player;;

let depth (Hex_checked_initial_game_t.PG(l)) = List.length l ;; 
let first_move (Hex_checked_initial_game_t.PG(l)) = List.hd l ;; 

let of_string s =Hex_checked_initial_game_t.PG( 
   Hex_common.cell_list_of_string s
);;

let to_string (Hex_checked_initial_game_t.PG(l))=
  Hex_common.cell_list_to_string  l;;


let print_out (fmt:Format.formatter) (Hex_checked_initial_game_t.PG(l))=
   Format.fprintf fmt "@[%s@]" (Hex_common.cell_list_to_pretty_string l);;     