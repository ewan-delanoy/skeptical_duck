(* 

#use"Hex_memory/hex_partial_game.ml";;

First coordinate is column index, second is row index

*)

let cmp=((fun 
 (Hax_checked_initial_game_t.PG(l1)) (Hax_checked_initial_game_t.PG(l2)) ->
   (Total_ordering.lex_compare 
       Hex_cell.cmp l1 l2) :> 
       Hax_checked_initial_game_t.t Total_ordering.t) );;

let joiner = " - ";;

let starts_with (Hax_checked_initial_game_t.PG(l1)) (Hax_checked_initial_game_t.PG(l2))=
  let (common_left,right1,right2)=Listennou.factor (l1,l2) in
  right2=[];; 

let cut_by (Hax_checked_initial_game_t.PG(l1)) (Hax_checked_initial_game_t.PG(l2))=
  let (common_left,right1,right2)=Listennou.factor (l1,l2) in
  if (right2=[])&&(right1<>[]) then Some(Hax_checked_initial_game_t.PG(right1)) else None;; 

let empty_one = Hax_checked_initial_game_t.PG  [];;
let singleton cell = Hax_checked_initial_game_t.PG [cell];;

let one_move_more (Hax_checked_initial_game_t.PG(l)) cell = 
  Hax_checked_initial_game_t.PG(l@[cell]);;

let last_one_to_play (Hax_checked_initial_game_t.PG(l)) =
   let d=(List.length l) mod 2 in 
   if d=0 then Hax_player_t.Second_player else Hax_player_t.First_player;;

let depth (Hax_checked_initial_game_t.PG(l)) = List.length l ;; 
let first_move (Hax_checked_initial_game_t.PG(l)) = List.hd l ;; 

let of_string s =Hax_checked_initial_game_t.PG( 
   Hax_common.cell_list_of_string s
);;

let to_string (Hax_checked_initial_game_t.PG(l))=
  Hax_common.cell_list_to_string  l;;


let print_out (fmt:Format.formatter) (Hax_checked_initial_game_t.PG(l))=
   Format.fprintf fmt "@[%s@]" (Hax_common.cell_list_to_pretty_string l);;     