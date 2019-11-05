(* 

#use"Hex_analysis/hex_strong_opening.ml";;

*)

let to_string (Hex_untamed_opening_t.O l)=
   "O("^(Hex_common.cell_list_to_string l)^")";;

let of_string text =
     let text1=Cull_string.two_sided_cutting ("O(",")") text in 
     Hex_untamed_opening_t.O(Hex_common.cell_list_of_string text1);;


let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_untamed_opening_t.O(
      Concrete_object_field.to_list Hex_cell.of_concrete_object arg1
   );;

let to_concrete_object (Hex_untamed_opening_t.O(l))=
   Concrete_object_t.Variant("Hex_"^"untamed_opening.O",
     [Concrete_object_field.of_list Hex_cell.to_concrete_object l
     ]);;

let recipient (Hex_untamed_opening_t.O(l))=Hex_common.has_just_played l;;

let extends 
   (Hex_untamed_opening_t.O l1) (Hex_untamed_opening_t.O l2)=
   let (_,_,r2)=Listennou.factor (l1,l2) in r2=[];;



let simplify_by_move new_move (Hex_untamed_opening_t.O l) =
  match l with 
  []->None
  |first_move::other_moves ->
    if (first_move = new_move)&&(other_moves <> [])
    then Some(Hex_untamed_opening_t.O(other_moves))
    else None;;


let first_move (Hex_untamed_opening_t.O l) = List.hd l;;

let easy_advance (Hex_untamed_opening_t.O l) =
   if List.length l=1 then Some(List.hd l) else None;;

let cmp = 
 ((fun (Hex_untamed_opening_t.O l1) (Hex_untamed_opening_t.O l2) ->
    Total_ordering.silex_compare Hex_cell.cmp l1 l2
) :> Hex_untamed_opening_t.t Total_ordering.t);;

