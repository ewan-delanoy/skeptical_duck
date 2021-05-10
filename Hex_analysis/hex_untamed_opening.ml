(* 

#use"Hex_analysis/hex_strong_opening.ml";;

*)

let cmp = 
 ((fun (Hex_untamed_opening_t.O l1) (Hex_untamed_opening_t.O l2) ->
    Total_ordering.silex_compare Hex_cell.cmp l1 l2
) :> Hex_untamed_opening_t.t Total_ordering.t);;

let extends 
   (Hex_untamed_opening_t.O l1) (Hex_untamed_opening_t.O l2)=
   Listennou.extends l1 l2;;

let first_move (Hex_untamed_opening_t.O l) = List.hd l;;

let has_odd_length (Hex_untamed_opening_t.O l)=(((List.length l) mod 2)=1);;


let of_concrete_object crobj=
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   Hex_untamed_opening_t.O(
      Crobj_converter_combinator.to_list Hex_cell.of_concrete_object arg1
   );;

let simplify_by_move new_move (Hex_untamed_opening_t.O l) =
  match l with 
  []->None
  |first_move::other_moves ->
    if (first_move = new_move)&&(other_moves <> [])
    then Some(Hex_untamed_opening_t.O(other_moves))
    else None;;

let to_concrete_object (Hex_untamed_opening_t.O(l))=
   Concrete_object_t.Variant("Hex_"^"untamed_opening.O",
     [Crobj_converter_combinator.of_list Hex_cell.to_concrete_object l
     ]);;

let to_readable_string (Hex_untamed_opening_t.O(l))=Hex_common.cell_list_to_pretty_string l;;










let unveil (Hex_untamed_opening_t.O l)=l;;

let length (Hex_untamed_opening_t.O l)=List.length l;;



