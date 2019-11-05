(* 

#use"Hex_analysis/hex_uog_list.ml";;

*)


module Private = struct 

let remove_redundancies l=
 let rec tempf=(function
   (treated,to_be_treated)->match to_be_treated with
   []->treated
   |uog::other_openings->
     let cleaned_openings=List.filter (
       fun uog2->not(Hex_untamed_opening.extends uog uog2)
     ) other_openings in 
     tempf(uog::treated,cleaned_openings)
 ) in
 tempf([],List.rev l);;

let select_openings_with_next_player_as_recipient l =
   List.filter Hex_untamed_opening.has_odd_length l;;

end ;; 


let of_concrete_object crobj = Concrete_object_field.to_list Hex_untamed_opening.of_concrete_object crobj;;
let to_concrete_object l = Concrete_object_field.of_list Hex_untamed_opening.to_concrete_object l;;


let add_opening fgame l=
   let temp1= Ordered.insert_plaen Hex_untamed_opening.cmp fgame l  in 
   Private.remove_redundancies temp1;;


let simplify_by_move move l=Option.filter_and_unpack (Hex_untamed_opening.simplify_by_move move) l;;     

let seek_interesting_move l=
   let temp1=Private.select_openings_with_next_player_as_recipient l in 
   if temp1=[] then None else 
   let temp2=List.hd temp1 in 
   Some(Hex_untamed_opening.first_move temp2,Hex_untamed_opening.is_a_singleton);;

