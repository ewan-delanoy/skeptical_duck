(* 

#use"Hex_analysis/hex_uog_list.ml";;

*)


module Private = struct 

let select_openings_with_next_player_as_recipient l =
   List.filter Hex_untamed_opening.has_odd_length l;;

end ;; 


let of_concrete_object crobj = Concrete_object_field.to_list Hex_untamed_opening.of_concrete_object crobj;;
let to_concrete_object l = Concrete_object_field.of_list Hex_untamed_opening.to_concrete_object l;;


let insert_in new_uog l=
   if List.exists (fun uog -> Hex_untamed_opening.extends uog new_uog ) l
   then l 
   else 
     let cleaned_l=List.filter 
       (fun uog->not(Hex_untamed_opening.extends new_uog uog)) l in 
     Ordered.insert Hex_untamed_opening.cmp new_uog cleaned_l;;  


let simplify_by_move move l=Option.filter_and_unpack 
(Hex_untamed_opening.simplify_by_move move) l;;     

let seek_interesting_move l=
   let temp1=Private.select_openings_with_next_player_as_recipient l in 
   if temp1=[] then None else 
   let temp2=List.hd temp1 in 
   Some(Hex_untamed_opening.first_move temp2,List.tl(Hex_untamed_opening.unveil temp2));;

