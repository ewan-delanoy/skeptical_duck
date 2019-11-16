(* 

#use"Hex_analysis/hex_uog_list.ml";;

*)


module Private = struct 

let select_openings_with_next_player_as_recipient l =
   List.filter Hex_untamed_opening.has_odd_length l;;

let absorb_move move (moves_before,next_to_move,dfg)=
  (move::moves_before,
    Hex_player.other_player next_to_move, 
     Hex_fg_double_list.simplify_by_move move dfg);;

let insert_in new_uog l=
   if List.exists (fun uog -> Hex_untamed_opening.extends uog new_uog ) l
   then l 
   else 
     let cleaned_l=List.filter 
       (fun uog->not(Hex_untamed_opening.extends new_uog uog)) l in 
     Ordered.insert Hex_untamed_opening.cmp new_uog cleaned_l;;  

let rec compute_maximal_strong_openings (treated,to_be_treated)=
   match to_be_treated with 
   []->treated 
   |triple :: others-> 
      let (moves_before,next_to_move,dfg) = triple in 
      let (Hex_cell_set_t.S new_pushes) = Hex_fg_double_list.first_moves next_to_move dfg in
      if new_pushes = []
      then let new_untamed = Hex_untamed_opening_t.O(List.rev(moves_before)) in 
           compute_maximal_strong_openings 
             (insert_in new_untamed treated,others)    
      else compute_maximal_strong_openings 
             (treated, 
               (Image.image (fun move -> absorb_move move triple) new_pushes) @ others);;

end ;; 

let compute_maximal_strong_openings dfg=
  Private.compute_maximal_strong_openings 
   ([],[[],Hex_player_t.First_player,dfg]);;

let of_concrete_object crobj = Concrete_object_field.to_list Hex_untamed_opening.of_concrete_object crobj;;


let insert_in = Private.insert_in;;  


let seek_interesting_move l=
   let temp1=Private.select_openings_with_next_player_as_recipient l in 
   if temp1=[] then None else 
   let temp2=List.hd temp1 in 
   Some(Hex_untamed_opening.first_move temp2,List.tl(Hex_untamed_opening.unveil temp2));;

let simplify_by_move move l=Option.filter_and_unpack 
(Hex_untamed_opening.simplify_by_move move) l;;     

let to_concrete_object l = Concrete_object_field.of_list Hex_untamed_opening.to_concrete_object l;;
