(* 

#use"Hex_analysis/hex_uog_list.ml";;

*)


exception Extract_untamed_exn of Hex_cell_t.t list;;
module Private = struct 

let select_openings_with_next_player_as_recipient l =
   List.filter Hex_untamed_opening.has_odd_length l;;



let insert_in new_uog l=
   if List.exists (fun uog -> Hex_untamed_opening.extends uog new_uog ) l
   then l 
   else 
     let cleaned_l=List.filter 
       (fun uog->not(Hex_untamed_opening.extends new_uog uog)) l in 
     Ordered.insert Hex_untamed_opening.cmp new_uog cleaned_l;;  

let rec helper_during_extraction (moves_before,next_to_move,moves_after,dfgl)=
  match moves_after with 
   []->raise(Extract_untamed_exn(List.rev moves_before)) 
  |move::others->
     let (Hex_cell_set_t.S next_pushes)=Hex_fg_double_list.first_moves next_to_move dfgl in 
     if next_pushes = []
     then Hex_untamed_opening_t.O(List.rev(move::moves_before))
     else let new_dfgl = Hex_fg_double_list.simplify_by_move move dfgl in 
          helper_during_extraction 
           (move::moves_before,Hex_player.other_player next_to_move,others,new_dfgl);;

end ;; 

let extract_untamed_openings dfgl =
   let (Hex_fg_double_list_t.DL(l1,_)) = dfgl in 
   let temp1 = Image.image (fun fg->
      Private.helper_during_extraction 
       ([],Hex_player_t.First_player,fg.Hex_finished_game_t.sequence_of_moves,dfgl)
    ) l1 in 
   Ordered.sort Hex_untamed_opening.cmp temp1 ;;


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
