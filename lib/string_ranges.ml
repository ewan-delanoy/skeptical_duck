(*

#use"lib/string_ranges.ml";;

*)

let range_for_next_occurrence_of_in_from_opt patt text start_idx =
 match Substring.leftmost_index_of_in_from_opt patt text start_idx with 
 None -> None 
 |Some i1 -> Some(i1,i1+(String.length patt)-1);;

let ranges_for_next_ordered_occurrence_of_uple_in_from_opt patts original_text start_idx =
  let rec helper =(fun (text,remaining_patts,treated,current_idx) ->
  match remaining_patts with 
  [] -> Some(List.rev treated) 
  |patt::other_patts ->
   match range_for_next_occurrence_of_in_from_opt patt text current_idx with 
   None -> None 
   |Some(i1,i2) -> 
      helper (text,other_patts,(i1,i2)::treated,i2+1)) in 
   helper (original_text,patts,[],start_idx);;

let complement_of_union_of_ranges text ranges =
  (* the ranges are assumed to be in increasing order *) 
  let n = String.length(text) in 
  let compl = Arithmetic_list.complement_union_of_ranges ranges n in 
  Image.image (fun (i,j)->Cull_string.interval text i j) compl ;;

(*

let txt = "abcdefghijklmnop" ;;
let ranges = Option.get(ranges_for_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt 1);;
let compl = complement_of_union_of_ranges txt ranges ;;

*)


   