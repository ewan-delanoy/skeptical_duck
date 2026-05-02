(*

#use"lib/string_ranges.ml";;

*)

module Private = struct 

let complement_of_union_of_ranges text ranges =
  (* the ranges are assumed to be in increasing order *) 
  let n = String.length(text) in 
  let compl = Arithmetic_list.complement_union_of_ranges ranges n in 
  Image.image (fun (i,j)->String.sub text (i-1) (j-i+1)) compl ;;

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

let remove_next_ordered_occurrence_of_uple_in_from_opt patts text start_idx = 
   (*
   The convention is that in the returned result,
   we include the leftmost leftover but not the rightmost,
   so that we can iterate this function consistently later.
   For example, 
   remove_next_ordered_occurrence_of_uple_in_from_opt ["a";"b"] "1a2b3" 1
   returns  (["1"; "2"], 5) not (["1"; "2"; "3"], 5)

   *)
   let subtext = String.sub text (start_idx-1) (String.length(text)-start_idx+1) in 
   match ranges_for_next_ordered_occurrence_of_uple_in_from_opt patts subtext 1 with
   None -> ([],start_idx) 
   |Some ranges1 ->
      let rev_ranges1 = List.rev ranges1 in 
      let (a1,b1) = List.hd(rev_ranges1) in 
      let modified_revranges1= (a1,String.length text) :: (List.tl rev_ranges1) in 
      let ranges = List.rev modified_revranges1 
      and next_idx = b1+1 in 
      (complement_of_union_of_ranges subtext ranges,(start_idx-1)+next_idx) ;;

(*

let txt = "abcdefghijklmnop" ;;
let ranges = Option.get(ranges_for_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt 1);;
let compl = complement_of_union_of_ranges txt ranges ;;
let remains = remove_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt 1;;

let txt2 = "123" ^ txt ;;
let ranges = Option.get(ranges_for_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt2 4);;
let compl = complement_of_union_of_ranges txt2 ranges ;;
let remains = remove_next_ordered_occurrence_of_uple_in_from_opt ["bc";"f";"jk"] txt2 4;;

*)

let rec helper_for_removing_all_occurrences patts (txt,treated,current_idx) =
  let (data,next_idx)= remove_next_ordered_occurrence_of_uple_in_from_opt patts txt current_idx in 
  if next_idx = current_idx 
  then let remainder = Cull_string.cobeginning (current_idx-1) txt in 
       (List.rev(treated),remainder)
  else helper_for_removing_all_occurrences patts (txt,data::treated,next_idx) ;;
    
end ;;

let remove_all_ordered_occurrences_of_uple_in patts txt = 
    Private.helper_for_removing_all_occurrences patts (txt,[],1) ;;

(*

remove_all_ordered_occurrences_of_uple_in ["bc";"f";"jk"] "123" ;;
remove_all_ordered_occurrences_of_uple_in ["bc";"f";"jk"] "abcdefghijklmnop" ;;
remove_all_ordered_occurrences_of_uple_in ["bc";"f";"jk"] "abcdefghijklmnop1bc23f456jk78" ;;

*)    

let remove_next_ordered_occurrence_of_uple_in_from_opt = Private.remove_next_ordered_occurrence_of_uple_in_from_opt ;; 

let replace_ranges_in l s=
    if l=[] then s else
    let n=String.length s in
    let ranges=Image.image fst l in
    let partition= Partition_list.from_set_of_ranges ranges n in 
    let temp1=Image.image (
      fun (i,j,will_be_replaced)->
        if will_be_replaced 
        then List.assoc (i,j) l
        else String.sub s (i-1) (j-i+1)
    ) partition in
    String.concat "" temp1;;

(*

replace_ranges_in [((3,5),"A");((8,12),"B")] "12345678901234567890";;

*)


   