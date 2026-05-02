(*

#use"lib/string_ranges.ml";;

*)

let complement_of_union_of_ranges text ranges =
  (* the ranges are assumed to be in increasing order *) 
  let n = String.length(text) in 
  let compl = Arithmetic_list.complement_union_of_ranges ranges n in 
  Image.image (fun (i,j)->Cull_string.interval text i j) compl ;;

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
   let subtext = Cull_string.cobeginning (start_idx-1) text in 
   match ranges_for_next_ordered_occurrence_of_uple_in_from_opt patts subtext 1 with
   None -> ([],1) 
   |Some ranges ->
      let next_idx = snd(List.hd(List.rev ranges))+1 in 
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


   