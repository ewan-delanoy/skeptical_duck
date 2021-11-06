(*

#use"Text_editing/mark_comments_for_copying_or_deletion.ml";;

*)

exception Open_close_mismatch of int * int ;;
exception Overlapping_intervals of int * int * int * int ;;

module Private = struct

let opener = "(*#" ;;
let closer = "#*)" ;;

let is_an_opening_line line = 
    Supstring.begins_with 
     (Cull_string.trim_spaces_on_the_left line) opener ;; 
let is_a_closing_line line = 
  Supstring.ends_with  
  (Cull_string.trim_spaces_on_the_right line) closer ;; 
          
let check_for_open_close_mismatch openers closers = 
  let num_of_openers = List.length openers 
  and num_of_closers = List.length closers in 
  if num_of_openers  <> num_of_closers 
  then raise(Open_close_mismatch(num_of_openers,num_of_closers))
  else () ;; 

let check_for_overlapping_intervals indexed_lines pairs =
  let temp1 = Listennou.universal_delta_list pairs in 
  match Option.seek (
    fun ((i1_open,i2_close),(i3_open,i4_close)) -> 
      i3_open < i2_close
  ) temp1 with 
   (Some((i1_open,i2_close),(i3_open,i4_close)))->
      let indices_in_order = 
        Ordered.sort Total_ordering.standard  
         [i1_open;i2_close;i3_open;i4_close] in
      let passage =
        String.concat "\n" (Image.image
           (fun idx->List.assoc idx indexed_lines) indices_in_order
        ) in 
      let msg = "\n\n\nOffending passage :\n\n\n"^passage^"\n\n\n" in 
      let _=(print_string msg; flush stdout) in 
      raise(Overlapping_intervals(i1_open,i2_close,i3_open,i4_close)) 
  | None -> ();;     



let ranges_for_marked_comments text =
   let indexed_lines = Lines_in_string.core text in 
   let openers = Option.filter_and_unpack (
      fun (line_idx,line) ->
       if is_an_opening_line line
       then Some(line_idx)
       else None   
   )  indexed_lines 
   and closers = Option.filter_and_unpack (
    fun (line_idx,line) ->
     if is_a_closing_line line
     then Some(line_idx)
     else None   
 )  indexed_lines in   
 let _ = check_for_open_close_mismatch openers closers in 
 let answer = List.combine openers closers in 
 let _ = check_for_overlapping_intervals indexed_lines answer in 
  answer ;;

(*  

ranges_for_marked_comments
"1\n2\n(*# 3\n4 *)\n(*# 5 \n6\n7 #*)\n(* 8\n9\n10 #*)\n11\n12" ;;


ranges_for_marked_comments
"1\n2\n(*# 3\n4\n5 #*)\n6\n(*# 7\n8\n9\n10 #*)\n11\n12" ;;

*)

let unmark_opening_line line = 
   let idx = Substring.leftmost_index_of_in opener line in 
   Cull_string.cobeginning (idx+(String.length opener)-1) line ;;

(*

unmark_opening_line "abc(*#defghi" ;;

*)

let unmark_closing_line line = 
    let idx = Substring.rightmost_index_of_in closer line in 
    Cull_string.coending (idx-1) line ;;

(*

unmark_closing_line "abc#*)defghi" ;;

*)    

let rec unmark_line_according_to_rangelist 
  ranges (line_idx,line)  = 
  match ranges with  
    [] -> line 
  | (opening_idx,closing_idx) :: other_ranges ->
     if line_idx < opening_idx 
     then line
     else  
     if line_idx = opening_idx 
     then unmark_opening_line line 
     else 
     if line_idx < closing_idx 
     then line
     else 
     if line_idx = closing_idx 
     then unmark_closing_line line      
     else unmark_line_according_to_rangelist 
     other_ranges (line_idx,line) ;;

let copy_between_strings src old_dest =
   let ranges = ranges_for_marked_comments src in 
   let copier = (
     fun dest1 pair ->
      Lines_in_string.copy_interval_from_string_to_string 
        pair src dest1
   ) in 
   let temp_dest = 
      List.fold_left copier old_dest ranges in  
   let indexed_lines = Lines_in_string.core temp_dest in 
   let final_lines = Image.image 
     (unmark_line_according_to_rangelist ranges) indexed_lines in 
   String.concat "\n" final_lines ;;   

(*

copy_between_strings
"1\n2\n(*# 3\n4\n5 #*)\n6\n(*# 7\n8\n9\n10 #*)\n11\n12"
"a\nb\nc\nd\ne\nf\ng" ;;

*)   

let delete_in_string src =
  let ranges = ranges_for_marked_comments src in 
  let indexed_lines = Lines_in_string.core src in 
  let is_to_be_kept = (fun idx ->
    List.for_all (fun (a,b)->(idx<a)||(b<idx)) ranges
  ) in 
  let kept_lines = Option.filter_and_unpack (
    fun (linedex,line) ->
       if is_to_be_kept linedex 
       then Some line 
       else None 
  ) indexed_lines  in 
  String.concat "\n" kept_lines ;;  

(*

delete
"1\n2\n(*# 3\n4\n5 #*)\n6\n(*# 7\n8\n9\n10 #*)\n11\n12"
 ;;

*)   

end ;;


let copy src_file dest_file = 
  let old_src = Io.read_whole_file src_file
  and old_text = Io.read_whole_file dest_file in
  let new_text = Private.copy_between_strings old_src old_text  in
  Io.overwrite_with dest_file new_text ;;  

let delete file = 
    let old_text = Io.read_whole_file file in
    let new_text = Private.delete_in_string old_text  in
    Io.overwrite_with file new_text ;;    


