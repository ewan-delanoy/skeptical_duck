(*

#use"lib/Text_editing/naive_footnote.ml";;

The most naive footnote format imaginable, reducing to (<Number>).

*)


module Private = struct 

let is_a_digit c = List.mem c 
    ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

let is_not_a_digit c = not(is_a_digit c) ;;

let seek_positive_integer_at_index text idx =
    if (idx<0)||(idx > (String.length text)) then None else 
    let c = String.get text (idx-1) in 
    if is_not_a_digit c then None else  
    let j_opt = String_find_char.from_inclusive_opt is_not_a_digit text idx in 
    let next_idx = (match j_opt with None -> (String.length text)+1 | Some j -> j) in 
    let written_integer = int_of_string(Cull_string.interval text idx (next_idx-1)) in 
    Some(written_integer,next_idx) ;;   

(*    
seek_positive_integer_at_index "abc725def" 3;;    
seek_positive_integer_at_index "abc725def" 4;;
seek_positive_integer_at_index "abc725def" 5;;
*)

let seek_substring_at_index substr text idx = 
    if Substring.is_a_substring_located_at substr text idx 
    then Some(idx+(String.length substr))
    else None ;;    

let seek_naive_reference_at_index text idx =     
  match seek_substring_at_index "(" text idx with 
  None -> None 
  | Some idx2 -> 
     (
      match seek_positive_integer_at_index text idx2 with 
           None -> None 
           | Some(written_integer,idx3) -> 
      (match seek_substring_at_index ")" text idx3 with 
      None -> None 
      | Some idx4 -> Some(written_integer,idx4)
     ));;

(*    
seek_naive_reference_at_index "1234(67) go marching in" 5;;    

*)

let rec helper_for_finding_naive_refs_after_index text (idx,text_length) =
   if idx>text_length 
   then None 
   else match seek_naive_reference_at_index text idx with 
        (Some(written_integer,next_idx)) -> Some(written_integer,idx,next_idx)
        | None -> helper_for_finding_naive_refs_after_index text (idx+1,text_length);;

let seek_naive_reference_after_index text idx =
  helper_for_finding_naive_refs_after_index text (idx,String.length text) ;; 
  
(*    
seek_naive_reference_after_index "1234(67) go marching in" 1;;    

*)  

let extract_next_content text previous_reference_end = 
  let next_reference_opt = seek_naive_reference_after_index text (previous_reference_end+1) in 
  let content_end=(match next_reference_opt with 
   None -> String.length text 
   |Some(_footnote_nbr,reference_start,_reference_end)->reference_start-1) in 
  (Cull_string.interval text previous_reference_end content_end,next_reference_opt) ;; 

(*    
extract_next_content "1234(67) when the saints(89) go marching in" 8;;    

*)   

let rec helper_for_naive_reference_collecting text (previous_references,previous_footnote_nbr,previous_reference_end) =
  let (previous_content,next_reference_opt) = extract_next_content text previous_reference_end in 
  let references =  (previous_footnote_nbr,previous_content)::previous_references in 
  match  next_reference_opt with 
  None -> List.rev references 
  |Some(footnote_nbr,_reference_start,reference_end)->
     helper_for_naive_reference_collecting text (references,footnote_nbr,reference_end);; 

let collect_naive_references text =
  match seek_naive_reference_after_index text 1 with 
  None -> (text,[])
  |Some(footnote_nbr,reference_start,reference_end) ->
    (Cull_string.beginning (reference_start-1) text ,
    helper_for_naive_reference_collecting text ([],footnote_nbr,reference_end)) ;;


(*

collect_naive_references "abc(7)de(2)fghij(5)klm" ;;
collect_naive_references "(7)de(2)fghij(5)klm(6)" ;;

*)

end ;; 

let collect = Private.collect_naive_references ;;