(*

#use"lib/Text_editing/make_paragraphs_one_lined.ml";;

*)

module Private = struct 

let rec find_next_delimiter (treated,to_be_treated) =
    match to_be_treated with 
    [] -> (treated,None,[])
    |elt :: other_elts ->
       (
         match elt with 
         |Str.Text(t) -> find_next_delimiter (elt ::treated,other_elts)
         |Str.Delim(d) ->  (treated,Some elt,other_elts)
       ) ;;

exception NDM_exn ;;

let rec helper_for_nondelimiters_merging (treated,to_be_treated) =
    let (reversed_left,opt_delim,right) = find_next_delimiter ([],to_be_treated) in 
    let texts=(List.rev_map (function Str.Text(x)->x |_->raise(NDM_exn)) reversed_left) in 
    let merged_text = Str.Text(String.concat "" texts) in 
    match opt_delim with 
    None -> List.rev(merged_text::treated) 
    |Some delim -> helper_for_nondelimiters_merging (delim::merged_text::treated,right) ;; 

let merge_nondeliminters l = helper_for_nondelimiters_merging ([],l) ;; 

let compute_paragraphs text =
  let temp1 = Str.full_split (Str.regexp "[ \t\n\r]+") text in 
  let temp2 = Image.image (
     function elt -> match elt with 
      |Str.Text(t) -> elt 
      |Str.Delim(d) ->
        if Strung.number_of_linebreaks(d)>=2 
        then elt  
        else Str.Text(d)    
  ) temp1 in 
  merge_nondeliminters temp2 ;;

let in_string text = 
   let parts = compute_paragraphs text  in 
   let compressed_parts = Image.image (
    function 
     |Str.Text(t) -> Replace_inside.silently_replace_inside_string ("\n"," ") t
     |Str.Delim(d) -> d
  ) parts in  
  String.concat "" compressed_parts ;; 

end ;;


let in_file file = 
  let old_text = Io.read_whole_file file in
  let new_text = Private.in_string old_text  in
  Io.overwrite_with file new_text ;;  

let in_string = Private.in_string ;;  

(* in_string "1\n2\n3\n\t\n\r\n\r\n4\n5\n6\n7" ;; *)  

