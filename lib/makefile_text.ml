(*

#use"lib/makefile_text.ml";;

*) (*


module Private = struct 

let makefile_lines_for_indexed_command (cmd_idx,(cmd_content,comment,is_commented_out)) = 
  let comment_box = (if is_commented_out then "#" else "") 
  and s_idx = string_of_int cmd_idx in 
  [
  comment_box^
  "\t@echo \"************************************************ Step "^s_idx^":"^comment^"\"";
  comment_box^
  "\t"^cmd_content   
  ] ;; 

let makefile_snippet_for_command ~command_name ~recipes = 
 let indexed_recipes = Int_range.index_everything recipes in 
 "\n"^command_name^":\n" ^
 (String.concat "\n"
 (List.flatten (Image.image makefile_lines_for_indexed_command indexed_recipes ))) ^ "\n";;   



 let add_interval_if_nonempty (treated,text,starter,current_idx)=
 if starter > current_idx 
 then treated 
 else (Cull_string.interval text starter current_idx) :: treated ;;   
 
     

let rec helper_for_ingredients_extractor (treated,text,starter,current_idx,text_length) = 
 if current_idx > text_length 
 then  (List.rev(add_interval_if_nonempty (treated,text,starter,text_length)),text_length + 1)
 else
 let c = Strung.get text current_idx in 
 if not(List.mem c [' ';'\t';'\r';'\n'])   
 then  helper_for_ingredients_extractor (treated,text,starter,current_idx+1,text_length) 
 else 
 let treated2 = add_interval_if_nonempty (treated,text,starter,current_idx-1) in     
 if c <> '\n'
 then  helper_for_ingredients_extractor (treated2,text,current_idx+1,current_idx+1,text_length) 
 else   
 if (Strung.get text (current_idx-1))<>'\\'
 then (List.rev treated2,current_idx+1)  
 else     
 let treated3 = add_interval_if_nonempty (treated,text,starter,current_idx-2) in     
 helper_for_ingredients_extractor (treated3,text,current_idx+1,current_idx+1,text_length) ;;
   
   
 let extract_ingredients_from_index (Makefile_text_t.MT text) idx = 
     helper_for_ingredients_extractor ([],text,idx,idx,String.length text)  ;;
   
(*  
 extract_ingredients ("123ab\\\n\tcdef \tghi\njk") 3;;
*)

let extract_ingredients_after_prefix_at_index mt_text prefix idx= 
  let left_offset = idx + (String.length prefix) in 
  extract_ingredients_from_index mt_text left_offset ;;

let ingredients_for_target mt_text target_name = 
  extract_ingredients_after_prefix mt_text (target_name^":") ;;
 
 
let list_value mt_text ~variable_name = 
  extract_ingredients_after_prefix mt_text (variable_name^" = ") ;;
 
 
let single_value mt_text ~variable_name = 
  let (idx1,(temp1,_)) = list_value mt_text ~variable_name in 
  (idx1,List.hd temp1) ;;


let rec helper_for_recipes_extractor (treated,text,starter,current_idx,text_length) = 
    if current_idx > text_length 
    then  List.rev(add_interval_if_nonempty (treated,text,starter,text_length))
    else
    if ((Strung.get text current_idx)<>'\n')||(Strung.get text (current_idx-1)='\\')
    then  helper_for_recipes_extractor (treated,text,starter,current_idx+1,text_length) 
    else
    let treated2 = add_interval_if_nonempty (treated,text,starter,current_idx-1) in   
    if ( (Strung.get text (current_idx+1))<>'\t' ) || (current_idx+2 > text_length)
    then  List.rev treated2
    else 
    helper_for_recipes_extractor (treated2,text,current_idx+2,current_idx+2,text_length) ;;
  
let extract_recipes (Makefile_text_t.MT text) idx = 
    helper_for_recipes_extractor ([],text,idx,idx,String.length text)  ;;
  
  (*  
  extract_recipes ("123ab\n\tcdef\n\tghi\njk") 3;;
  *)

let ingredients_and_recipes_for_target mt_text target_name = 
 let (_,(ingredients,frontier_idx)) = ingredients_for_target mt_text target_name in 
 (ingredients,extract_recipes mt_text frontier_idx) ;;    

let recipes_for_target mt_text target_name = 
 snd(ingredients_and_recipes_for_target mt_text target_name) ;; 

end ;;

let ingredients_and_recipes_for_target = Private.ingredients_and_recipes_for_target ;;


let ingredients_for_target = Private.ingredients_for_target ;;

let list_value = Private.list_value ;;

let single_value = Private.single_value ;;

let snippet  = Private.makefile_snippet_for_command ;; *)

