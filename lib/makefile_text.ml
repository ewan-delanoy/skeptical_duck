(*

#use"lib/makefile_text.ml";;

*) 

exception List_value_exn of (int * ((string list) * int)) list ;;

exception Check_all_are_empty_but_last_exn of (int * (string list)) list ;;

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
  extract_ingredients_from_index mt_text left_offset;;

let located_ingredients_for_prefix mt_text prefix = 
  let (Makefile_text_t.MT text) = mt_text in 
  let pairs = Lines_in_string.occurrences_of_in_at_beginnings_of_lines prefix text in 
  Image.image (
   fun (c_idx,l_idx) -> 
    (l_idx,extract_ingredients_after_prefix_at_index mt_text prefix c_idx)
  ) pairs ;;

let located_ingredients_for_target mt_text target_name = 
   located_ingredients_for_prefix mt_text (target_name^":") ;;
 
 
let list_value mt_text ~variable_name = 
  let temp1 = located_ingredients_for_prefix mt_text (variable_name^" = ") in 
  if List.length(temp1)<>1
  then raise(List_value_exn(temp1))
  else List.hd temp1;;
 
 
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

let located_ingredients_and_recipes_for_target mt_text target_name = 
 Image.image (
  fun (start_idx,(ingredients,frontier_idx)) ->
     (start_idx,(ingredients,extract_recipes mt_text frontier_idx))
 ) (located_ingredients_for_target mt_text target_name) ;;
 
(*
   
let mt1 = Makefile_text_t.MT
  (String.concat "\n"
  ["arthur: a.txt"; "\t@echo \"This is Arthur 1\""; "belinda: b.txt";
  "\t@echo \"This is Belinda\""; "arthur: c.txt";
  "\t@echo \"This is Arthur 2\""; "arthur: d.txt";
  "\t@echo \"This is Arthur 3\"\t"; "a.txt:"; "\tcp origin.txt a.txt"; "b.txt:";
  "\tcp origin.txt b.txt\t"; "c.txt:"; "\tcp origin.txt c.txt\t"; "d.txt:";
  "\tcp origin.txt d.txt\t\t\t"; "clean:"; "\trm -f a.txt b.txt c.txt d.txt";
  "\ttouch what_gnu_make_did.txt \t"; ""] 
  ) ;; 

let see1 = located_ingredients_and_recipes_for_target 
    mt1 "arthur" ;;  

*)

let check_all_are_empty_but_last l =
   let (h,t) = List_again.head_with_tail(List.rev l) in
   if List.for_all (fun (_idx,recipes)->recipes=[]) t 
   then snd h
   else raise (Check_all_are_empty_but_last_exn l);;  

 let ingredients_and_recipes_for_target mt_text target_name = 
  let temp1 = located_ingredients_and_recipes_for_target mt_text target_name in 
  if temp1 = [] then ([],[]) else
  let temp2 = Image.image (fun (_start_idx,(ingr,_recipe))->ingr ) temp1
  and temp3 = Image.image (fun (start_idx,(_ingr,recipe))->(start_idx,recipe) ) temp1 in 
  let ingredients = List.flatten temp2
  and recipes = check_all_are_empty_but_last temp3 in 
  (ingredients,recipes) ;;


  let ingredients_for_target mt_text target_name = 
    fst(ingredients_and_recipes_for_target mt_text target_name) ;;

let recipes_for_target mt_text target_name = 
 snd(ingredients_and_recipes_for_target mt_text target_name) ;; 

 
let rec helper_for_target_list_expansion mt_text (treated,terminals,to_be_treated) = 
  match to_be_treated with 
  [] -> List.rev treated 
 |(target,already_visited) :: others ->
    if already_visited 
    then  helper_for_target_list_expansion mt_text (target::treated,terminals,others)
    else 
    let (ingredients,recipes)  = ingredients_and_recipes_for_target mt_text target in 
    if (ingredients,recipes)  = ([],[])
    then  helper_for_target_list_expansion mt_text (treated,target::terminals,others) 
    else 
    let old_ingredients = treated @ terminals in 
    let new_ingredients = List.filter (fun tgt ->not(List.mem tgt old_ingredients)) ingredients in 
    if new_ingredients = []
    then helper_for_target_list_expansion mt_text (target::treated,terminals,others)
    else
    let new_goal = (Image.image (fun x->(x,false)) new_ingredients)
                    @( (target,true) :: others) in 
    helper_for_target_list_expansion mt_text (treated,terminals,new_goal) ;;                    
   
let expand_target_list mt_text l = 
  helper_for_target_list_expansion mt_text ([],[],Image.image (fun x->(x,false)) l) ;;

end ;;

let expand_target_list = Private.expand_target_list ;; 

let ingredients_and_recipes_for_target = Private.ingredients_and_recipes_for_target ;;


let ingredients_for_target = Private.ingredients_for_target ;;

let list_value = Private.list_value ;;

let single_value = Private.single_value ;;

let snippet  = Private.makefile_snippet_for_command ;; 

