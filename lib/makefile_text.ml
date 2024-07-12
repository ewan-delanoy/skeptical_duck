(*

#use"lib/makefile_text.ml";;

*)


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

end ;;


let snippet  = Private.makefile_snippet_for_command ;; 

