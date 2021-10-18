(*

#use "Githubbed_archive/prepare_fw_with_dependencies.ml";;

*)

open Needed_values ;;

let submodules = [
   "Modularized_details";
   "Order";
   "Needed_dirs";
   "Needed_libs";
   "All_subdirectories";
   "All_printables"  
] ;; 

(*

Each uple in the list is as follows :

method_name (string),
old_fw_present (bool),
list of other args (string list),
optional returned data (string option)

*)


let methods = [
   "empty_one",false,["config"],None;
   "forget_modules",true,["mods_to_be_erased"],None;
   "inspect_and_update",true,[],Some "extra";
   "of_concrete_object",false,["crobj"],None;
   "of_configuration",false,["config"],None;
   "of_configuration_and_list",false,["pair"],None;
   "overwrite_file_if_it_exists",true,["pair"],Some "extra";
   "reflect_latest_changes_in_github",true,["opt_msg"],None;
   "register_rootless_paths",true,["rootlesses"],Some "extra";
   "relocate_module_to",true,["pair"],Some "extra";
   "remove_files",true,["files_to_be_removed"],Some "extra";
   "rename_module",true,["triple"],Some "extra";
   "rename_subdirectory_as",true,["pair"],Some "extra";
   "replace_string",true,["pair"],Some "extra";
   "replace_value",true,["pair"],Some "extra";
   "set_gitpush_after_backup",true,["yes_or_no"],None;
   "set_last_noticed_changes",true,["diff"],None;
] ;;

module Cached = struct 

let list_of_arguments (meth_name,ancestor_is_here,other_args,opt_extra) opt_parent =
    let ancestor_part = (
       if ancestor_is_here 
       then opt_parent^" "  
       else ""
    ) in 
    ancestor_part^(String.concat " " other_args);;

let data_from_opt_extra opt_extra =
   match opt_extra with 
   None -> (" { "," } ;; ","new_parent")
   |Some(extra) -> (" ({ "," },"^extra^" ) ;; ","(new_parent,"^extra^")") ;;

let optional_line_for_old_parent meth_uple =
   let (meth_name,ancestor_is_here,other_args,opt_extra) = meth_uple in 
   if ancestor_is_here 
   then [
         " let old_parent = parent old_fw in ";
        ]  
   else [];;   


let index_determination meth_uple =
   let (meth_name,ancestor_is_here,other_args,opt_extra) = meth_uple in 
   if ancestor_is_here 
   then [
         " let instance_idx = fst( index old_fw ) in ";
         " let _ = Fw_indexer.push_state instance_idx in "
        ]  
   else [
         " let instance_idx = Fw_indexer.create_new_instance () in "
        ];;


   


let cached meth_uple =
   let (meth_name,ancestor_is_here,other_args,opt_extra) = meth_uple in
   let (left,right,old_res) = data_from_opt_extra opt_extra 
   and args = list_of_arguments meth_uple in 
   String.concat "\n" 
   ([ 
      "let "^meth_name^" "^(args "old_fw")^" =  ";
   ]
   @(optional_line_for_old_parent meth_uple)
   @[    
      " let "^old_res^" = Fw_with_small_details."^meth_name^" "^(args "old_parent")^" in ";
   ]
   @(index_determination meth_uple)
   @[   
      left;
      "   Fw_with_dependencies_t.parent = new_parent ;";
      "   index_for_caching = expand_index instance_idx ;";
      right
   ]) ;;   

   let full_text () =
      let all_meth_lines = Image.image cached methods in 
         "module Cached = struct \n\n"^
         (String.concat "\n\n" all_meth_lines)^
         "end ;;"
      ;;


end ;;   




let text_for_submodule sumo =      
   String.concat "\n" 
   [
      "module "^(sumo)^" = struct \n";
      " let the_hashtbl = ((Hashtbl.create 10)) ;; ";
      "end ;;"
   ];;

let text_for_all_subdmodules () = 
   let temp1 =
      (Cached.full_text ()) 
      ::(Image.image text_for_submodule submodules) in 
   "\n\n\n"^(String.concat "\n\n\n" temp1)^"\n\n\n" ;;
    
let prelude = String.concat "\n" [
  "let expand_index idx = (idx,Fw_indexer.get_state idx) ;;";
  "let index fw = fw.Fw_with_dependencies_t.index_for_caching ;; ";  
  "let parent fw = fw.Fw_with_dependencies_t.parent ;;";
] ;;

let write_all () =
   let text = "\n\n"^prelude^"\n\n"^(text_for_all_subdmodules ()) 
   and file = Absolute_path.of_string "Fads/sirloin.ml"
   and beg_mark = "(* Beginning of sirloin *)"
   and end_mark = "(* End of sirloin *)" in 
   Replace_inside.overwrite_between_markers_inside_file 
     (Overwriter.of_string text) (beg_mark,end_mark) file ;;
     



