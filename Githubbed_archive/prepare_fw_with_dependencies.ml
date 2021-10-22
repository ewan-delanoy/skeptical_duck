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
   "rename_module_on_filename_level_and_in_files",true,["triple"],Some "extra";
   "rename_subdirectory_as",true,["pair"],Some "extra";
   "replace_string",true,["pair"],Some "extra";
   "replace_value",true,["pair"],Some "extra";
   "set_gitpush_after_backup",true,["yes_or_no"],None;
   "set_last_noticed_changes",true,["diff"],None;
] ;;

module Common = struct 

let list_of_arguments (meth_name,ancestor_is_here,other_args,opt_extra) opt_parent =
   let ancestor_part = (
      if ancestor_is_here 
      then opt_parent^" "  
      else ""
   ) in 
   ancestor_part^(String.concat " " other_args);;
   
end ;;


module Cached = struct 

let optional_line_for_old_parent meth_uple =
   let (meth_name,ancestor_is_here,other_args,opt_extra) = meth_uple in 
   if ancestor_is_here 
   then [
         " let old_parent = parent old_fw in ";
        ]  
   else [];;   

let data_from_opt_extra opt_extra new_fw_name=
   match opt_extra with 
    None -> (" { "," } ;; ",new_fw_name)
   |Some(extra) -> (" ({ "," },"^extra^" ) ;; ","("^new_fw_name^","^extra^")") ;;

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


   


let text_for_method meth_uple =
   let (meth_name,ancestor_is_here,other_args,opt_extra) = meth_uple in
   let (left,right,old_res) = data_from_opt_extra opt_extra "new_parent"
   and args = Common.list_of_arguments meth_uple in 
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
      let all_meth_lines = Image.image text_for_method methods in 
         "module Cached = struct \n\n"^
         (String.concat "\n\n" all_meth_lines)^
         "end ;;"
      ;;


end ;;   

module Modular = struct 

let preceding_module modname =
     let k = Listennou.find_index modname submodules in 
     if k = 1 then "Cached" else List.nth submodules (k-2) ;;  

let data_from_opt_extra opt_extra rest_of_line addendum_modifier=
     match opt_extra with 
      None -> ([
                 " let new_fw = "^rest_of_line
               ]," new_fw")
     |Some(extra) -> 
          if addendum_modifier=""
          then ([
                 " let visible = "^rest_of_line;
                 " let (new_fw,"^extra^") = visible in "
               ]," visible")
          else ([
            " let (new_fw,"^extra^") = "^rest_of_line
               ]," (new_fw,"^addendum_modifier^extra^")") ;;


let text_for_method modname meth_uple modmeth_uple =
   let (meth_name,ancestor_is_here,other_args,opt_extra) = meth_uple 
   and (lines_before_answer,addendum_modifier) = modmeth_uple in
   let args = Common.list_of_arguments meth_uple 
   and prmod = preceding_module modname in 
   let rest_of_line = prmod^"."^meth_name^" "^(args "old_fw")^" in " in 
   let (introducing_new_fw,final_answer) = 
       data_from_opt_extra opt_extra rest_of_line addendum_modifier in 
   String.concat "\n" 
   ([ 
      "let "^meth_name^" "^(args "old_fw")^" =  ";   
   ]
   @introducing_new_fw
   @lines_before_answer   
   @[ 
      " let _ = Hashtbl.add the_hashtbl (index new_fw) answer in ";
      final_answer^" ;;"
   ]) ;;

end ;;   


let cartesian_ref = ref ([]:((string * string) * (string list * string)) list) ;;

let add_to_cartesian submod methd data=
  (cartesian_ref:=((submod,methd),data)::(!cartesian_ref)) ;;

exception Seek_in_cartesian of string ;;

let seek_in_cartesian submod methd =
   try List.assoc (submod,methd) (!cartesian_ref) with 
   _ -> raise(Seek_in_cartesian(submod));;

exception Seek_method of string ;;
    
let seek_method methd =
   try Listennou.force_find (fun (methd2,_,_,_)->methd2 = methd) methods with 
   _ -> raise(Seek_method(methd));;


let text_from_cartesian modname methd = 
   let meth_uple = seek_method methd 
   and modmeth_uple = seek_in_cartesian modname methd in 
   Modular.text_for_method modname meth_uple modmeth_uple
;; 


let ghetto_ref = ref ([]: (string * string list) list) ;;

let add_to_ghetto submod lines=
    (ghetto_ref:=(submod,lines)::(!ghetto_ref)) ;;  

exception Seek_in_ghetto of string ;;

let seek_in_ghetto submod =
     try List.assoc submod (!ghetto_ref) with 
     _ -> raise(Seek_in_ghetto(submod));;

let text_from_ghetto submod = 
   let lines = seek_in_ghetto submod in 
   let (first_line,other_lines) = Listennou.ht lines in 
   let all_lines = 
      (" let force_get fw = "^first_line) ::
      (other_lines @
      [
        " let get fw = " ;
        "   let idx = index fw in " ; 
        "   match Hashtbl.find_opt the_hashtbl idx with " ;
        "      Some(old_answer)-> old_answer " ;
        "     | None -> ";
        "   let answer = force_get fw in "; 
        "   let _ = (Hashtbl.add the_hashtbl idx answer) in ";
        "   answer ;; "
      ]) in 
   all_lines;;    

add_to_ghetto
   "Modularized_details"
   ["Fw_module_small_details.modularize_details (parent fw)"];;
 
let mod_details = add_to_cartesian "Modularized_details";;

mod_details "empty_one" ([" let answer = [] in "],"") ;;

mod_details "forget_modules" ([
   " let old_val = get old_fw in ";
   " let answer = List.filter (fun (mn,_)->not(List.mem mn mods_to_be_erased)) old_val in "
],"") ;;

mod_details "inspect_and_update" ([
   " let old_val = get old_fw in ";
   " let ((a_files,u_files,nc_files),changed_u_files) = extra in ";
   " let tempf = (";
   "   fun old_pair ->";
   "    let (mn,details) = old_pair in "; 
   "    let temp1 = List.filter (fun (rl,details2)->";
   "       (Dfn_rootless.to_module rl)= mn";
   "      ) changed_u_files in ";
   " if temp1 <> []";
   " then (mn, Fw_module_small_details.compute_details_from_acolytes_list_for_one_module temp1)";
   "    else old_pair"; 
   " ) in "; 
   " let answer = Image.image tempf old_val in "
],"") ;;

mod_details "of_concrete_object" ([
   " let answer = force_get new_fw in "
],"") ;;

mod_details "of_configuration" ([
   " let answer = force_get new_fw in "
],"") ;;

mod_details "of_configuration_and_list" ([
   " let answer = force_get new_fw in "
],"") ;;

mod_details "overwrite_file_if_it_exists" ([
   " let old_val = get old_fw in ";
   " let answer = ( match extra with ";
   "      None -> old_val ";
   "      |Some(change) ->";
   " let tempf = (";
   "        fun old_pair -> ";
   "          let (mn,details) = old_pair in ";
   "          let temp1 = List.filter (fun (rl,details2)->";
   "             (Dfn_rootless.to_module rl)= mn";
   "            ) [change] in";
   "          if temp1 <> []";
   "          then let new_parent = parent new_fw in ";
   "               (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)";
   "          else old_pair ";
   "      ) in ";
   " Image.image tempf old_val) in ";
],"") ;;

mod_details "reflect_latest_changes_in_github" ([
   "  let answer = get old_fw in "
],"") ;;

mod_details "register_rootless_paths" ([
   "  let old_val = get old_fw in ";
   "  let ((a_files,u_files,nc_files),new_details) = extra in";
   "  let involved_mods = Image.image Dfn_rootless.to_module rootlesses in";
   "  let (overlapping,nonoverlapping) = List.partition (";
   "     fun (rl,_) -> List.mem (Dfn_rootless.to_module rl) involved_mods ";
   "  ) new_details in ";
   "  let tempf1 = (";
   "    fun old_pair -> ";
   "      let (mn,details) = old_pair in ";
   "      let temp1 = Option.filter_and_unpack (fun (rl,details2)->";
   "         if (Dfn_rootless.to_module rl)= mn";
   "         then Some(rl,Some(rl,details2))";
   "         else None ";
   "        ) overlapping in";
   "      if temp1 <> []";
   "      then let new_parent = parent new_fw in ";
   "           (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)";
   "      else old_pair ";
   "  ) in ";
   "  let answer = (Image.image tempf1 old_val)@";
   "  (Fw_module_small_details.compute_details_from_acolytes_list_for_several_modules nonoverlapping) in ";
],"") ;;

let mod_details_usual_preliminary = [
   " let old_val = get old_fw in ";
   " let tempf = (";
   "   fun old_pair -> ";
   "     let (mn,details) = old_pair in ";
   "     let temp1 = List.filter (fun (rl,new_pair_for_rl)->";
   "        (Dfn_rootless.to_module rl)= mn";
   "       ) extra in";
   "     if temp1 <> []";
   "     then let new_parent = parent new_fw in ";
   "          (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)";
   "     else old_pair ";
   " ) in ";
   " let answer = Image.image tempf old_val in ";
] ;;


mod_details "relocate_module_to" (mod_details_usual_preliminary,"") ;;

mod_details "remove_files" (mod_details_usual_preliminary,"") ;;

mod_details "rename_module_on_filename_level_and_in_files" (mod_details_usual_preliminary,"") ;;

mod_details "rename_subdirectory_as" (mod_details_usual_preliminary,"") ;;

mod_details "replace_string" (mod_details_usual_preliminary,"") ;;

mod_details "replace_value" (mod_details_usual_preliminary,"") ;;

mod_details "set_gitpush_after_backup" ([
   "  let answer = get old_fw in "
],"") ;;

mod_details "set_last_noticed_changes" ([
   "  let answer = get old_fw in "
],"") ;;


add_to_ghetto
   "Order"
   ["Fw_determine_order.main (Modularized_details.get fw)"];;
 
let order = add_to_cartesian "Order";;

order "empty_one" ([" let answer = [] in "],"") ;;

order "forget_modules" ([
   " let old_val = get old_fw in ";
   " let answer = List.filter (fun (mn,_)->not(List.mem mn mods_to_be_erased)) old_val in "
],"") ;;

let order_usual_preliminary = [
   " let old_val = get old_fw in ";
   " let modules_in_old_order = Image.image fst old_val in ";
   " let details_in_old_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles";
   " (Modularized_details.get new_fw) modules_in_old_order in ";
   " let answer = Fw_determine_order.main  details_in_old_order in ";
] ;;

order "inspect_and_update" (order_usual_preliminary,"") ;;

order "of_concrete_object" ([
   " let answer = force_get new_fw in "
],"") ;;

order "of_configuration" ([
   " let answer = force_get new_fw in "
],"") ;;

order "of_configuration_and_list" ([
   " let answer = force_get new_fw in "
],"") ;;

order "overwrite_file_if_it_exists" (order_usual_preliminary,"") ;;

order "reflect_latest_changes_in_github" ([
   "  let answer = get old_fw in "
],"") ;;

order "register_rootless_paths" ([
   " let old_val = get old_fw in ";
   " let extended_details_list = Modularized_details.get new_fw in ";
   " let new_details = Listennou.big_tail (List.length old_val) extended_details_list in";
   " let new_modules_in_order = Image.image fst (Fw_determine_order.main new_details) in ";
   " let new_details_in_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles";
   "     new_details new_modules_in_order in ";
   " let answer = Fw_determine_order.compute_coatoms_and_ancestors_in_small_extension";
   "      old_val new_details_in_order in ";
],"") ;;

order "relocate_module_to" ([
  "  let answer = get old_fw in "
],"") ;;

order "remove_files" ([
  " let answer = force_get new_fw in "
],"") ;;

order "rename_module_on_filename_level_and_in_files" ([
  " let old_val = get old_fw in ";
  " let (old_mname,new_mname,_) = triple in";
  " let rep = (fun mn->if mn = old_mname then new_mname else mn) in  ";
  " let answer = Image.image (fun (mn2,(coat_mn2,ancestors_mn2)) ->";
  "     (rep mn2,(Image.image rep coat_mn2,Image.image rep ancestors_mn2))";
  " ) old_val in ";
],"") ;;

order "rename_subdirectory_as" ([
  "  let answer = get old_fw in "
],"") ;;

order "replace_string" (order_usual_preliminary,"") ;;

order "replace_value" (order_usual_preliminary,"") ;;

order "set_gitpush_after_backup" ([
   "  let answer = get old_fw in "
],"") ;;

order "set_last_noticed_changes" ([
   "  let answer = get old_fw in "
],"") ;;

 add_to_ghetto
   "Needed_dirs" 
   [   "let details = Modularized_details.get fw in ";
    " let subdir_at_module = (fun mn->";
    "   Fw_module_small_details.subdirectory(List.assoc mn details)";
    " ) in ";
    " Image.image (";
    "  fun (mn,(_,ancestors)) ->";
    "   let temp1 = Image.image subdir_at_module (mn::ancestors) in ";
    "   (mn,Ordered.sort Total_ordering.standard temp1)"; 
    ") (Order.get fw) ;;"];;
 
let needed_dirs = add_to_cartesian "Needed_dirs";;

needed_dirs "empty_one" ([" let answer = [] in "],"") ;;

needed_dirs "forget_modules" ([
  " let answer = force_get new_fw in "
],"") ;;


needed_dirs "inspect_and_update" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_dirs "of_concrete_object" ([
   " let answer = force_get new_fw in "
],"") ;;

needed_dirs "of_configuration" ([
   " let answer = force_get new_fw in "
],"") ;;

needed_dirs "of_configuration_and_list" ([
   " let answer = force_get new_fw in "
],"") ;;

needed_dirs "overwrite_file_if_it_exists" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_dirs "reflect_latest_changes_in_github" ([
   "  let answer = get old_fw in "
],"") ;;

needed_dirs "register_rootless_paths" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_dirs "relocate_module_to" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_dirs "remove_files" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_dirs "rename_module_on_filename_level_and_in_files" ([
  " let old_val = get old_fw in ";
  " let (old_mname,new_mname,_) = triple in";
  " let rep = (fun mn->if mn = old_mname then new_mname else mn) in ";
  " let answer = Image.image (fun (mn2,sdirs) -> (rep mn2,sdirs)) old_val in ";
],"") ;;

needed_dirs "rename_subdirectory_as" ([
  " let old_val = get old_fw in ";
  " let rep = (fun sdir ->";
   "   match Dfa_subdirectory.soak pair sdir with ";
   "   None -> sdir ";
   "   |Some new_sdir -> new_sdir   ";
   " ) in ";
   " let answer = Image.image (fun (mn,sdirs)->(mn,Image.image rep sdirs) ) old_val in ";
],"") ;;

needed_dirs "replace_string" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_dirs "replace_value" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_dirs "set_gitpush_after_backup" ([
   "  let answer = get old_fw in "
],"") ;;

needed_dirs "set_last_noticed_changes" ([
   "  let answer = get old_fw in "
],"") ;;

 add_to_ghetto
    "Needed_libs" 
    [   "let details = Modularized_details.get fw in ";
     " let needed_libs_at_module = (fun mn->";
     "   Fw_module_small_details.used_libraries(List.assoc mn details)";
     " ) in ";
     " Image.image (";
     "  fun (mn,(_,ancestors)) ->";
     "   let temp1 = List.flatten(Image.image needed_libs_at_module (mn::ancestors)) in ";
     "   (mn,Ordered.sort Total_ordering.standard temp1)"; 
     ") (Order.get fw) ;;"];;

let needed_libs = add_to_cartesian "Needed_libs";;     

needed_libs "empty_one" ([" let answer = [] in "],"") ;;

needed_libs "forget_modules" ([
  " let answer = force_get new_fw in "
],"") ;;

let needed_dirs_usual_preliminary = [
   
] ;;

needed_libs "inspect_and_update" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_libs "of_concrete_object" ([
   " let answer = force_get new_fw in "
],"") ;;

needed_libs "of_configuration" ([
   " let answer = force_get new_fw in "
],"") ;;

needed_libs "of_configuration_and_list" ([
   " let answer = force_get new_fw in "
],"") ;;

needed_libs "overwrite_file_if_it_exists" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_libs "reflect_latest_changes_in_github" ([
   "  let answer = get old_fw in "
],"") ;;

needed_libs "register_rootless_paths" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_libs "relocate_module_to" ([
  "  let answer = get old_fw in "
],"") ;;

needed_libs "remove_files" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_libs "rename_module_on_filename_level_and_in_files" ([
  " let old_val = get old_fw in ";
  " let (old_mname,new_mname,_) = triple in";
  " let rep = (fun mn->if mn = old_mname then new_mname else mn) in ";
  " let answer = Image.image (fun (mn2,libs) -> (rep mn2,libs)) old_val in ";
],"") ;;

needed_libs "rename_subdirectory_as" ([
  "  let answer = get old_fw in "
],"") ;;

needed_libs "replace_string" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_libs "replace_value" ([
  " let answer = force_get new_fw in "
],"") ;;

needed_libs "set_gitpush_after_backup" ([
   "  let answer = get old_fw in "
],"") ;;

needed_libs "set_last_noticed_changes" ([
   "  let answer = get old_fw in "
],"") ;;

add_to_ghetto
     "All_subdirectories" 
     [   
       " let details = Modularized_details.get fw in ";
       " Ordered.sort Total_ordering.standard (Image.image (";
       "  fun (mn,details_on_mn) ->";
       "  Fw_module_small_details.subdirectory(details_on_mn)";
       ") details) ;;"
     ];;
 
let all_subdirectories = add_to_cartesian "All_subdirectories";;     

all_subdirectories "empty_one" ([" let answer = [] in "],"") ;;

all_subdirectories "forget_modules" ([
  " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "inspect_and_update" ([
  " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "of_concrete_object" ([
   " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "of_configuration" ([
   " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "of_configuration_and_list" ([
   " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "overwrite_file_if_it_exists" ([
  " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "reflect_latest_changes_in_github" ([
   "  let answer = get old_fw in "
],"") ;;

all_subdirectories "register_rootless_paths" ([
  " let old_val = get old_fw in ";
  " let (_,novelties) = extra in ";
  " let possibly_new = Ordered.sort Total_ordering.standard ";
  "   (Image.image (fun (rl,dets)->Dfn_rootless.to_subdirectory rl  ) novelties) in ";
  " let answer = Ordered.merge Total_ordering.standard possibly_new old_val in ";
],"") ;;

all_subdirectories "relocate_module_to" ([
  "  let answer = get old_fw in "
],"") ;;

all_subdirectories "remove_files" ([
  " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "rename_module_on_filename_level_and_in_files" ([
  "  let answer = get old_fw in "
],"") ;;

all_subdirectories "rename_subdirectory_as" ([
   " let old_val = get old_fw in ";
   " let rep = (fun sdir ->";
   "   match Dfa_subdirectory.soak pair sdir with ";
   "   None -> sdir ";
   "   |Some new_sdir -> new_sdir   ";
   " ) in ";
   " let answer = Image.image rep old_val in ";
],"") ;;

all_subdirectories "replace_string" ([
  " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "replace_value" ([
  " let answer = force_get new_fw in "
],"") ;;

all_subdirectories "set_gitpush_after_backup" ([
   "  let answer = get old_fw in "
],"") ;;

all_subdirectories "set_last_noticed_changes" ([
   "  let answer = get old_fw in "
],"") ;;

 add_to_ghetto
    "All_printables"
    [
      " let mods_without_subdirs = Option.filter_and_unpack (";
      "  fun (mn,details) ->";
      " if Fw_module_small_details.has_printer details";
      "  then Some mn"; 
      "  else None";  
      " ) (Modularized_details.get fw)"; 
      " and main_table = Modularized_details.get fw in ";
      " Image.image (";
      "    fun mn ->";
      "      let details = List.assoc mn main_table in ";
      "      let subdir = Fw_module_small_details.subdirectory details in ";
      "      Dfn_join.subdirectory_to_module subdir mn"; 
      " ) mods_without_subdirs ;;" 
    ] ;;
  
let all_printables = add_to_cartesian "All_printables";;

all_printables "empty_one" ([" let answer = [] in "],"") ;;

all_printables "forget_modules" ([
  " let old_val = get old_fw in ";
  " let answer = List.filter (fun middle->";
  "    not(List.mem (Dfn_middle.to_module middle) mods_to_be_erased)) old_val in "
],"") ;;

all_printables "inspect_and_update" ([
  " let answer = force_get new_fw in "
],"") ;;

all_printables "of_concrete_object" ([
   " let answer = force_get new_fw in "
],"") ;;

all_printables "of_configuration" ([
   " let answer = force_get new_fw in "
],"") ;;

all_printables "of_configuration_and_list" ([
   " let answer = force_get new_fw in "
],"") ;;

all_printables "overwrite_file_if_it_exists" ([
  " let answer = force_get new_fw in "
],"") ;;

all_printables "reflect_latest_changes_in_github" ([
   "  let answer = get old_fw in "
],"") ;;

all_printables "register_rootless_paths" ([
  " let answer = force_get new_fw in "
],"") ;;

all_printables "relocate_module_to" ([
  "  let answer = get old_fw in "
],"") ;;

all_printables "remove_files" ([
  " let answer = force_get new_fw in "
],"") ;;

all_printables "rename_module_on_filename_level_and_in_files" ([
  " let old_val = get old_fw in ";
  " let (old_mname,new_mname,_) = triple in";
  " let rep = Dfn_middle.rename_module (old_mname,new_mname) in ";
  " let answer = Image.image rep old_val in ";
],"") ;;

all_printables "rename_subdirectory_as" ([
   " let old_val = get old_fw in ";
   " let (old_sdir,new_sdir) = pair in";
   " let s_new_sdir = Dfa_subdirectory.without_trailing_slash new_sdir in ";
   " let rep = Dfn_middle.rename_endsubdirectory (old_sdir,s_new_sdir) in ";
   " let answer = Image.image rep old_val in ";
],"") ;;

all_printables "replace_string" ([
  " let answer = force_get new_fw in "
],"") ;;

all_printables "replace_value" ([
  " let answer = force_get new_fw in "
],"") ;;

all_printables "set_gitpush_after_backup" ([
   "  let answer = get old_fw in "
],"") ;;

all_printables "set_last_noticed_changes" ([
   "  let answer = get old_fw in "
],"") ;;

let text_for_submodule sumo =      
   String.concat "\n" 
   ([
      "module "^(sumo)^" = struct \n";
      " let the_hashtbl = ((Hashtbl.create 10)) ;; "
   ]@
   (text_from_ghetto sumo)   
   @(Image.image (fun (methname,_,_,_)->
      "\n"^(text_from_cartesian sumo methname)) methods)
   @[   "\nend ;;"
   ]);;


let text_for_all_subdmodules () = 
   let temp1 =
      (Cached.full_text ()) 
      ::(Image.image text_for_submodule submodules) in 
   "\n\n\n"^(String.concat "\n\n\n" temp1)^"\n\n\n" ;;

   

let prelude = String.concat "\n" [
  "module Private = struct\n"; 
  " let expand_index idx = (idx,Fw_indexer.get_state idx) ;;";
  " let index fw = fw.Fw_with_dependencies_t.index_for_caching ;; ";  
  " let parent fw = fw.Fw_with_dependencies_t.parent ;;";
] ;;

let postlude = String.concat "\n" [
   "  let details_for_module  fw mn = List.assoc mn (Modularized_details.get fw) ;;";
   "  module Exit = All_printables ;; ";
   "end;;";
   "";
   "let all_subdirectories fw = Private.All_subdirectories.get fw;;";
   "let ancestors_for_module fw mn = snd (List.assoc mn (Private.Order.get fw)) ;;";
   "let configuration fw = Fw_with_small_details.configuration (Private.parent fw) ;;";
   "let dep_ordered_modules fw = Image.image fst (Private.Order.get fw);;";
   "let direct_fathers_for_module fw mn = fst (List.assoc mn (Private.Order.get fw)) ;;";
   "let empty_one = Private.Exit.empty_one ;;";
   "let forget_modules = Private.Exit.forget_modules ;;";
   "let get_mtime fw rl = Fw_with_small_details.get_mtime (Private.parent fw) rl ;;";
   "let get_mtime_or_zero_if_file_is_nonregistered fw rl = Fw_with_small_details.get_mtime_or_zero_if_file_is_nonregistered (Private.parent fw) rl ;;";
   "let inspect_and_update = Private.Exit.inspect_and_update ;;";
   "let last_noticed_changes fw = Fw_with_small_details.last_noticed_changes (Private.parent fw) ;;";
   "let mli_mt_for_module fw mn = match Fw_module_small_details.opt_mli_modification_time (Private.details_for_module fw mn) with ";
   "                              None -> \"0.\" |Some(fl)->fl ;;";
   "let mli_presence_for_module fw mn = Fw_module_small_details.mli_present (Private.details_for_module fw mn) ;;";
   "let needed_dirs_for_module fw mn = List.assoc mn (Private.Needed_dirs.get fw) ;;";
   "let needed_libs_for_module fw mn = List.assoc mn (Private.Needed_libs.get fw) ;;";
   "let noncompilable_files fw = Fw_with_small_details.noncompilable_files (Private.parent fw) ;;";
   "let of_concrete_object = Private.Exit.of_concrete_object ;;";
   "let of_configuration = Private.Exit.of_configuration ;;";
   "let of_configuration_and_list = Private.Exit.of_configuration_and_list ;;";
   "let overwrite_file_if_it_exists = Private.Exit.overwrite_file_if_it_exists ;;";
   "let principal_ending_for_module fw mn = Fw_module_small_details.principal_ending (Private.details_for_module fw mn) ;;";
   "let principal_mt_for_module fw mn = Fw_module_small_details.principal_modification_time (Private.details_for_module fw mn) ;;";
   "let printer_equipped_types fw = Private.All_printables.get fw;;";
   "let reflect_latest_changes_in_github = Private.Exit.reflect_latest_changes_in_github ;;";
   "let register_rootless_paths = Private.Exit.register_rootless_paths ;;";
   "let relocate_module_to = Private.Exit.relocate_module_to ;;";
   "let remove_files = Private.Exit.remove_files ;;";
   "let rename_module_on_filename_level_and_in_files = Private.Exit.rename_module_on_filename_level_and_in_files ;;";
   "let rename_subdirectory_as = Private.Exit.rename_subdirectory_as ;;";
   "let replace_string = Private.Exit.replace_string ;;";
   "let replace_value = Private.Exit.replace_value ;;";
   "let set_gitpush_after_backup = Private.Exit.set_gitpush_after_backup ;;";
   "let set_last_noticed_changes = Private.Exit.set_last_noticed_changes ;;";
   "let subdir_for_module fw mn = Fw_module_small_details.subdirectory (Private.details_for_module fw mn) ;;";
   "let to_concrete_object fw = Fw_with_small_details.to_concrete_object (Private.parent fw) ;;";
   "let usual_compilable_files fw = Fw_with_small_details.usual_compilable_files (Private.parent fw) ;;";


] ;;

let write_all_to_draft () =
   let text = "\n\n"^prelude^"\n\n"
               ^(text_for_all_subdmodules ()) 
               ^("\n\n"^postlude^"\n\n")
   and file = Absolute_path.of_string "Fads/sirloin.ml"
   and beg_mark = "(* Beginning of sirloin *)"
   and end_mark = "(* End of sirloin *)" in 
   Replace_inside.overwrite_between_markers_inside_file 
     (Overwriter.of_string text) (beg_mark,end_mark) file ;;
     
let write_all () =
      let text = "(*\n\n#use\"Filewatching/fw_with_dependencies.ml\";;\n\n*)\n\n"^
                 "\n\n"^prelude^"\n\n"
                  ^(text_for_all_subdmodules ()) 
                  ^("\n\n"^postlude^"\n\n")
      and file = Absolute_path.of_string "Filewatching/fw_with_dependencies.ml" in 
      Io.overwrite_with file text   ;;
        




