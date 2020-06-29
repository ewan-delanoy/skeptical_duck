(*

#use"more_unix.ml";;

*)


module Private=struct
 
let naive_extension ap=
   let s=Absolute_path.to_string ap in
   let i=String.rindex s '.' in
   (Cull_string.cobeginning (i+1) s);; 
   
let extension x=try (naive_extension x) with 
  any_exception->"";;
  
 let is_a_directory ap=
   let s=Absolute_path.to_string ap in
   try (function x->true)(Sys.readdir s) with any_exception->false;;
 
 let father ap=
   let s=Absolute_path.to_string ap in
   let i=String.rindex s '/' in
   if i=0 then Directory_name.of_string"/" else
   Directory_name.of_string (Cull_string.beginning i s);; 
   
 let son dir=
   let s=Directory_name.connectable_to_subpath dir in
   let i=String.rindex s '/' in
   if i=0 then "" else
   (Cull_string.cobeginning (i+1) s);; 
  
 let is_a_nondirectory_or_a_nib x=
  if is_a_directory(x)
  then extension(x)="nib"
  else not(Substring.is_a_substring_of(".nib/")(Absolute_path.to_string x));;
  
 let naive_ls dir=
   let s=Directory_name.connectable_to_subpath dir in
   let s_with_slash=(function ()->
    if String.get(s)(String.length(s)-1)='/'
    then s
    else s^"/"
   )() in
   let temp1=Array.to_list(Sys.readdir(s)) in
   let tempf=(function w->try (Some(Absolute_path.of_string(s_with_slash^w))) with
   any_exception->None) in
   Option.filter_and_unpack tempf temp1;;
   
 let ls x=try (naive_ls x) with any_exception->[];;  
 
 let test_for_cleaniness=function ap->
  let s=Absolute_path.to_string ap in
  Cull_string.after_rightmost(s)('/')<>".DS_Store";;
 
 let cleaned_ls x=
   List.filter test_for_cleaniness (ls x);;
   
let select_by_prefix subdir forbidden_subdirs =
  Option.filter_and_unpack (
     fun forb_subdir -> 
        if Supstring.begins_with forb_subdir subdir 
        then Some(Cull_string.two_sided_cutting (subdir,"") forb_subdir)
        else None
  ) forbidden_subdirs ;; 

let ls_with_ignored_subdirs (dir,forbidden_subdirs)=
   let temp1 = Array.to_list (Sys.readdir dir) in
   let temp2 = Option.filter_and_unpack (
      fun fname -> if List.for_all (
          fun forb_subdir -> 
           not(Supstring.begins_with fname forb_subdir)
        )  forbidden_subdirs
           then Some(dir^fname)
           else None
   ) temp1 in 
   let is_a_dir  = (fun s->is_a_directory(Absolute_path.AP(s))) in 
   let (found_dirs,found_nondirs) = List.partition is_a_dir temp2 in 
   let new_constraints = Image.imagination (
     fun full_subdir_path ->
        let subdir = Cull_string.two_sided_cutting (dir,"") full_subdir_path in 
       (full_subdir_path^"/",select_by_prefix subdir forbidden_subdirs)
   ) found_dirs in 
   (found_nondirs,found_dirs,new_constraints);;

let rec helper_for_complete_ls_with_ignored_subdirs 
  (treated_nondirs,treated_dirs,to_be_treated) = match to_be_treated with 
  [] -> (Image.imagination Absolute_path.of_string treated_nondirs,treated_dirs)
  |(dir,forbidden_subdirs) :: others -> 
    let (found_nondirs,found_dirs,new_constraints) = 
        ls_with_ignored_subdirs (dir,forbidden_subdirs) in 
    let new_treated_nondirs = List.rev_append found_nondirs treated_nondirs 
    and new_treated_dirs =  List.rev_append found_dirs treated_dirs 
    and new_to_be_treated = List.rev_append new_constraints others in 
    let n = string_of_int(List.length new_to_be_treated) in 
    let msg = " "^n^" to go ...\n" in 
    let _= (print_string msg;flush stdout) in 
    helper_for_complete_ls_with_ignored_subdirs 
    (new_treated_nondirs,new_treated_dirs,new_to_be_treated) ;;

let complete_ls_with_ignored_subdirs dir forbidden_subdirs = 
   let s_dir = Directory_name.connectable_to_subpath dir in 
   let (treated_nondirs,treated_dirs) = 
   helper_for_complete_ls_with_ignored_subdirs 
  ([],[],[s_dir,
         Image.imagination Dfa_subdirectory.without_trailing_slash forbidden_subdirs]) in 
   (treated_nondirs,Image.imagination 
     (fun x->Dfa_subdirectory.of_line(Cull_string.two_sided_cutting (s_dir,"") x)) 
   treated_dirs);;

let ls_with_directories_only dir=
   let temp1 = cleaned_ls dir in 
   Option.filter_and_unpack (
     fun ap -> 
       if is_a_directory ap 
       then let s_ap = Absolute_path.to_string ap in 
            Some(Directory_name.of_string s_ap)
       else None
   )  temp1 ;;

 let dirty_ones_in_ls x=
   List.filter (function u->not(test_for_cleaniness u) )(ls x);; 
 
 let adhoc_ls ap=
   let s=Absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Directory_name.of_string s in
   ls dir;;
 

 
let complete_ls dir=
   let s_dir=Directory_name.connectable_to_subpath dir in
   let x=Absolute_path.of_string s_dir in
   Explicit.explore_tree adhoc_ls [x];;   

let adhoc_ls_with_ignored_subdirs ap=
   let s=Absolute_path.to_string ap in
   if not(is_a_directory ap) 
   then []
   else 
   let dir=Directory_name.of_string s in
   ls dir;;

let complete_ls_with_directories_only x=
  Explicit.explore_tree ls_with_directories_only [x];;
  

 let complete_ls_with_nondirectories_only x=
  List.filter(is_a_nondirectory_or_a_nib)(complete_ls x);;
  
  
 let beheaded_ls_with_nondirectories_only x=
  let n0=String.length(Absolute_path.to_string x) in
  let temp1=List.filter(is_a_nondirectory_or_a_nib)(adhoc_ls x) in
  let temp2=Image.imagination (fun ap->Cull_string.cobeginning n0 (Absolute_path.to_string ap)) temp1 in
  temp2;; 
 
 let dir_substructure x=
    let n0=String.length(Absolute_path.to_string x) in
    let temp1=(Stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(function x->extension(x)<>"nib")(temp1) in
    List.rev_map(function ap->Cull_string.cobeginning n0 (Absolute_path.to_string ap))(temp2);;
  
 let endfiles x=
    let n0=String.length(Absolute_path.to_string x)+1(*because of the slash!*) in
    let temp1=(Stabilize.explore_tree adhoc_ls (adhoc_ls x)) in
    let temp2=List.filter(is_a_nondirectory_or_a_nib)(temp1) in
    List.rev_map(function ap->Cull_string.cobeginning n0 (Absolute_path.to_string ap))(temp2);;
    
let quick_complete_ls s=
  let x=Directory_name.of_string s in
  let temp1=complete_ls x in
  Image.imagination Absolute_path.to_string temp1;;  
  
 

let quick_beheaded_complete_ls s=
  let x=Directory_name.of_string s in
  let n=String.length(Directory_name.connectable_to_subpath x) in
  let temp1=complete_ls x in
  Image.imagination (fun ap->Cull_string.cobeginning n (Absolute_path.to_string ap)) temp1;; 
  
let beheaded_simple_ls dir=
  let n=String.length(Directory_name.connectable_to_subpath dir) in
  let temp1=ls dir in
  Image.imagination (fun ap->
   Cull_string.cobeginning n (Absolute_path.to_string ap)) temp1;; 


let clear_directory_contents root =
    let s_root = Dfa_root.connectable_to_subpath root in 
    let cmd = "rm -rf "^s_root^"*" in 
    Sys.command cmd;;


let create_subdirs_and_fill_files_if_necessary root subdirs files_with_content =
   let s_root = Dfa_root.connectable_to_subpath root in 
   let cmds1=Image.imagination (
      fun subdir -> 
         "mkdir -p "^s_root^(Dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Image.imagination Sys.command cmds1 in 
   let temp1=Option.filter_and_unpack (
     fun (rootless,content)->
        let full_path = Dfn_full.to_line(Dfn_join.root_to_rootless root rootless) in 
        if Sys.file_exists full_path 
        then None 
        else Some(full_path,content)
   ) files_with_content in 
   Image.imagination (
      fun (full_path,content) ->
         let _=Sys.command("touch "^full_path) in 
         Io.overwrite_with (Absolute_path.of_string full_path) content
   )  temp1;;


let create_subdirs_and_fill_files root subdirs files_with_content =
   let s_root = Dfa_root.connectable_to_subpath root in 
   let cmds1=Image.imagination (
      fun subdir -> 
         "mkdir -p "^s_root^(Dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Image.imagination Sys.command cmds1 in 
   Image.imagination (
     fun (rootless,content)->
        let full_path = Dfn_full.to_line(Dfn_join.root_to_rootless root rootless) in 
         let _=Sys.command("touch "^full_path) in 
         Io.overwrite_with (Absolute_path.of_string full_path) content
   ) files_with_content;;

end;;    


let all_files_with_endings dir l_endings=
   let temp1=Private.complete_ls dir in
   let temp2=List.filter(
   fun ap->
     let s_ap=Absolute_path.to_string ap in
     List.exists( fun ending->
       Supstring.ends_with s_ap ending)
     l_endings  
   ) temp1 in
   temp2;;  
let beheaded_simple_ls=Private.beheaded_simple_ls;;
let complete_ls=Private.complete_ls;;
let complete_ls_with_directories_only=Private.complete_ls_with_directories_only;;
let complete_ls_with_ignored_subdirs=Private.complete_ls_with_ignored_subdirs;;
let complete_ls_with_nondirectories_only=Private.complete_ls_with_nondirectories_only;;
let clear_directory_contents = Private.clear_directory_contents;;
let create_subdirs_and_fill_files = Private.create_subdirs_and_fill_files;;
let create_subdirs_and_fill_files_if_necessary = Private.create_subdirs_and_fill_files_if_necessary;;
let is_a_directory=Private.is_a_directory;;   
let quick_beheaded_complete_ls=Private.quick_beheaded_complete_ls;;           
let simple_ls=Private.ls;;