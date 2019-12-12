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


 let complete_ls_with_nondirectories_only x=
  List.filter(is_a_nondirectory_or_a_nib)(complete_ls x);;
  
  
 let beheaded_ls_with_nondirectories_only x=
  let n0=String.length(Absolute_path.to_string x) in
  let temp1=List.filter(is_a_nondirectory_or_a_nib)(adhoc_ls x) in
  let temp2=Image.image (fun ap->Cull_string.cobeginning n0 (Absolute_path.to_string ap)) temp1 in
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
  Image.image Absolute_path.to_string temp1;;  
  
 

let quick_beheaded_complete_ls s=
  let x=Directory_name.of_string s in
  let n=String.length(Directory_name.connectable_to_subpath x) in
  let temp1=complete_ls x in
  Image.image (fun ap->Cull_string.cobeginning n (Absolute_path.to_string ap)) temp1;; 
  
let beheaded_simple_ls dir=
  let n=String.length(Directory_name.connectable_to_subpath dir) in
  let temp1=ls dir in
  Image.image (fun ap->
   Cull_string.cobeginning n (Absolute_path.to_string ap)) temp1;; 
  
let create_conventional_context dir subdirs files_with_content =
   (* we assume that each file in files_with_content has its subir in the subdirs list *)
   let s_dir = Directory_name.connectable_to_subpath dir in 
   let commands_for_subdirs = Image.image (
      fun subdir->
         "mkdir -p "^s_dir^(Dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Unix_command.conditional_multiple_uc commands_for_subdirs in 
   let temp1 = Option.filter_and_unpack (
      fun (rootless,content)->
          let fn = s_dir^(Dfn_rootless.to_line rootless) in 
          if Sys.file_exists fn 
          then None
         else let ap = Absolute_path.of_string fn in 
              Some("touch "^fn,ap,content)
   ) files_with_content in 
   List.iter (fun (cmd,ap,content)->
        let i=Unix_command.hardcore_uc cmd in 
        if i=0 then Io.overwrite_with ap content
    ) temp1 ;;
    
     
let create_subdirs_and_fill_files_if_necessary root subdirs files_with_content =
   let s_root = Dfa_root.connectable_to_subpath root in 
   let cmds1=Image.image (
      fun subdir -> 
         "mkdir -p "^s_root^(Dfa_subdirectory.without_trailing_slash subdir)
   ) subdirs in 
   let _=Image.image Sys.command cmds1 in 
   let temp1=Option.filter_and_unpack (
     fun (rootless,content)->
        let full_path = Dfn_full.to_line(Dfn_join.root_to_rootless root rootless) in 
        if Sys.file_exists full_path 
        then None 
        else Some(full_path,content)
   ) files_with_content in 
   Image.image (
      fun (full_path,content) ->
         let _=Sys.command("touch "^full_path) in 
         Io.overwrite_with (Absolute_path.of_string full_path) content
   )  temp1;;


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
let complete_ls_with_nondirectories_only=Private.complete_ls_with_nondirectories_only;;
let create_conventional_context = Private.create_conventional_context;;
let create_subdirs_and_fill_files_if_necessary = Private.create_subdirs_and_fill_files_if_necessary;;
let is_a_directory=Private.is_a_directory;;   
let quick_beheaded_complete_ls=Private.quick_beheaded_complete_ls;;           
let simple_ls=Private.ls;;