(*

#use"Filewatching/fw_wrapper.ml";;

*)

exception Register_rootless_path_exn of string;;

module Private = struct

let mtime file = string_of_float((Unix.stat file).Unix.st_mtime) ;;

let recompute_mtime fw path =
     let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     mtime file;;


let recompute_all_info fw path =
     let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
     and s_path=Dfn_rootless.to_line path in 
     let file = s_root^s_path in 
     (path,mtime file);;

let update_in_list_of_pairs fw  to_be_updated pairs  =
   Image.image (
      fun pair -> 
        let (rootless,mtime)=pair in 
        if List.mem rootless to_be_updated 
        then recompute_all_info fw rootless 
        else pair
   ) pairs;;

let update_some_files fw (w_files,sw_files) = {
    fw with 
      Fw_wrapper_t.watched_files = update_in_list_of_pairs fw w_files (fw.Fw_wrapper_t.watched_files) ;
      special_watched_files = update_in_list_of_pairs fw sw_files (fw.Fw_wrapper_t.special_watched_files) ;
} ;;

let remove_nonspecial_watched_files fw rootless_paths =
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.watched_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.watched_files)  
   };;

let remove_special_watched_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.special_watched_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.special_watched_files)  
   };;

let remove_watched_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.watched_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.watched_files)  ;
      Fw_wrapper_t.special_watched_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.special_watched_files)  
   };;



let forget_module fw mod_name =
   let the_files = Option.filter_and_unpack (
      fun (path,_)-> 
        if (Dfn_rootless.to_module path)=mod_name 
        then Some path
        else None
   ) fw.Fw_wrapper_t.watched_files in 
   remove_watched_files fw the_files;;

let forget fw x=
      if String.contains x '.'
      then remove_watched_files fw [Dfn_rootless.of_line x]
      else forget_module fw (Dfa_module.of_line(x));;


let register_rootless_path fw rootless_path= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
   let s_full_path = s_root^(Dfn_rootless.to_line rootless_path)  in 
   if not(Sys.file_exists s_full_path)
   then raise(Register_rootless_path_exn(s_full_path))
   else
    {
      fw with 
      Fw_wrapper_t.watched_files =  
        (fw.Fw_wrapper_t.watched_files)@[recompute_all_info fw rootless_path]  
    };;

let register_special_rootless_path fw rootless_path= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
   let s_full_path = s_root^(Dfn_rootless.to_line rootless_path)  in 
   if not(Sys.file_exists s_full_path)
   then raise(Register_rootless_path_exn(s_full_path))
   else
    {
      fw with 
      Fw_wrapper_t.special_watched_files =  
        (fw.Fw_wrapper_t.special_watched_files)@[recompute_all_info fw rootless_path]  
    };;



let relocate_watched_files_to fw rootless_paths new_subdir=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
    and s_subdir = Dfa_subdirectory.connectable_to_subpath new_subdir in 
    let displacements_to_be_made = Image.image (
      fun path->" mv "^s_root^(Dfn_rootless.to_line path)^" "^
      s_root^s_subdir 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc displacements_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.watched_files = Image.image (fun pair->
         let (path,_)=pair in 
         if(List.mem path rootless_paths) 
         then let new_path = Dfn_rootless.relocate_to path new_subdir in 
              (new_path,recompute_mtime fw new_path)
         else pair
      ) (fw.Fw_wrapper_t.watched_files)  
   };;

let relocate_module_to fw mod_name new_subdir=
   let the_files = Option.filter_and_unpack (
      fun (path,_)-> 
        if (Dfn_rootless.to_module path)=mod_name 
        then Some path
        else None
   ) fw.Fw_wrapper_t.watched_files in 
   relocate_watched_files_to fw the_files new_subdir;;

let helper_during_string_replacement fw (old_string,new_string) accu old_list=
    let new_list =Image.image (
       fun pair->
         let (old_path,_)=pair in 
         if not(Supstring.contains (Fw_wrapper_field.get_content fw old_path) old_string)
         then pair 
         else 
            let ap = Dfn_full.to_absolute_path (Dfn_join.root_to_rootless (Fw_wrapper_field.root fw) old_path) in 
            let _=(
             Replace_inside.replace_inside_file (old_string,new_string) ap;
             accu:=old_path::(!accu)
            ) in 
            recompute_all_info fw old_path 
         ) old_list in 
    (new_list,List.rev(!accu));;

let replace_string fw (old_string,new_string)=
    let ref_for_usual_ones=ref[] 
    and ref_for_special_ones=ref[] in 
    let (new_usual_files,changed_usual_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_usual_ones fw.Fw_wrapper_t.watched_files 
    and  (new_special_files,changed_special_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_special_ones fw.Fw_wrapper_t.special_watched_files   in 
    let new_fw ={
       fw with
       Fw_wrapper_t.watched_files         = new_usual_files ;
       Fw_wrapper_t.special_watched_files = new_special_files ;
    }  in 
    (new_fw,(changed_usual_files,changed_special_files));;         
       
let rename_value_inside_module fw (old_name,new_name) preceding_files rootless_path=
   let full_path = Dfn_join.root_to_rootless (Fw_wrapper_field.root fw) rootless_path in 
   let absolute_path=Dfn_full.to_absolute_path  full_path in 
   let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name absolute_path in 
   let new_watched_files =Image.image (
       fun pair->
         let (path,_)=pair in 
         if path = rootless_path 
         then recompute_all_info fw path
         else pair 
   ) (fw.Fw_wrapper_t.watched_files) in 
   {
      fw with 
       Fw_wrapper_t.watched_files = new_watched_files
   };;  


let helper1_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root pair=
   let (rootless_path,_)=pair in 
   if Dfn_rootless.to_subdirectory rootless_path = old_subdir 
   then pair
   else let new_rootless_path=Dfn_rootless.rename_subdirectory_as (old_subdir,new_subdir) rootless_path in
        let cmd=" mv "^s_root^(Dfn_rootless.to_line rootless_path)^" "^(Dfn_rootless.to_line new_rootless_path) in 
        let _=Unix_command.hardcore_uc cmd in 
        recompute_all_info fw new_rootless_path;;

let helper2_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root l_pairs =
     Image.image (helper1_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root) l_pairs;;

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw)  in 
   {
      fw with
      Fw_wrapper_t.watched_files = helper2_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root (fw.Fw_wrapper_t.watched_files)  ;
      Fw_wrapper_t.special_watched_files = helper2_during_subdirectory_renaming (old_subdir,new_subdir) fw s_root (fw.Fw_wrapper_t.special_watched_files)  ;
   };;   

let helper1_during_inspection fw accu pair=
   let (rootless_path,old_mtime)=pair in 
   let new_mtime = recompute_mtime fw rootless_path in 
   if new_mtime <> old_mtime
   then let _=(accu:=rootless_path::(!accu)) in 
        recompute_all_info fw rootless_path
   else pair;;

let helper2_during_inspection fw accu l_pairs =
   let new_l_pairs = Image.image (helper1_during_inspection fw accu) l_pairs in 
   (new_l_pairs,List.rev(!accu));;

let inspect_and_update fw = 
    let ref_for_usual_ones=ref[] 
    and ref_for_special_ones=ref[] in 
    let (new_usual_files,changed_usual_files)=
        helper2_during_inspection fw ref_for_usual_ones fw.Fw_wrapper_t.watched_files 
    and  (new_special_files,changed_special_files)=
        helper2_during_inspection fw ref_for_special_ones fw.Fw_wrapper_t.special_watched_files   in 
    let new_fw ={
       fw with
       Fw_wrapper_t.watched_files         = new_usual_files ;
       Fw_wrapper_t.special_watched_files = new_special_files ;
    }  in 
    (new_fw,(changed_usual_files,changed_special_files));;         


let helper1_inside_module_renaming_in_filename fw s_new_module rootless_to_be_renamed =
  let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
   and (Dfn_rootless_t.J(s,m,e))=rootless_to_be_renamed in 
   let s_old_ap=s_root^(Dfn_rootless.to_line rootless_to_be_renamed)
   and s_new_ap=s_root^(Dfa_subdirectory.connectable_to_subpath s)
                ^s_new_module^(Dfa_ending.connectable_to_modulename e) in 
   "mv "^s_old_ap^" "^s_new_ap;;

let helper2_inside_module_renaming_in_filename fw new_module rootless_to_be_renamed =
  let (Dfn_rootless_t.J(s,m,e))=rootless_to_be_renamed in 
  (rootless_to_be_renamed,Dfn_rootless_t.J(s,new_module,e));;

let rename_module_in_filename_only fw rootlesses_to_be_renamed new_module =
   let s_new_module = Dfa_module.to_line new_module in 
   let l_cmds = Image.image (helper1_inside_module_renaming_in_filename fw s_new_module) rootlesses_to_be_renamed in 
   let replacements = Image.image (helper2_inside_module_renaming_in_filename fw new_module) rootlesses_to_be_renamed  in            
   let _ =Unix_command.conditional_multiple_uc l_cmds in  
   let old_watched_files = fw.Fw_wrapper_t.watched_files  in    
   let new_watched_files = Image.image (
     fun pair->
       let (rootless,_)=pair in 
       match Option.seek (fun (old_one,_)->old_one=rootless) replacements with  
       Some(_,new_rootless_path)-> recompute_all_info fw new_rootless_path  
       |None -> pair 
   )  old_watched_files in 
   {
      fw with 
      Fw_wrapper_t.watched_files = new_watched_files
   }     ;;
    
let rename_module_in_files fw (old_module,new_module) files_to_be_rewritten =
  let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
  let _=List.iter (
    fun rootless_path->
      let ap=Absolute_path.of_string (s_root^(Dfn_rootless.to_line rootless_path)) in 
      Look_for_module_names.change_module_name_in_ml_file old_module new_module ap 
  ) files_to_be_rewritten in 
  let old_watched_files = fw.Fw_wrapper_t.watched_files  in    
  let new_watched_files = Image.image (
     fun pair->
       let (rootless,_)=pair in 
       if List.mem rootless files_to_be_rewritten
       then recompute_all_info fw rootless 
       else pair 
   )  old_watched_files in 
   {
      fw with 
      Fw_wrapper_t.watched_files = new_watched_files
   }     ;;
      
let rename_module_in_special_files fw (old_module,new_module) =
  let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
  let old_special_files = fw.Fw_wrapper_t.special_watched_files   in    
  let new_special_files = Image.image (
     fun pair->
       let (rootless,mtime)=pair in 
       let content = Fw_wrapper_field.get_content fw rootless in 
       if List.mem old_module (Look_for_module_names.names_in_ml_ocamlcode content)
       then let ap=Absolute_path.of_string (s_root^(Dfn_rootless.to_line rootless)) in 
            let _=Look_for_module_names.change_module_name_in_ml_file old_module new_module ap in 
            recompute_all_info fw rootless 
       else pair 
   )  old_special_files in 
   {
      fw with 
      Fw_wrapper_t.special_watched_files = new_special_files
   }     ;;   

let rename_module_everywhere fw rootlesses_to_be_renamed new_module files_to_be_rewritten=
   let (Dfn_rootless_t.J(_,old_module,_))=List.hd rootlesses_to_be_renamed in
   let fw2=rename_module_in_filename_only fw rootlesses_to_be_renamed new_module in 
   let fw3=rename_module_in_files fw2 (old_module,new_module) files_to_be_rewritten in 
   rename_module_in_special_files fw3 (old_module,new_module);;

let replace_string_in_list_of_pairs fw (replacee,replacer) l=
   let changed_ones=ref[] in 
   let new_l=Image.image (
      fun pair ->
        let (rootless,mtime)=pair in 
        let content = Fw_wrapper_field.get_content fw rootless in 
        if Substring.is_a_substring_of replacee content 
        then let _=(changed_ones:= rootless:: (!changed_ones)) in 
             let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
             and s_path=Dfn_rootless.to_line rootless in 
             let file = s_root^s_path in  
             let ap=Absolute_path.of_string file in 
             let _=Replace_inside.replace_inside_file (replacee,replacer) ap in 
             recompute_all_info fw rootless 
        else pair     
   ) l in 
   (new_l,List.rev(!changed_ones));;

let replace_string fw (replacee,replacer) =
   let rep = replace_string_in_list_of_pairs fw (replacee,replacer)  in 
   let (new_w_files,changed_w_files) =  rep fw.Fw_wrapper_t.watched_files 
   and (new_sw_files,changed_sw_files) =  rep fw.Fw_wrapper_t.special_watched_files in 
   let new_fw ={
       fw with
       Fw_wrapper_t.watched_files = new_w_files;
       special_watched_files = new_sw_files;
   } in 
   (new_fw,(changed_w_files,changed_sw_files));;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Overwriter.of_string pre_replacer) path in 
    let rootless = Dfn_common.decompose_absolute_path_using_root path (Fw_wrapper_field.root fw)  in 
    let fw2= update_some_files fw ([rootless],[]) in 
    let (fw3,(changed_w_files,changed_sw_files))=replace_string fw2 (replacee,replacer) in 
    (fw3,(rootless::changed_w_files,changed_sw_files));;


let refresh fw =
   let config = fw.Fw_wrapper_t.configuration in 
   let the_root = config.Fw_configuration_t.root in 
   let the_dir =  Directory_name.of_string (Dfa_root.without_trailing_slash the_root) in 
   let list1 = More_unix.complete_ls_with_nondirectories_only the_dir in 
   let list2 = Option.filter_and_unpack(
     fun ap-> try Some(Dfn_common.decompose_absolute_path_using_root ap the_root) with 
              _->None 
   ) list1 in
   let (specials,nonspecials) = List.partition (
      fun rootless -> List.mem rootless config.Fw_configuration_t.special_git_saved_files 
   ) list2 in  
   let nonspecials_to_be_watched1 = List.filter (
      fun rootless ->  (List.mem (Dfn_rootless.to_ending rootless)
         config.Fw_configuration_t.allowed_endings )
         &&
         (
            not(List.mem (Dfn_rootless.to_subdirectory rootless)
             config.Fw_configuration_t.git_ignored_subdirectories
            )
         )
   ) nonspecials in 
   let the_cleaner = (fw.Fw_wrapper_t.configuration).Fw_configuration_t.final_cleaner in 
   let nonspecials_to_be_watched = Fw_final_cleaner.clean the_cleaner  nonspecials_to_be_watched1 in 
   let w_files = Image.image (recompute_all_info fw) nonspecials_to_be_watched 
   and sw_files = Image.image (recompute_all_info fw) specials in 
   {
      fw with 
      Fw_wrapper_t.watched_files = w_files;
       special_watched_files = sw_files;
   }
   ;;

let nonspecial_absolute_paths fw= 
   let root = Fw_wrapper_field.root fw in 
   Image.image (
     fun (rootless,_)-> 
        Absolute_path.of_string (
           Dfn_common.recompose_potential_absolute_path root rootless
        )
   ) fw.Fw_wrapper_t.watched_files;;
   
let overwrite_nonspecial_file_if_it_exists fw rootless new_content =
   let root = Fw_wrapper_field.root fw in 
   if List.exists ( fun (r,_)->r=rootless ) fw.Fw_wrapper_t.watched_files 
   then let ap = Absolute_path.of_string (Dfn_common.recompose_potential_absolute_path root rootless) in 
        let _=Io.overwrite_with ap new_content in 
        {
           fw with 
           Fw_wrapper_t.watched_files = update_in_list_of_pairs fw [rootless] (fw.Fw_wrapper_t.watched_files);
        }
   else fw;;


end;;


let default root_dir= {
   Fw_wrapper_t.configuration = Fw_configuration.default(root_dir);
   watched_files = [];
   special_watched_files = [];
};; 

let forget = Private.forget;;

let forget_module = Private.forget_module;;

let inspect_and_update = Private.inspect_and_update;;

let nonspecial_absolute_paths = Private.nonspecial_absolute_paths;;

let overwrite_nonspecial_file_if_it_exists = Private.overwrite_nonspecial_file_if_it_exists;;

let refresh = Private.refresh ;;

let register_rootless_path = Private.register_rootless_path;;

let relocate_module_to = Private.relocate_module_to;;

let remove_watched_files = Private.remove_watched_files;;

let rename_module = Private.rename_module_everywhere;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;


