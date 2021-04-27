(*

#use"Filewatching/fw_wrapper.ml";;

*)

exception Register_rootless_path_exn of string list;;

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
      Fw_wrapper_t.compilable_files = update_in_list_of_pairs fw w_files (fw.Fw_wrapper_t.compilable_files) ;
      noncompilable_files = update_in_list_of_pairs fw sw_files (fw.Fw_wrapper_t.noncompilable_files) ;
} ;;

let remove_nonnoncompilable_files fw rootless_paths =
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.compilable_files)  
   };;

let remove_noncompilable_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
   {
      fw with 
      Fw_wrapper_t.noncompilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.noncompilable_files)  
   };;

let remove_files fw rootless_paths=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
    let removals_to_be_made = Image.image (
      fun path->" rm -f "^s_root^(Dfn_rootless.to_line path) 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc removals_to_be_made in 
    let fw2 ={
      fw with 
      Fw_wrapper_t.compilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.compilable_files)  ;
      Fw_wrapper_t.noncompilable_files = List.filter (fun (path,_)->
         not(List.mem path rootless_paths)
      ) (fw.Fw_wrapper_t.noncompilable_files)  
   } in 
   Fw_wrapper_field.reflect_destructions_in_diff fw2 rootless_paths ;;



let forget_modules fw mod_names =
   let the_files = Option.filter_and_unpack (
      fun (path,_)-> 
        if List.mem (Dfn_rootless.to_module path) mod_names 
        then Some path
        else None
   ) fw.Fw_wrapper_t.compilable_files in 
   remove_files fw the_files;;

let register_rootless_paths fw rootless_paths= 
   let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in
   let bad_paths = Option.filter_and_unpack (
     fun rp-> let s_full_path = s_root^(Dfn_rootless.to_line rp)  in 
     if not(Sys.file_exists s_full_path)
     then Some(s_full_path)
     else None
   ) rootless_paths in 
   if bad_paths<>[]
   then raise(Register_rootless_path_exn(bad_paths))
   else
   let (c_paths,nc_paths) = List.partition Dfn_rootless.is_compilable rootless_paths in 
   let fw2=  {
      fw with 
      Fw_wrapper_t.compilable_files =  
        (fw.Fw_wrapper_t.compilable_files)@
          (Image.image (recompute_all_info fw) c_paths)  ;
      Fw_wrapper_t.noncompilable_files =  
        (fw.Fw_wrapper_t.noncompilable_files)@
         (Image.image (recompute_all_info fw) nc_paths)  
    }  in 
    (Fw_wrapper_field.reflect_creations_in_diff fw2 rootless_paths,(c_paths,nc_paths));;

let relocate_compilable_files_to fw rootless_paths new_subdir=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) 
    and s_subdir = Dfa_subdirectory.connectable_to_subpath new_subdir in 
    let displacements_to_be_made = Image.image (
      fun path->" mv "^s_root^(Dfn_rootless.to_line path)^" "^
      s_root^s_subdir 
    ) rootless_paths in 
    let _=Unix_command.conditional_multiple_uc displacements_to_be_made in 
    let reps = Image.image (fun path->
      (path,Dfn_rootless.relocate_to path new_subdir)
    ) rootless_paths in 
    let fw2 = {
      fw with 
      Fw_wrapper_t.compilable_files = Image.image (fun pair->
         let (path,_)=pair in 
         if(List.mem path rootless_paths) 
         then let new_path = Dfn_rootless.relocate_to path new_subdir in 
              (new_path,recompute_mtime fw new_path)
         else pair
      ) (fw.Fw_wrapper_t.compilable_files)  
   } in 
   Fw_wrapper_field.reflect_replacements_in_diff fw2 reps ;;

let relocate_module_to fw mod_name new_subdir=
   let the_files = Option.filter_and_unpack (
      fun (path,_)-> 
        if (Dfn_rootless.to_module path)=mod_name 
        then Some path
        else None
   ) fw.Fw_wrapper_t.compilable_files in 
   relocate_compilable_files_to fw the_files new_subdir;;

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
           fw (old_string,new_string) ref_for_usual_ones fw.Fw_wrapper_t.compilable_files 
    and  (new_special_files,changed_special_files)=
        helper_during_string_replacement 
           fw (old_string,new_string) ref_for_special_ones fw.Fw_wrapper_t.noncompilable_files   in 
    let new_fw ={
       fw with
       Fw_wrapper_t.compilable_files         = new_usual_files ;
       Fw_wrapper_t.noncompilable_files = new_special_files ;
    }  in 
    (new_fw,(changed_usual_files,changed_special_files));;         
       
let rename_value_inside_module fw (old_name,new_name) preceding_files rootless_path=
   let full_path = Dfn_join.root_to_rootless (Fw_wrapper_field.root fw) rootless_path in 
   let absolute_path=Dfn_full.to_absolute_path  full_path in 
   let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files old_name new_name absolute_path in 
   let new_compilable_files =Image.image (
       fun pair->
         let (path,_)=pair in 
         if path = rootless_path 
         then recompute_all_info fw path
         else pair 
   ) (fw.Fw_wrapper_t.compilable_files) in 
   {
      fw with 
       Fw_wrapper_t.compilable_files = new_compilable_files
   };;  

let ref_for_subdirectory_renaming = ref [];;

let remember_during_subdirectory_renaming pair =
   (ref_for_subdirectory_renaming := pair :: (!ref_for_subdirectory_renaming) );;

let helper1_during_subdirectory_renaming fw (old_subdir,new_subdir) pair=
   let (rootless_path,_)=pair in 
   match Dfn_rootless.soak (old_subdir,new_subdir) rootless_path with 
   Some(new_rootless_path) -> 
        let _=(
           remember_during_subdirectory_renaming (rootless_path,new_rootless_path)
        ) in 
        recompute_all_info fw new_rootless_path
   |None -> pair;;

let helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) l_pairs =
     let _=(ref_for_subdirectory_renaming := []) in 
     let comp=Image.image (helper1_during_subdirectory_renaming fw (old_subdir,new_subdir)) l_pairs in 
     (comp,List.rev(!ref_for_subdirectory_renaming));;

let rename_subdirectory_as fw (old_subdir,new_subdir)=
    let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw)  in 
    let s_old_subdir = Dfa_subdirectory.without_trailing_slash old_subdir 
    and s_new_subdir = Dfa_subdirectory.without_trailing_slash new_subdir in 
    let old_full_path = s_root^s_old_subdir 
    and new_full_path = s_root^s_new_subdir in 
    let cmd=" mv "^old_full_path^" "^new_full_path in 
        let _=Unix_command.hardcore_uc cmd in 
    let (c_files,c_reps)   =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Fw_wrapper_t.compilable_files) 
    and (nc_files,nc_reps) =  helper2_during_subdirectory_renaming fw (old_subdir,new_subdir) (fw.Fw_wrapper_t.noncompilable_files) in    
   let fw2 = {
      fw with
      Fw_wrapper_t.compilable_files = c_files  ;
      Fw_wrapper_t.noncompilable_files = nc_files  ;
   } in 
   Fw_wrapper_field.reflect_replacements_in_diff fw2 (c_reps@nc_reps);;   

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
    let ref_for_compilable_ones=ref[] 
    and ref_for_noncompilable_ones=ref[] in 
    let (new_c_files,changed_c_files)=
        helper2_during_inspection fw ref_for_compilable_ones fw.Fw_wrapper_t.compilable_files 
    and  (new_nc_files,changed_nc_files)=
        helper2_during_inspection fw ref_for_noncompilable_ones fw.Fw_wrapper_t.noncompilable_files   in 
    let fw2 ={
       fw with
       Fw_wrapper_t.compilable_files         = new_c_files ;
       Fw_wrapper_t.noncompilable_files = new_nc_files ;
    }  in 
    let new_fw = Fw_wrapper_field.reflect_changes_in_diff fw2 (changed_c_files@changed_nc_files) in 
    (new_fw,(changed_c_files,changed_nc_files));;         


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
   let old_compilable_files = fw.Fw_wrapper_t.compilable_files  in    
   let new_compilable_files = Image.image (
     fun pair->
       let (rootless,_)=pair in 
       match Option.seek (fun (old_one,_)->old_one=rootless) replacements with  
       Some(_,new_rootless_path)-> recompute_all_info fw new_rootless_path  
       |None -> pair 
   )  old_compilable_files in 
   let fw2 ={
      fw with 
      Fw_wrapper_t.compilable_files = new_compilable_files
   }   in 
   Fw_wrapper_field.reflect_replacements_in_diff fw2 replacements;;
    
let rename_module_in_files fw (old_module,new_module) files_to_be_rewritten =
  let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
  let _=List.iter (
    fun rootless_path->
      let ap=Absolute_path.of_string (s_root^(Dfn_rootless.to_line rootless_path)) in 
      Look_for_module_names.change_module_name_in_mlx_file old_module new_module ap 
  ) files_to_be_rewritten in 
  let old_compilable_files = fw.Fw_wrapper_t.compilable_files  in    
  let new_compilable_files = Image.image (
     fun pair->
       let (rootless,_)=pair in 
       if List.mem rootless files_to_be_rewritten
       then recompute_all_info fw rootless 
       else pair 
   )  old_compilable_files in 
   let fw2 ={
      fw with 
      Fw_wrapper_t.compilable_files = new_compilable_files
   }    in 
   Fw_wrapper_field.reflect_changes_in_diff fw2 files_to_be_rewritten
    ;;
      
let rename_module_in_special_files fw (old_module,new_module) =
  let s_root = Dfa_root.connectable_to_subpath (Fw_wrapper_field.root fw) in 
  let old_special_files = fw.Fw_wrapper_t.noncompilable_files   in    
  let new_special_files = Image.image (
     fun pair->
       let (rootless,mtime)=pair in 
       let content = Fw_wrapper_field.get_content fw rootless in 
       if List.mem old_module (Look_for_module_names.names_in_ml_ocamlcode content)
       then let ap=Absolute_path.of_string (s_root^(Dfn_rootless.to_line rootless)) in 
            let _=Look_for_module_names.change_module_name_in_mlx_file old_module new_module ap in 
            recompute_all_info fw rootless 
       else pair 
   )  old_special_files in 
   {
      fw with 
      Fw_wrapper_t.noncompilable_files = new_special_files
   }     ;;   

let rename_module_everywhere fw rootlesses_to_be_renamed new_module files_to_be_rewritten=
   let (Dfn_rootless_t.J(_,old_module,_))=List.hd rootlesses_to_be_renamed in
   let fw2=rename_module_in_filename_only fw rootlesses_to_be_renamed new_module in 
   let fw3=rename_module_in_files fw2 (old_module,new_module) files_to_be_rewritten in 
   fw3;;

let replace_string_in_list_of_pairs fw (replacee,replacer) l=
   let changed_ones=ref[]  in 
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
             let _=(Replace_inside.replace_inside_file (replacee,replacer) ap;
                   ) in 
             recompute_all_info fw rootless 
        else pair     
   ) l in 
   (new_l,List.rev(!changed_ones));;

let replace_string fw (replacee,replacer) =
   let rep = replace_string_in_list_of_pairs fw (replacee,replacer)  in 
   let (new_c_files,changed_c_files) =  rep fw.Fw_wrapper_t.compilable_files 
   and (new_nc_files,changed_nc_files) =  rep fw.Fw_wrapper_t.noncompilable_files in 
   let fw2 ={
       fw with
       Fw_wrapper_t.compilable_files = new_c_files;
       noncompilable_files = new_nc_files;
   } in 
   let fw3 = Fw_wrapper_field.reflect_changes_in_diff fw2 (changed_c_files @ changed_nc_files) in 
   (fw3,(changed_c_files,changed_nc_files));;

let replace_value fw (preceding_files,path) (replacee,pre_replacer) =
    let replacer=(Cull_string.before_rightmost replacee '.')^"."^pre_replacer in 
    let _=Rename_moduled_value_in_file.rename_moduled_value_in_file 
      preceding_files replacee (Overwriter.of_string pre_replacer) path in 
    let rootless = Dfn_common.decompose_absolute_path_using_root path (Fw_wrapper_field.root fw)  in 
    let fw2= update_some_files fw ([rootless],[]) in 
    let (fw3,(changed_c_files,changed_nc_files))=replace_string fw2 (replacee,replacer) in 
    let fw4 =  Fw_wrapper_field.reflect_changes_in_diff fw3 (rootless::(changed_c_files@changed_nc_files)) in         
    (fw4,(rootless::changed_c_files,changed_nc_files));;


let compilable_absolute_paths fw= 
   let root = Fw_wrapper_field.root fw in 
   Image.image (
     fun (rootless,_)-> 
        Absolute_path.of_string (
           Dfn_common.recompose_potential_absolute_path root rootless
        )
   ) fw.Fw_wrapper_t.compilable_files;;
   
let overwrite_compilable_file_if_it_exists fw rootless new_content =
   let root = Fw_wrapper_field.root fw in 
   if List.exists ( fun (r,_)->r=rootless ) fw.Fw_wrapper_t.compilable_files 
   then let ap = Absolute_path.of_string (Dfn_common.recompose_potential_absolute_path root rootless) in 
        let _=Io.overwrite_with ap new_content in 
        {
           fw with 
           Fw_wrapper_t.compilable_files = update_in_list_of_pairs fw [rootless] (fw.Fw_wrapper_t.compilable_files);
        }
   else fw;;


end;;


let empty_one config= {
   Fw_wrapper_t.configuration = config;
   compilable_files = [];
   noncompilable_files = [];
   last_noticed_changes = Dircopy_diff.empty_one;
};; 

let forget_modules = Private.forget_modules;;

let inspect_and_update = Private.inspect_and_update;;

let compilable_absolute_paths = Private.compilable_absolute_paths;;

let overwrite_compilable_file_if_it_exists = Private.overwrite_compilable_file_if_it_exists;;

let reflect_latest_changes_in_github fw opt_msg=
   let config = fw.Fw_wrapper_t.configuration in 
   let _= Reflect_change_in_github.backup config fw.Fw_wrapper_t.last_noticed_changes opt_msg in 
   {fw with Fw_wrapper_t.last_noticed_changes = Dircopy_diff.empty_one} ;; 


let register_rootless_paths = Private.register_rootless_paths;;

let relocate_module_to = Private.relocate_module_to;;

let remove_files = Private.remove_files;;

let rename_module = Private.rename_module_everywhere;;

let rename_subdirectory_as = Private.rename_subdirectory_as;;

let replace_string = Private.replace_string;;

let replace_value = Private.replace_value;;


