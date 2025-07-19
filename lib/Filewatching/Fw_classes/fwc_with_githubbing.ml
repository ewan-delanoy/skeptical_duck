(* 

#use"lib/Filewatching/Fw_classes/fwc_with_githubbing.ml";;

*)

type t = Fwg_with_githubbing.t ;;

module Inherited = struct

  module Parent = Fwc_with_modular_infrastructure ;;

  let parent fw = Fwg_with_githubbing.parent fw ;;

  module Ancestry = Parent.Inherited ;;  

  let make fw_deps github_config= 
    Fwg_with_githubbing.make fw_deps github_config ;;


  
  let all_endinglesses fw = Parent.all_endinglesses (parent fw) ;;
  let all_moduled_mlx_files fw = Parent.all_moduled_mlx_files (parent fw) ;;
  let all_subdirectories fw = Parent.all_subdirectories (parent fw) ;;
  let ancestors_for_module fw = Parent.ancestors_for_module (parent fw) ;;
  let below fw = Parent.below (parent fw) ;;
  
  let check_module_sequence_for_forgettability fw = 
    Parent.check_module_sequence_for_forgettability (parent fw) ;;
  
  let check_that_no_change_has_occurred fw = 
    Ancestry.check_that_no_change_has_occurred(parent fw)  ;;  
    
  
  let decipher_module fw = Parent.decipher_module (parent fw) ;;
  let decipher_path fw = Parent.decipher_path (parent fw) ;;
  let dep_ordered_modules fw = Parent.dep_ordered_modules (parent fw) ;;
  let directly_below fw = Parent.directly_below (parent fw) ;;
  let direct_fathers_for_module fw = Parent.direct_fathers_for_module (parent fw) ;;
  let duplicate_module fw = Parent.duplicate_module (parent fw) ;;
  let endingless_at_module fw = Parent.endingless_at_module (parent fw) ;;
  let find_subdir_from_suffix fw = Parent.find_subdir_from_suffix (parent fw) ;;
  
  
  let gitpush_after_backup fw = Fwc_github_configuration.gitpush_after_backup (Fwg_with_githubbing.github_configuration fw) ;;
  
  let ignored_files fw = Ancestry.ignored_files (parent fw) ;;
  let ignored_subdirectories fw = Ancestry.ignored_subdirectories (parent fw) ;;
  
  
  let  latest_changes fw = Ancestry.latest_changes(parent fw)  ;;    
  
  let list_values_from_module fw = Parent.list_values_from_module(parent fw)  ;;
  
  
  let modules_using_value fw = Parent.modules_using_value (parent fw) ;; 
  
  let  noncompilable_files fw = Ancestry.noncompilable_files(parent fw)  ;; 
  let number_of_modules fw = Parent.number_of_modules (parent fw) ;;
  
  let registered_printers fw = Parent.registered_printers (parent fw) ;;
  let root fw = Ancestry.root (parent fw) ;;
  
  let set_fw_with_dependencies fw fw_deps = 
    make fw_deps 
       (Fwg_with_githubbing.github_configuration fw) ;;
  let set_gitpush_after_backup fw gab = 
    let old_github_config = Fwg_with_githubbing.github_configuration fw in 
    let new_github_config = Fwc_github_configuration.set_gitpush_after_backup old_github_config gab in 
    make (parent fw) new_github_config ;;
  
  
  
  let  show_value_occurrences fw = Parent.show_value_occurrences(parent fw)  ;;
  
  let test_equality fw1 fw2 = 
    let gc = Fwg_with_githubbing.github_configuration in 
    let get_github_root = (fun fw->Fwc_github_configuration.root(gc fw))
    and get_dir_for_backup = (fun fw->Fwc_github_configuration.dir_for_backup(gc fw))
    and get_github_url = (fun fw->Fwc_github_configuration.github_url(gc fw))
    and get_encoding_protected_files = (fun fw->Fwc_github_configuration.encoding_protected_files(gc fw)) in 
  
    (
      Ancestry.test_equality (parent fw1) (parent fw2)
    )
    @
    (
      List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld)
      [
        "github_root",((get_github_root fw1)=(get_github_root fw2));
        "dir_for_backup",((get_dir_for_backup fw1)=(get_dir_for_backup fw2));
        "github_url",((get_github_url fw1)=(get_github_url fw2));
        "encoding_protected_files",((get_encoding_protected_files fw1)=(get_encoding_protected_files fw2));
      ]
    ) ;;
  
    
  let test_for_admissibility fw = Ancestry.test_for_admissibility (parent fw) ;;
  
  let to_fw_configuration fw = Ancestry.to_fw_configuration (parent fw) ;;
  
  let to_fw_with_archives fw = Ancestry.to_fw_with_archives (parent fw) ;;
  let to_fw_with_dependencies fw = parent fw ;;

  
  let  usual_compilable_files fw = Ancestry.usual_compilable_files(parent fw)  ;; 
  
  
end ;;  


module Crobj = struct 
  let salt = "Fwc_with_githubbing." ;;
  let label_for_parent = salt ^ "parent" ;;
  let label_for_github_config  = salt ^ "github_config" ;;
  
  
  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in 
    Fwg_with_githubbing.make 
     (Fwc_with_modular_infrastructure.Crobj.of_concrete_object (g label_for_parent))
     (Fwc_github_configuration.Crobj.of_concrete_object (g label_for_github_config))
    ;;
  
  let to_concrete_object fw = 
   let items =  
   [
     label_for_parent, Fwc_with_modular_infrastructure.Crobj.to_concrete_object ( Fwg_with_githubbing.parent fw ) ;
     label_for_github_config, Fwc_github_configuration.Crobj.to_concrete_object ( Fwg_with_githubbing.github_configuration fw ) ;
   ] in 
   Concrete_object_t.Record items ;;
  
  
end;; 
  
   

module Private = struct 

let parent = Fwg_with_githubbing.parent ;; 
let github_configuration = Fwg_with_githubbing.github_configuration ;;
let make = Fwg_with_githubbing.make;;




  let plunge_fw_config_with_github_config fw_config github_config= 
   Inherited.make (Fwc_with_modular_infrastructure.plunge_fw_configuration fw_config)
   github_config ;;
  
    
   let backup fw diff opt_msg = Fwc_github_configuration.backup  
     (Fwg_with_githubbing.github_configuration fw) diff opt_msg ;;

  exception Forget_modules_exn of Dfa_module_t.t  list ;;     


let forget_modules fw mods = 
  let old_fw_deps = Inherited.parent fw in 
  let check = Inherited.check_module_sequence_for_forgettability fw mods in 
  if check <> []
  then raise(Forget_modules_exn(check))
  else
  let (new_fw_deps,removed_files) = 
  Fwc_with_modular_infrastructure.forget_modules old_fw_deps mods in 
  let descr = String.concat " , " (Image.image Dfa_module.to_line mods) in 
  let msg="delete "^descr in 
  let diff = Dircopy_diff.destroy Dircopy_diff.empty_one removed_files  in  
  let _ = backup fw diff (Some msg) in     
  Inherited.make new_fw_deps (github_configuration fw) ;;    

let forget_nonmodular_rootlesses fw rootless_paths = 
  let old_fw_deps = Inherited.parent fw in 
  let (new_fw_deps,_) = Fwc_with_modular_infrastructure.remove_files 
         old_fw_deps rootless_paths in 
  let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
  let msg="delete "^descr in 
  let diff = Dircopy_diff.destroy Dircopy_diff.empty_one rootless_paths  in  
  let _ = backup fw diff (Some msg) in     
  Inherited.make new_fw_deps (github_configuration fw) ;;      
    
let inspect_and_update fw opt_comment = 
  let old_fw_deps = Inherited.parent fw in
  let (new_fw_deps,(_,_,changed_files))
                   =Fwc_with_modular_infrastructure.inspect_and_update old_fw_deps in 
  let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one changed_files in 
  let _ = backup fw diff opt_comment in 
  Inherited.make new_fw_deps (github_configuration fw) ;;  

let refresh fw =
  let fw_config = Inherited.to_fw_configuration fw
  and github_config = github_configuration fw in 
  let root = Fwc_configuration.root fw_config in 
  let proj_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash root) '/' in
  let _=(Unix_again.create_subdirs_and_fill_files_if_necessary root
        Fw_constant.minimal_set_of_needed_dirs 
            (Fw_constant.conventional_files_with_minimal_content proj_name)) in 
  let fw_with_deps = Fwc_with_modular_infrastructure.of_configuration fw_config in 
  Inherited.make fw_with_deps github_config  ;;

let register_rootless_paths fw rootless_paths = 
  let old_fw_deps = Inherited.parent fw in 
  let (new_fw_deps,_)=
             Fwc_with_modular_infrastructure.register_rootless_paths old_fw_deps rootless_paths in 
  let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
  let msg="register "^descr in 
  let diff = Dircopy_diff.create Dircopy_diff.empty_one rootless_paths  in  
  let _ = backup fw diff (Some msg) in     
  Inherited.make new_fw_deps (github_configuration fw) ;;   
   

let relocate_module_to fw mod_name new_subdir = 
  let old_fw_deps = Inherited.parent fw in 
  let (new_fw_deps,(_,replacements))=
  Fwc_with_modular_infrastructure.relocate_module_to old_fw_deps 
  (mod_name,new_subdir) in
  let msg="move "^(Dfa_module.to_line mod_name)^" to "^
  (Dfa_subdirectory.connectable_to_subpath new_subdir) in 
  let diff = Dircopy_diff.replace Dircopy_diff.empty_one replacements  in  
  let _ = backup fw diff (Some msg) in     
  Inherited.make new_fw_deps (github_configuration fw) ;;  

let rename_module fw old_middle_name new_nonslashed_name = 
  let old_nm=Dfn_middle.to_module old_middle_name in
  let new_nm=Dfa_module.of_line 
           (No_slashes.to_string new_nonslashed_name) in  
  let old_fw_deps = Inherited.parent fw in 
  let separated_acolytes_below=List.filter_map(
    fun mn->
    if List.mem old_nm (Fwc_with_modular_infrastructure.ancestors_for_module 
                old_fw_deps mn)
    then Some(Image.image (Dfn_full.to_rootless) 
             (Fwc_with_modular_infrastructure.acolytes_at_module old_fw_deps mn))
    else None) 
    (Fwc_with_modular_infrastructure.dep_ordered_modules old_fw_deps) in
  let all_acolytes_below=List.flatten separated_acolytes_below in
  let (new_fw_deps,changes) = 
    Fwc_with_modular_infrastructure.rename_module_on_filename_level_and_in_files 
            old_fw_deps (old_nm,new_nm,all_acolytes_below) in 
  let (_,(file_renamings,changed_files)) = changes in 
  let msg="rename "^(Dfa_module.to_line
             (Dfn_middle.to_module old_middle_name))^
                " as "^(No_slashes.to_string new_nonslashed_name) in       
  let diff1 = Dircopy_diff.replace Dircopy_diff.empty_one file_renamings  in  
  let diff2 = Dircopy_diff.add_changes diff1  changed_files  in  
  let _ = backup fw diff2 (Some msg) in  
  Inherited.make new_fw_deps (github_configuration fw) ;;   

let rename_subdirectory_as fw (old_subdir,new_subdir) = 
  let old_fw_deps = Inherited.parent fw in
  let (new_fw_deps,extra)=Fwc_with_modular_infrastructure.rename_subdirectory_as 
           old_fw_deps (old_subdir,new_subdir) in   
  let (_,original_reps) = extra in 
  let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)
        ^" as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
  let diff = Dircopy_diff.replace Dircopy_diff.empty_one original_reps in   
  let _ = backup fw diff (Some msg) in   
  Inherited.make new_fw_deps (github_configuration fw);; 
  
let replace_string fw old_s new_s = 
  let old_fw_deps = Inherited.parent fw in 
  let (new_fw_deps,(_,all_changed_files)) = 
        Fwc_with_modular_infrastructure.replace_string old_fw_deps (old_s,new_s) in 
  let msg="rename "^old_s^" as "^new_s in 
  let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one 
      all_changed_files in 
  let _ = backup fw diff (Some msg) in 
  Inherited.make new_fw_deps (github_configuration fw)  ;; 
    

let replace_value fw
      ((preceding_files,path),(old_v,new_v)) = 
      let old_fw_deps = Inherited.parent fw in 
      let (new_fw_deps,(_u_changes,all_changes)) = 
        Fwc_with_modular_infrastructure.replace_value old_fw_deps 
         ((preceding_files,path),(old_v,new_v)) in 
      let msg="rename "^old_v^" as "^new_v in 
      let diff = Dircopy_diff.add_changes 
      Dircopy_diff.empty_one all_changes in 
      let _ = backup fw diff (Some msg) in 
      Inherited.make 
       new_fw_deps (github_configuration fw)  ;;
 
   

      

 

    

end ;; 


let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
let github_configuration = Fwg_with_githubbing.github_configuration ;;
let inspect_and_update = Private.inspect_and_update ;;

let plunge_fw_config_with_github_config = Private.plunge_fw_config_with_github_config ;;
let refresh = Private.refresh ;;
let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;         
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;    


