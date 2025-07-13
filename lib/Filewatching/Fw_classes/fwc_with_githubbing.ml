(* 

#use"lib/Filewatching/Fw_classes/fwc_with_githubbing.ml";;

*)

type t = Fwg_with_githubbing.t ;;

module Inherited = struct

  module Parent = Fwc_with_batch_compilation ;;

  module Ancestry = Parent.Inherited ;;
  let parent = Fwg_with_githubbing.parent ;;
  
  let set_parent fw fw_batch =
    Fwg_with_githubbing.make fw_batch 
       (Fwg_with_githubbing.github_configuration fw) ;;
  
  
  let all_endinglesses fw = Ancestry.all_endinglesses (parent fw) ;;
  let all_moduled_mlx_files fw = Ancestry.all_moduled_mlx_files (parent fw) ;;
  let all_subdirectories fw = Ancestry.all_subdirectories (parent fw) ;;
  let ancestors_for_module fw = Ancestry.ancestors_for_module (parent fw) ;;
  let below fw = Ancestry.below (parent fw) ;;
  
  let check_module_sequence_for_forgettability fw = 
    Ancestry.check_module_sequence_for_forgettability (parent fw) ;;
  
  let check_that_no_change_has_occurred fw = Ancestry.check_that_no_change_has_occurred(parent fw)  ;;  
    
  
  let  clean_debug_dir fw = Parent.clean_debug_dir(parent fw)  ;;
  
  let  clean_exec_dir fw = Parent.clean_exec_dir(parent fw)  ;;
  
  let decipher_module fw = Ancestry.decipher_module (parent fw) ;;
  let decipher_path fw = Ancestry.decipher_path (parent fw) ;;
  let dep_ordered_modules fw = Ancestry.dep_ordered_modules (parent fw) ;;
  let directly_below fw = Ancestry.directly_below (parent fw) ;;
  let direct_fathers_for_module fw = Ancestry.direct_fathers_for_module (parent fw) ;;
  let duplicate_module fw = Ancestry.duplicate_module (parent fw) ;;
  let endingless_at_module fw = Ancestry.endingless_at_module (parent fw) ;;
  let find_subdir_from_suffix fw = Ancestry.find_subdir_from_suffix (parent fw) ;;
  
  
  let gitpush_after_backup fw = Fwc_github_configuration.gitpush_after_backup (Fwg_with_githubbing.github_configuration fw) ;;
  
  let ignored_files fw = Ancestry.ignored_files (parent fw) ;;
  let ignored_subdirectories fw = Ancestry.ignored_subdirectories (parent fw) ;;
  
  
  let  latest_changes fw = Ancestry.latest_changes(parent fw)  ;;    
  
  let list_values_from_module fw = Ancestry.list_values_from_module(parent fw)  ;;
  
  let  modern_recompile fw changed_mods = 
    set_parent fw (Parent.modern_recompile
         (Fwg_with_githubbing.parent fw) changed_mods ) ;;
  
  
  let modules_using_value fw = Ancestry.modules_using_value (parent fw) ;; 
  
  let  noncompilable_files fw = Ancestry.noncompilable_files(parent fw)  ;; 
  
  let number_of_modules fw = Ancestry.number_of_modules (parent fw) ;;
  let  preq_types_with_extra_info fw = Parent.preq_types_with_extra_info(parent fw)  ;;
  
  let root fw = Ancestry.root (parent fw) ;;
  
  let set_gitpush_after_backup fw gab = 
    let old_github_config = Fwg_with_githubbing.github_configuration fw in 
    let new_github_config = Fwc_github_configuration.set_gitpush_after_backup old_github_config gab in 
    Fwg_with_githubbing.make (parent fw) new_github_config ;;
  
  
  
  let  show_value_occurrences fw = Ancestry.show_value_occurrences(parent fw)  ;;
  
  let  start_debugging fw = Parent.start_debugging(parent fw)  ;;
  
  let  start_executing fw = Parent.start_executing(parent fw)  ;;
  
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
  
  let to_fw_with_dependencies fw = Ancestry.to_fw_with_dependencies (parent fw) ;;

  let  up_to_date_elesses fw = Parent.up_to_date_elesses(parent fw)  ;;
  
  
  let  usual_compilable_files fw = Ancestry.usual_compilable_files(parent fw)  ;; 
  
  
end ;;  


module Crobj = struct 
  let salt = "Fwc_with_githubbing." ;;
  let label_for_parent = salt ^ "parent" ;;
  let label_for_github_config  = salt ^ "github_config" ;;
  
  
  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in 
    Fwg_with_githubbing.make 
     (Fwc_with_batch_compilation.Crobj.of_concrete_object (g label_for_parent))
     (Fwc_github_configuration.Crobj.of_concrete_object (g label_for_github_config))
    ;;
  
  let to_concrete_object fw = 
   let items =  
   [
     label_for_parent, Fwc_with_batch_compilation.Crobj.to_concrete_object ( Fwg_with_githubbing.parent fw ) ;
     label_for_github_config, Fwc_github_configuration.Crobj.to_concrete_object ( Fwg_with_githubbing.github_configuration fw ) ;
   ] in 
   Concrete_object_t.Record items ;;
  
  
end;; 
  
   

module Private = struct 

let parent = Fwg_with_githubbing.parent ;; 
let github_configuration = Fwg_with_githubbing.github_configuration ;;
let make = Fwg_with_githubbing.make;;


let usual_extension fw_batch backup_dir gab git_url enc_files = 
  make fw_batch (Fwc_github_configuration.make 
 ~v_root:(Fwc_with_batch_compilation.Inherited.root fw_batch)
  ~v_dir_for_backup:backup_dir
~v_gitpush_after_backup:gab
~v_github_url:git_url
~v_encoding_protected_files:enc_files) ;;


  let set_parent ~child ~new_parent = 
   make new_parent child ;;
  

  let usual_batch fw modnames = 
    let (new_parent,rejected_ones,accepted_ones) = 
      Fwc_with_batch_compilation.usual_batch (parent fw) modnames in 
    (Inherited.set_parent fw new_parent,rejected_ones,accepted_ones) ;; 
  
  

  
  let of_fw_config_and_github_config fw_config github_config = 
    make (Fwc_with_batch_compilation.of_configuration fw_config)
       github_config ;;
   


  let plunge_fw_config_with_github_config fw_config github_config= 
   make (Fwc_with_batch_compilation.plunge_fw_configuration fw_config)
   github_config ;;
  
    
   let backup fw diff opt_msg = Fwc_github_configuration.backup  
     (Fwg_with_githubbing.github_configuration fw) diff opt_msg ;;

  exception Forget_modules_exn of Dfa_module_t.t  list ;;     

  let forget_modules fw mods = 
    let check = Inherited.check_module_sequence_for_forgettability fw mods in 
    if check <> []
    then raise(Forget_modules_exn(check))
    else
    let (new_parent,removed_files) = Fwc_with_batch_compilation.forget_modules (parent fw) mods in 
    let descr = String.concat " , " (Image.image Dfa_module.to_line mods) in 
    let msg="delete "^descr in 
    let diff = Dircopy_diff.destroy Dircopy_diff.empty_one removed_files  in  
    let _ = backup fw diff (Some msg) in     
    set_parent ~child:(github_configuration fw) ~new_parent ;;     

  let forget_nonmodular_rootlesses fw rootless_paths=
      let new_parent = Fwc_with_batch_compilation.remove_files (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="delete "^descr in 
      let diff = Dircopy_diff.destroy Dircopy_diff.empty_one rootless_paths  in  
      let _ = backup fw diff (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;;     
    
  
  let register_rootless_paths fw rootless_paths = 
      let new_parent = Fwc_with_batch_compilation.register_rootless_paths (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="register "^descr in 
      let diff = Dircopy_diff.create Dircopy_diff.empty_one rootless_paths  in  
      let _ = backup fw diff (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;;  

   

  let relocate_module_to fw mod_name new_subdir = 
      let (new_parent,(_,replacements)) = Fwc_with_batch_compilation.relocate_module_to (parent fw) mod_name new_subdir in 
      let msg="move "^(Dfa_module.to_line mod_name)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
      let diff = Dircopy_diff.replace Dircopy_diff.empty_one replacements  in  
      let _ = backup fw diff (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;; 

  let rename_module fw old_middle_name new_nonslashed_name = 
      let (new_parent,(_,(file_renamings,changed_files))) = Fwc_with_batch_compilation.rename_module (parent fw) old_middle_name new_nonslashed_name in 
      let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
              " as "^(No_slashes.to_string new_nonslashed_name) in       
      let diff1 = Dircopy_diff.replace Dircopy_diff.empty_one file_renamings  in  
      let diff2 = Dircopy_diff.add_changes diff1  changed_files  in  
      let _ = backup fw diff2 (Some msg) in     
      set_parent ~child:(github_configuration fw) ~new_parent ;;    

  let rename_subdirectory_as fw (old_subdir,new_subdir) = 
    let (new_parent,(_,original_reps)) = Fwc_with_batch_compilation.rename_subdirectory_as 
          (parent fw) (old_subdir,new_subdir) in 
    let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
          " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
    let diff = Dircopy_diff.replace Dircopy_diff.empty_one original_reps in   
    let _ = backup fw diff (Some msg) in     
    set_parent ~child:(github_configuration fw) ~new_parent ;; 

  
  let replace_string fw old_s new_s = 
      let (parent1,(changed_modules_in_any_order,all_changed_files)) = 
      Fwc_with_batch_compilation.replace_string (parent fw) old_s new_s  in 
      let new_parent = Fwc_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
      let msg="rename "^old_s^" as "^new_s in 
      let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changed_files in 
      let _ = backup fw diff (Some msg) in 
      set_parent ~child:(github_configuration fw) ~new_parent ;;

  let replace_value fw ((preceding_files,path),(old_v,new_v)) = 
        let (parent1,(changed_modules_in_any_order,all_changes)) = 
        Fwc_with_batch_compilation.replace_value (parent fw) ((preceding_files,path),(old_v,new_v))  in 
        let new_parent = Fwc_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
        let msg="rename "^old_v^" as "^new_v in 
        let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changes in 
        let _ = backup fw diff (Some msg) in 
        set_parent ~child:(github_configuration fw) ~new_parent ;; 
 
   
  let usual_recompile fw opt_comment = 
    let (new_parent,(_changed_uc,changed_files)) = Fwc_with_batch_compilation.usual_recompile (parent fw)  in 
    let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one changed_files in 
    let _ = backup fw diff opt_comment in 
    set_parent ~child:(github_configuration fw) ~new_parent ;;

 

    

end ;; 


let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
let github_configuration = Fwg_with_githubbing.github_configuration ;;

let of_fw_with_batch_compilation =Private.usual_extension ;;
let of_fw_config_and_github_config = Private.of_fw_config_and_github_config ;;
let parent = Fwg_with_githubbing.parent ;;
let plunge_fw_config_with_github_config = Private.plunge_fw_config_with_github_config ;;
let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;         
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;    
let usual_recompile = Private.usual_recompile ;;

