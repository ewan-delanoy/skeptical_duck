(* 

#use"Filewatching/fw_with_githubbing.ml";;

*)


module Private = struct

  let parent = Fw_poly.parent ;; 
  
  let set_parent = Fw_poly.set_parent ;;
  
  
  let usual_batch fw modnames = 
    let (new_parent,rejected_ones,accepted_ones) = Fw_with_batch_compilation.usual_batch (parent fw) modnames in 
    (set_parent fw new_parent,rejected_ones,accepted_ones) ;; 
  
  let usual_extension fw_batch backup_dir gab git_url enc_files = 
      Fw_poly.extend_fw_with_batch_compilation_to_fw_with_githubbing 
      fw_batch
      ~dir_for_backup:backup_dir 
      ~gitpush_after_backup:gab
      ~github_url:git_url
      ~encoding_protected_files:enc_files ;;

  let github_config = 
    Fw_poly.restrict_fw_with_githubbing_to_github_configuration ;;

  
  let of_fw_config_and_github_config fw_config github_config = usual_extension 
    (Fw_with_batch_compilation.of_configuration fw_config)
    (Fw_poly.dir_for_backup github_config) 
    (Fw_poly.gitpush_after_backup github_config) 
    (Fw_poly.github_url github_config)
    (Fw_poly.encoding_protected_files github_config);;


  let plunge_fw_config_with_github_config fw_config github_config= usual_extension 
      (Fw_with_batch_compilation.plunge_fw_configuration fw_config)
      (Fw_poly.dir_for_backup github_config) 
      (Fw_poly.gitpush_after_backup github_config) 
      (Fw_poly.github_url github_config)
      (Fw_poly.encoding_protected_files github_config);;
    
  exception Forget_modules_exn of Dfa_module_t.t  list ;;     

  let forget_modules fw mods = 
    let check = Fw_with_dependencies.check_module_sequence_for_forgettability (parent fw) mods in 
    if check <> []
    then raise(Forget_modules_exn(check))
    else
    let (new_parent,removed_files) = Fw_with_batch_compilation.forget_modules (parent fw) mods in 
    let descr = String.concat " , " (Image.image Dfa_module.to_line mods) in 
    let msg="delete "^descr in 
    let diff = Dircopy_diff.destroy Dircopy_diff.empty_one removed_files  in  
    let _ = Transmit_change_to_github.backup (github_config fw) diff (Some msg) in     
    set_parent fw new_parent ;;     

  let forget_nonmodular_rootlesses fw rootless_paths=
      let new_parent = Fw_with_batch_compilation.remove_files (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="delete "^descr in 
      let diff = Dircopy_diff.destroy Dircopy_diff.empty_one rootless_paths  in  
      let _ = Transmit_change_to_github.backup (github_config fw) diff (Some msg) in     
      set_parent fw new_parent ;;     
    
  
  let register_rootless_paths fw rootless_paths = 
      let new_parent = Fw_with_batch_compilation.register_rootless_paths (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="register "^descr in 
      let diff = Dircopy_diff.create Dircopy_diff.empty_one rootless_paths  in  
      let _ = Transmit_change_to_github.backup (github_config fw) diff (Some msg) in     
      set_parent fw new_parent ;;  

   

  let relocate_module_to fw mod_name new_subdir = 
      let (new_parent,(_,replacements)) = Fw_with_batch_compilation.relocate_module_to (parent fw) mod_name new_subdir in 
      let msg="move "^(Dfa_module.to_line mod_name)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
      let diff = Dircopy_diff.replace Dircopy_diff.empty_one replacements  in  
      let _ = Transmit_change_to_github.backup (github_config fw) diff (Some msg) in     
      set_parent fw new_parent ;; 

  let rename_module fw old_middle_name new_nonslashed_name = 
      let (new_parent,(_,(file_renamings,changed_files))) = Fw_with_batch_compilation.rename_module (parent fw) old_middle_name new_nonslashed_name in 
      let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
              " as "^(No_slashes.to_string new_nonslashed_name) in       
      let diff1 = Dircopy_diff.replace Dircopy_diff.empty_one file_renamings  in  
      let diff2 = Dircopy_diff.add_changes diff1  changed_files  in  
      let _ = Transmit_change_to_github.backup (github_config fw) diff2 (Some msg) in     
      set_parent fw new_parent ;;    

  let rename_subdirectory_as fw (old_subdir,new_subdir) = 
    let (new_parent,(_,original_reps)) = Fw_with_batch_compilation.rename_subdirectory_as 
          (parent fw) (old_subdir,new_subdir) in 
    let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
          " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
    let diff = Dircopy_diff.replace Dircopy_diff.empty_one original_reps in   
    let _ = Transmit_change_to_github.backup (github_config fw) diff (Some msg) in     
    set_parent fw new_parent ;; 

  
  let replace_string fw old_s new_s = 
      let (parent1,(changed_modules_in_any_order,all_changed_files)) = 
      Fw_with_batch_compilation.replace_string (parent fw) old_s new_s  in 
      let parent2 = Fw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
      let msg="rename "^old_s^" as "^new_s in 
      let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changed_files in 
      let _ = Transmit_change_to_github.backup (github_config fw) diff (Some msg) in 
      set_parent fw parent2 ;;

  let replace_value fw ((preceding_files,path),(old_v,new_v)) = 
        let (parent1,(changed_modules_in_any_order,all_changes)) = 
        Fw_with_batch_compilation.replace_value (parent fw) ((preceding_files,path),(old_v,new_v))  in 
        let parent2 = Fw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
        let msg="rename "^old_v^" as "^new_v in 
        let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changes in 
        let _ = Transmit_change_to_github.backup (github_config fw) diff (Some msg) in 
        set_parent fw parent2 ;; 
 
   
  let usual_recompile fw opt_comment = 
    let (new_parent,(changed_uc,changed_files)) = Fw_with_batch_compilation.usual_recompile (parent fw)  in 
    let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one changed_files in 
    let _ = Transmit_change_to_github.backup (github_config fw) diff opt_comment in 
    set_parent fw new_parent ;;
    

end;;  
      

let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
let github_configuration = Private.github_config ;;     
let of_fw_with_batch_compilation =Private.usual_extension ;;
let of_fw_config_and_github_config = Private.of_fw_config_and_github_config ;;
let plunge_fw_config_with_github_config = Private.plunge_fw_config_with_github_config ;;
let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;         
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;    
let to_fw_configuration = Fw_poly.restrict_fw_with_githubbing_to_fw_configuration ;;
let usual_recompile = Private.usual_recompile ;;

