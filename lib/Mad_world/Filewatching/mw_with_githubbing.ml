(* 

#use"lib/Mad_world/Filewatching/mw_with_githubbing.ml";;

*)

(* Beginning of loose version of the genesis of Mw_with_persisting.t *)

type t = (* Subclass of *) Mw_poly_t.t  ;;

module Genesis = struct 

let parent x = x ;;

let extend x = x ;;

end ;;  

(* End of loose version of the genesis of Mw_with_persisting.t *)

(* Beginning of tight version of the genesis of Mw_with_persisting.t *)
(*


type t = { parent : Mw_poly_t.t } ;;

module Genesis = struct 

let parent x = x.parent ;;

let extend x = {parent = x} ;;

end ;;  


*)
(* End of tight version of the genesis of Mw_with_persisting.t *)


module Private = struct

  let parent = Mw_poly.parent ;; 
  
  let set_parent = Mw_poly.set_parent ;;
  

  let usual_batch fw modnames = 
    let (new_parent,rejected_ones,accepted_ones) = 
      Mw_with_batch_compilation.usual_batch (parent fw) modnames in 
    (set_parent ~child:fw ~new_parent,rejected_ones,accepted_ones) ;; 
  
  let usual_extension fw_batch backup_dir gab git_url enc_files = 
      Mw_poly.extend_fw_with_batch_compilation_to_fw_with_githubbing 
      fw_batch
      ~dir_for_backup:backup_dir 
      ~gitpush_after_backup:gab
      ~github_url:git_url
      ~encoding_protected_files:enc_files ;;

  
  let of_fw_config_and_github_config fw_config github_config = usual_extension 
    (Mw_with_batch_compilation.of_configuration fw_config)
    (Mw_poly.dir_for_backup github_config) 
    (Mw_poly.gitpush_after_backup github_config) 
    (Mw_poly.github_url github_config)
    (Mw_poly.encoding_protected_files github_config);;


  let plunge_fw_config_with_github_config fw_config github_config= usual_extension 
      (Mw_with_batch_compilation.plunge_fw_configuration fw_config)
      (Mw_poly.dir_for_backup github_config) 
      (Mw_poly.gitpush_after_backup github_config) 
      (Mw_poly.github_url github_config)
      (Mw_poly.encoding_protected_files github_config);;
    
  exception Forget_modules_exn of Dfa_module_t.t  list ;;     

  let forget_modules fw mods = 
    let check = Mw_with_dependencies.check_module_sequence_for_forgettability (parent fw) mods in 
    if check <> []
    then raise(Forget_modules_exn(check))
    else
    let (new_parent,removed_files) = Mw_with_batch_compilation.forget_modules (parent fw) mods in 
    let descr = String.concat " , " (Image.image Dfa_module.to_line mods) in 
    let msg="delete "^descr in 
    let diff = Dircopy_diff.destroy Dircopy_diff.empty_one removed_files  in  
    let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff (Some msg) in     
    set_parent ~child:fw ~new_parent ;;     

  let forget_nonmodular_rootlesses fw rootless_paths=
      let new_parent = Mw_with_batch_compilation.remove_files (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="delete "^descr in 
      let diff = Dircopy_diff.destroy Dircopy_diff.empty_one rootless_paths  in  
      let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff (Some msg) in     
      set_parent ~child:fw ~new_parent ;;     
    
  
  let register_rootless_paths fw rootless_paths = 
      let new_parent = Mw_with_batch_compilation.register_rootless_paths (parent fw) rootless_paths in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
      let msg="register "^descr in 
      let diff = Dircopy_diff.create Dircopy_diff.empty_one rootless_paths  in  
      let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff (Some msg) in     
      set_parent ~child:fw ~new_parent ;;  

   

  let relocate_module_to fw mod_name new_subdir = 
      let (new_parent,(_,replacements)) = Mw_with_batch_compilation.relocate_module_to (parent fw) mod_name new_subdir in 
      let msg="move "^(Dfa_module.to_line mod_name)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
      let diff = Dircopy_diff.replace Dircopy_diff.empty_one replacements  in  
      let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff (Some msg) in     
      set_parent ~child:fw ~new_parent ;; 

  let rename_module fw old_middle_name new_nonslashed_name = 
      let (new_parent,(_,(file_renamings,changed_files))) = Mw_with_batch_compilation.rename_module (parent fw) old_middle_name new_nonslashed_name in 
      let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
              " as "^(No_slashes.to_string new_nonslashed_name) in       
      let diff1 = Dircopy_diff.replace Dircopy_diff.empty_one file_renamings  in  
      let diff2 = Dircopy_diff.add_changes diff1  changed_files  in  
      let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff2 (Some msg) in     
      set_parent ~child:fw ~new_parent ;;    

  let rename_subdirectory_as fw (old_subdir,new_subdir) = 
    let (new_parent,(_,original_reps)) = Mw_with_batch_compilation.rename_subdirectory_as 
          (parent fw) (old_subdir,new_subdir) in 
    let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
          " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
    let diff = Dircopy_diff.replace Dircopy_diff.empty_one original_reps in   
    let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff (Some msg) in     
    set_parent ~child:fw ~new_parent ;; 

  
  let replace_string fw old_s new_s = 
      let (parent1,(changed_modules_in_any_order,all_changed_files)) = 
      Mw_with_batch_compilation.replace_string (parent fw) old_s new_s  in 
      let new_parent = Mw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
      let msg="rename "^old_s^" as "^new_s in 
      let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changed_files in 
      let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff (Some msg) in 
      set_parent ~child:fw ~new_parent ;;

  let replace_value fw ((preceding_files,path),(old_v,new_v)) = 
        let (parent1,(changed_modules_in_any_order,all_changes)) = 
        Mw_with_batch_compilation.replace_value (parent fw) ((preceding_files,path),(old_v,new_v))  in 
        let new_parent = Mw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
        let msg="rename "^old_v^" as "^new_v in 
        let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one all_changes in 
        let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff (Some msg) in 
        set_parent ~child:fw ~new_parent ;; 
 
   
  let usual_recompile fw opt_comment = 
    let (new_parent,(_changed_uc,changed_files)) = Mw_with_batch_compilation.usual_recompile (parent fw)  in 
    let diff = Dircopy_diff.add_changes Dircopy_diff.empty_one changed_files in 
    let _ = Mw_transmit_change_to_github.backup (Mw_poly.to_github_configuration fw) diff opt_comment in 
    set_parent ~child:fw ~new_parent ;;
    
end;;  
      
module Constructor = struct
let of_fw_config_and_github_config fw_config github_config = Private.usual_extension 
(Mw_with_batch_compilation.of_configuration fw_config)
(Mw_poly.dir_for_backup github_config) 
(Mw_poly.gitpush_after_backup github_config) 
(Mw_poly.github_url github_config)
(Mw_poly.encoding_protected_files github_config);;

let of_fw_with_batch_compilation =Private.usual_extension ;;

let plunge_fw_config_with_github_config fw_config github_config= Private.usual_extension 
  (Mw_with_batch_compilation.plunge_fw_configuration fw_config)
  (Mw_poly.dir_for_backup github_config) 
  (Mw_poly.gitpush_after_backup github_config) 
  (Mw_poly.github_url github_config)
  (Mw_poly.encoding_protected_files github_config);;

end ;;

module Not_Inherited = struct



let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  

let modern_recompile fw changed_modules_in_any_order =  
  Mw_with_batch_compilation.modern_recompile fw changed_modules_in_any_order ;;

let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;         
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;    
let usual_recompile = Private.usual_recompile ;;

end ;;


let all_endinglesses fw =
  Mw_with_dependencies.all_endinglesses (Genesis.parent fw) ;;

let ancestors_for_module fw mn=
  Mw_with_dependencies.ancestors_for_module (Genesis.parent fw) mn;;

let below fw mn =
    Mw_with_dependencies.below (Genesis.parent fw) mn ;;

let check_that_no_change_has_occurred fw= 
    Mw_file_watcher.check_that_no_change_has_occurred 
      (Genesis.parent fw) ;;

let clean_debug_dir fw = 
  Mw_with_batch_compilation.clean_debug_dir (Genesis.parent fw) ;;


let clean_exec_dir fw = 
  Mw_with_batch_compilation.clean_exec_dir (Genesis.parent fw) ;;

let decipher_module fw mn=
    Mw_with_dependencies.decipher_module (Genesis.parent fw) mn;;

let decipher_path fw path=
    Mw_with_dependencies.decipher_path (Genesis.parent fw) path;;    

let dep_ordered_modules fw = 
      Mw_with_dependencies.dep_ordered_modules fw ;;  
    

let direct_fathers_for_module fw mn =
    Mw_with_dependencies.direct_fathers_for_module (Genesis.parent fw) mn ;;

let directly_below fw mn =
    Mw_with_dependencies.directly_below (Genesis.parent fw) mn ;;

let duplicate_module fw old_t1 old_t2=
  Mw_with_dependencies.duplicate_module (Genesis.parent fw) old_t1 old_t2 ;;

let endingless_at_module fw mn=
  Mw_with_dependencies.endingless_at_module (Genesis.parent fw) mn;;

let find_subdir_from_suffix fw possibly_slashed_suffix =
    Mw_with_dependencies.find_subdir_from_suffix 
    (Genesis.parent fw) possibly_slashed_suffix ;; 

let forget_modules = Not_Inherited.forget_modules ;;

let forget_nonmodular_rootlesses = Not_Inherited.forget_nonmodular_rootlesses ;;

let gitpush_after_backup fw = 
  Mw_poly.gitpush_after_backup (Genesis.parent fw) ;;

let latest_changes fw =
    Mw_with_archives.latest_changes (Genesis.parent fw) ;;

let list_values_from_module fw mn =
    Mw_with_dependencies.list_values_from_module (Genesis.parent fw) mn ;;

let modern_recompile = Not_Inherited.modern_recompile ;;

let modules_using_value fw value_name =
    Mw_with_dependencies.modules_using_value (Genesis.parent fw) value_name ;;

let noncompilable_files fw = 
   Mw_with_archives.noncompilable_files (Genesis.parent fw) ;;

let number_of_modules fw = 
   Mw_with_dependencies.number_of_modules (Genesis.parent fw) ;;



let overwrite_file_if_it_exists fw pair =  
    Mw_with_dependencies.overwrite_file_if_it_exists (Genesis.parent fw) pair ;;

    
   
let register_rootless_paths = Not_Inherited.register_rootless_paths ;; 
  
let relocate_module_to = Not_Inherited.relocate_module_to ;; 
  
let rename_module = Not_Inherited.rename_module ;;

let rename_subdirectory_as = Not_Inherited.rename_subdirectory_as ;;

let replace_string = Not_Inherited.replace_string ;;

let replace_value = Not_Inherited.replace_value ;;

let root fw = fw.Mw_poly_t.root ;;

let set_gitpush_after_backup fw gab= 
Genesis.extend(
  Mw_poly.set_gitpush_after_backup (Genesis.parent fw) gab);;

let show_value_occurrences fw mn = 
  Mw_with_dependencies.show_value_occurrences (Genesis.parent fw) mn ;;  

let start_debugging fw =
  Mw_with_batch_compilation.start_debugging (Genesis.parent fw) ;;

let start_executing fw short_path=
 Mw_with_batch_compilation.start_executing (Genesis.parent fw) short_path ;;


let usual_compilable_files fw = 
    Mw_with_archives.usual_compilable_files (Genesis.parent fw) ;;
 

let usual_recompile = Not_Inherited.usual_recompile ;;




