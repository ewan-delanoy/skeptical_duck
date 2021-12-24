(* 

#use "Compilation_management/coma_state.ml";;

*)

module Private = struct

  let parent cs = cs.Coma_state_t.parent ;; 
  
  let set_parent fw batch_compiler = { 
     fw with 
     Coma_state_t.parent = batch_compiler ;
  } ;;
  
  let below cs mn = Fw_with_batch_compilation.below (parent cs) mn ;;
  let directly_below cs mn = Fw_with_batch_compilation.directly_below (parent cs) mn ;;
  let root cs = Fw_with_batch_compilation.root (parent cs) ;;
  
  let usual_batch cs modnames = 
    let (new_parent,rejected_ones,accepted_ones) = Fw_with_batch_compilation.usual_batch (parent cs) modnames in 
    (set_parent cs new_parent,rejected_ones,accepted_ones) ;; 
  
  
  let salt = "Coma_"^"state.";;
    
  let parent_label                         = salt ^ "parent";;
  let dir_for_backup_label                 = salt ^ "dir_for_backup";;
  let gitpush_after_backup_label           = salt ^ "gitpush_after_backup";;
  let github_url_label                     = salt ^ "github_url";;
  let encoding_protected_files_label       = salt ^ "encoding_protected_files";;
  
  
  let of_concrete_object ccrt_obj = 
     let g=Concrete_object.get_record ccrt_obj in
     {
        Coma_state_t.parent = Fw_with_batch_compilation.of_concrete_object(g parent_label);
        dir_for_backup = Dfa_root.of_concrete_object(g dir_for_backup_label);
        gitpush_after_backup = Crobj_converter.bool_of_concrete_object (g gitpush_after_backup_label);
        github_url = Crobj_converter.string_of_concrete_object (g github_url_label);
        encoding_protected_files = Dfn_rootless.pair_list_of_concrete_object (g encoding_protected_files_label);
     };; 
  
  let to_concrete_object cs=
     let items= 
     [
      parent_label, Fw_with_batch_compilation.to_concrete_object (parent cs);
      dir_for_backup_label, Dfa_root.to_concrete_object cs.Coma_state_t.dir_for_backup;
      gitpush_after_backup_label, Crobj_converter.bool_to_concrete_object  cs.Coma_state_t.gitpush_after_backup;
      github_url_label, Crobj_converter.string_to_concrete_object cs.Coma_state_t.github_url;
      encoding_protected_files_label, Dfn_rootless.pair_list_to_concrete_object cs.Coma_state_t.encoding_protected_files;
     ]  in
     Concrete_object_t.Record items;;
  
  
  
  
    
    let empty_one config backup_dir gab git_url enc_files=
      {
        Coma_state_t.parent = Fw_with_batch_compilation.empty_one config;
        dir_for_backup = backup_dir;
        gitpush_after_backup = gab;
        github_url = git_url;
        encoding_protected_files = enc_files;
  
      };;
    
  exception Forget_modules_exn of Dfa_module_t.t  list ;;     

  let forget_modules cs mods = 
    let check = Fw_with_batch_compilation.check_module_sequence_for_forgettability (parent cs) mods in 
    if check <> []
    then raise(Forget_modules_exn(check))
    else
    let parent1 = Fw_with_batch_compilation.forget_modules (parent cs) mods in 
    let parent2 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
              parent1 None in  
      set_parent cs parent2 ;;      

  let forget_nonmodular_rootlesses cs rootless_paths=
      let parent1 = Fw_with_batch_compilation.remove_files (parent cs) rootless_paths in 
      let parent2 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
              parent1 None in  
      set_parent cs parent2 ;;     
    
  
  let read_persistent_version x=
    let full_path=Dfn_join.root_to_rootless (root x)  Coma_constant.rootless_path_for_targetfile in
    let ap= Dfn_full.to_absolute_path full_path in
    let the_archive=Io.read_whole_file ap in
    let archived_object = Crobj_parsing.parse the_archive in 
    of_concrete_object archived_object;;      
  
  let register_rootless_paths cs mod_names = 
      let parent1 = Fw_with_batch_compilation.register_rootless_paths (parent cs) mod_names in 
      let descr = String.concat " , " (Image.image Dfn_rootless.to_line mod_names) in 
      let msg="register "^descr in 
      let parent2 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
              parent1 (Some msg) in  
      set_parent cs parent2 ;;  

   

  let relocate_module_to cs mod_name new_subdir = 
      let parent1 = Fw_with_batch_compilation.relocate_module_to (parent cs) mod_name new_subdir in 
      let msg="move "^(Dfa_module.to_line mod_name)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
      let parent2 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
              parent1 (Some msg) in  
      set_parent cs parent2 ;;  

  let rename_module cs old_middle_name new_nonslashed_name = 
      let (parent1,extra) = Fw_with_batch_compilation.rename_module (parent cs) old_middle_name new_nonslashed_name in 
      let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
              " as "^(No_slashes.to_string new_nonslashed_name) in       
      let parent2 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
              parent1 (Some msg) in     
      (set_parent cs parent2,extra) ;; 

  let rename_string_or_value cs old_sov new_sov = 
      let (parent1,changed_modules_in_any_order) = 
      Fw_with_batch_compilation.rename_string_or_value (parent cs) old_sov new_sov  in 
      let parent2 = Fw_with_batch_compilation.modern_recompile parent1 changed_modules_in_any_order in 
      let msg="rename "^old_sov^" as "^new_sov in 
      let parent3 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
        parent2 (Some msg) in 
      set_parent cs parent3 ;;      

  let rename_subdirectory_as cs (old_subdir,new_subdir) = 
    let parent1 = Fw_with_batch_compilation.rename_subdirectory_as 
          (parent cs) (old_subdir,new_subdir) in 
    let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
          " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
    let parent2 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
       parent1 (Some msg) in 
    set_parent cs parent2 ;; 

  let usual_recompile cs opt_comment = 
    let (parent1,changed_uc) = Fw_with_batch_compilation.inspect_and_update (parent cs)  in 
    let unordered_mods = Image.image Dfn_rootless.to_module changed_uc in  
    let parent2 = Fw_with_batch_compilation.modern_recompile parent1 unordered_mods  in 
    let parent3 = Fw_with_batch_compilation.reflect_latest_changes_in_github 
      parent2 opt_comment in 
    set_parent cs parent3 ;;   

end;;  
        
  
  let all_endinglesses cs = Fw_with_batch_compilation.all_endinglesses (Private.parent cs) ;;
  let all_ml_absolute_paths cs = Fw_with_batch_compilation.all_ml_absolute_paths (Private.parent cs) ;;
  let all_mlx_files cs = Fw_with_batch_compilation.all_mlx_files (Private.parent cs) ;;
  let all_subdirectories cs = Fw_with_batch_compilation.all_subdirectories (Private.parent cs) ;;
  let ancestors_for_module cs mn = Fw_with_batch_compilation.ancestors_for_module (Private.parent cs) mn ;;
  let below cs mn = Fw_with_batch_compilation.below (Private.parent cs) mn ;;
  let census_of_foreigners cs = Fw_with_batch_compilation.census_of_foreigners (Private.parent cs) ;;
  let check_module_sequence_for_forgettability cs = Fw_with_batch_compilation.check_module_sequence_for_forgettability (Private.parent cs) ;;
  let check_that_no_change_has_occurred cs =
    Fw_with_batch_compilation.check_that_no_change_has_occurred (Private.parent cs) ;; 
  let clean_debug_dir cs = Fw_with_batch_compilation.clean_debug_dir (Private.parent cs) ;;
  let clean_exec_dir cs = Fw_with_batch_compilation.clean_exec_dir (Private.parent cs) ;;
  let configuration cs= Fw_with_batch_compilation.configuration (Private.parent cs) ;;
  let dep_ordered_modules cs = Fw_with_batch_compilation.dep_ordered_modules (Private.parent cs) ;;
  let direct_fathers_for_module cs mn = Fw_with_batch_compilation.direct_fathers_for_module (Private.parent cs) mn ;;
  let directly_below cs mn = Fw_with_batch_compilation.directly_below (Private.parent cs) mn ;;
  let duplicate_module cs  vague_mname1 vague_mname2 = Fw_with_batch_compilation.duplicate_module (Private.parent cs) vague_mname1 vague_mname2 ;;
  let empty_one = Private.empty_one ;;
  let endingless_at_module cs mn = Fw_with_batch_compilation.endingless_at_module (Private.parent cs) mn ;;
  let find_subdir_from_suffix cs = Fw_with_batch_compilation.find_subdir_from_suffix (Private.parent cs) ;;
  let forget_modules = Private.forget_modules ;; 
  let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
  let gitpush_after_backup cs=(configuration cs).Fw_configuration_t.gitpush_after_backup;;   
  let inspect_and_update cs  = 
      let (new_parent,changed_usual_compilables) = Fw_with_batch_compilation.inspect_and_update (Private.parent cs)  in 
      (Private.set_parent cs new_parent,changed_usual_compilables) ;;   
  let latest_changes fw = Fw_with_batch_compilation.latest_changes (Private.parent fw)  ;;      
  let list_values_from_module cs mn = 
    Fw_with_batch_compilation.list_values_from_module  (Private.parent cs) mn ;;
  let modern_recompile cs changed_modules_in_any_order = 
    let new_parent = Fw_with_batch_compilation.modern_recompile (Private.parent cs) changed_modules_in_any_order in 
    Private.set_parent cs new_parent ;; 
  let modules_using_value cs module_name =
      Fw_with_batch_compilation.modules_using_value (Private.parent cs) module_name ;;  
  let noncompilable_files cs = 
      Fw_with_batch_compilation.noncompilable_files (Private.parent cs) ;; 
  let number_of_modules fw = Fw_with_batch_compilation.number_of_modules (Private.parent fw) ;;    
  let of_configuration config backup_dir gab git_url enc_files = 
    {
      Coma_state_t.parent = Fw_with_batch_compilation.of_configuration config;
      dir_for_backup = backup_dir;
      gitpush_after_backup = gab;
      github_url = git_url;
      encoding_protected_files = enc_files;
    };;
  
  let of_concrete_object = Private.of_concrete_object ;;  
  let of_fw_with_batch_compilation batch_compiler backup_dir gab git_url enc_files = 
    {
      Coma_state_t.parent = batch_compiler ;
      dir_for_backup = backup_dir;
      gitpush_after_backup = gab;
      github_url = git_url;
      encoding_protected_files = enc_files;
    };;
    
  let preq_types_with_extra_info cs = 
    Fw_with_batch_compilation.preq_types_with_extra_info (Private.parent cs) ;; 
  let read_persistent_version = Private.read_persistent_version ;;
  let reflect_latest_changes_in_github cs opt_msg=
      let new_parent = Fw_with_batch_compilation.reflect_latest_changes_in_github (Private.parent cs) opt_msg in 
      Private.set_parent cs new_parent ;;   
  let register_rootless_paths = Private.register_rootless_paths ;;      
  let relocate_module_to  = Private.relocate_module_to ;;  
  let remove_files cs rps = 
      let new_parent = Fw_with_batch_compilation.remove_files (Private.parent cs) rps in 
      Private.set_parent cs new_parent ;;        
  let rename_module = Private.rename_module ;;   
  let rename_string_or_value = Private.rename_string_or_value ;;
  let rename_subdirectory_as = Private.rename_subdirectory_as ;;      
  let root = Private.root ;;
  let set_gitpush_after_backup cs bowl = 
    let new_parent = Fw_with_batch_compilation.set_gitpush_after_backup (Private.parent cs) bowl in 
    Private.set_parent cs new_parent ;;   
  let show_value_occurrences cs t = 
    Fw_with_batch_compilation.show_value_occurrences  (Private.parent cs) t ;;  
  let start_debugging cs = Fw_with_batch_compilation.start_debugging (Private.parent cs) ;; 
  let start_executing cs short_path = Fw_with_batch_compilation.start_executing (Private.parent cs) short_path;;  
  let to_concrete_object = Private.to_concrete_object ;;
  let up_to_date_elesses cs = 
    Fw_with_batch_compilation.up_to_date_elesses (Private.parent cs) ;; 
  let usual_compilable_files cs = 
      Fw_with_batch_compilation.usual_compilable_files (Private.parent cs) ;; 
  let usual_recompile = Private.usual_recompile ;;