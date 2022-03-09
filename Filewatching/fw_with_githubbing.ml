(* 

#use"Filewatching/fw_with_githubbing.ml";;

*)


module Private = struct

  let parent fw = fw.Fw_with_githubbing_t.parent ;; 
  
  let set_parent fw batch_compiler = { 
     fw with 
     Fw_with_githubbing_t.parent = batch_compiler ;
  } ;;
  
  let below fw mn = Fw_with_dependencies.below (parent fw) mn ;;
  let directly_below fw mn = Fw_with_dependencies.directly_below (parent fw) mn ;;
  let root fw = Fw_with_dependencies.root (parent fw) ;;
  
  let usual_batch fw modnames = 
    let (new_parent,rejected_ones,accepted_ones) = Fw_with_batch_compilation.usual_batch (parent fw) modnames in 
    (set_parent fw new_parent,rejected_ones,accepted_ones) ;; 
  
  let github_config fw = 
   Fw_poly.construct_github_configuration
   ~root:(root fw)
   ~dir_for_backup:fw.Fw_with_githubbing_t.dir_for_backup 
   ~gitpush_after_backup:fw.Fw_with_githubbing_t.gitpush_after_backup
   ~github_url:fw.Fw_with_githubbing_t.github_url
   ~encoding_protected_files:fw.Fw_with_githubbing_t.encoding_protected_files ;;


  let salt = "Coma_"^"state.";;
    
  let parent_label                         = salt ^ "parent";;
  let dir_for_backup_label                 = salt ^ "dir_for_backup";;
  let gitpush_after_backup_label           = salt ^ "gitpush_after_backup";;
  let github_url_label                     = salt ^ "github_url";;
  let encoding_protected_files_label       = salt ^ "encoding_protected_files";;
  
  
  let of_concrete_object ccrt_obj = 
     let g=Concrete_object.get_record ccrt_obj in
     {
        Fw_with_githubbing_t.parent = Fw_poly.of_concrete_object(g parent_label);
        dir_for_backup = Dfa_root.of_concrete_object(g dir_for_backup_label);
        gitpush_after_backup = Crobj_converter.bool_of_concrete_object (g gitpush_after_backup_label);
        github_url = Crobj_converter.string_of_concrete_object (g github_url_label);
        encoding_protected_files = Dfn_rootless.pair_list_of_concrete_object (g encoding_protected_files_label);
     };; 
  
  let to_concrete_object fw=
     let items= 
     [
      parent_label, Fw_poly.to_concrete_object (parent fw);
      dir_for_backup_label, Dfa_root.to_concrete_object fw.Fw_with_githubbing_t.dir_for_backup;
      gitpush_after_backup_label, Crobj_converter.bool_to_concrete_object  fw.Fw_with_githubbing_t.gitpush_after_backup;
      github_url_label, Crobj_converter.string_to_concrete_object fw.Fw_with_githubbing_t.github_url;
      encoding_protected_files_label, Dfn_rootless.pair_list_to_concrete_object fw.Fw_with_githubbing_t.encoding_protected_files;
     ]  in
     Concrete_object_t.Record items;;
  
  
  
  
    
    let empty_one config backup_dir gab git_url enc_files=
      {
        Fw_with_githubbing_t.parent = Fw_with_batch_compilation.plunge_fw_configuration config;
        dir_for_backup = backup_dir;
        gitpush_after_backup = gab;
        github_url = git_url;
        encoding_protected_files = enc_files;
  
      };;
    
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
    
  
  let read_persistent_version x=
    let full_path=Dfn_join.root_to_rootless (root x)  Coma_constant.rootless_path_for_targetfile in
    let ap= Dfn_full.to_absolute_path full_path in
    let the_archive=Io.read_whole_file ap in
    let archived_object = Crobj_parsing.parse the_archive in 
    of_concrete_object archived_object;;      
  
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
    
    let shrinkable_above fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.above fw_poly ;;   

   let shrinkable_all_endinglesses fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.all_endinglesses fw_poly ;;  

   let shrinkable_all_ml_absolute_paths fw =
       let fw_poly = fw.Fw_with_githubbing_t.parent in 
       Fw_with_dependencies.all_ml_absolute_paths fw_poly ;; 
       
   let shrinkable_all_mlx_files fw =
        let fw_poly = fw.Fw_with_githubbing_t.parent in 
        Fw_with_dependencies.all_mlx_files fw_poly ;;     

   let shrinkable_all_subdirectories fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.all_subdirectories fw_poly ;;  

   let shrinkable_ancestors_for_module fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.ancestors_for_module fw_poly ;;    

   let shrinkable_below fw =
       let fw_poly = fw.Fw_with_githubbing_t.parent in 
       Fw_with_dependencies.below fw_poly ;;  
       
   let shrinkable_directly_below fw =
         let fw_poly = fw.Fw_with_githubbing_t.parent in 
         Fw_with_dependencies.directly_below fw_poly ;;      

   let shrinkable_check_that_no_change_has_occurred fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_archives.check_that_no_change_has_occurred fw_poly ;; 

  let shrinkable_config fw =
    let fw_poly = fw.Fw_with_githubbing_t.parent in 
    {
      fw_poly with Fw_poly_t.type_name = "fw_configuration";
    } ;;

   let shrinkable_decipher_module fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.decipher_module fw_poly ;;  

   let shrinkable_decipher_path fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.decipher_path fw_poly ;;    

   let shrinkable_dep_ordered_modules fw =
       let fw_poly = fw.Fw_with_githubbing_t.parent in 
       Fw_with_dependencies.dep_ordered_modules fw_poly ;; 
       
    let shrinkable_direct_fathers_for_module fw =
        let fw_poly = fw.Fw_with_githubbing_t.parent in 
        Fw_with_dependencies.direct_fathers_for_module fw_poly ;;     

   let shrinkable_duplicate_module fw =
       let fw_poly = fw.Fw_with_githubbing_t.parent in 
       Fw_with_dependencies.duplicate_module fw_poly ;;  

   let shrinkable_endingless_at_module fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.endingless_at_module fw_poly ;;  

   let shrinkable_find_subdir_from_suffix fw =
       let fw_poly = fw.Fw_with_githubbing_t.parent in 
       Fw_with_dependencies.find_subdir_from_suffix fw_poly ;;  

    let shrinkable_latest_changes fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_archives.latest_changes fw_poly ;;  

   let shrinkable_modules_using_value fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_dependencies.modules_using_value fw_poly ;;    

   let shrinkable_noncompilable_files fw =
     let fw_poly = fw.Fw_with_githubbing_t.parent in 
     Fw_with_archives.noncompilable_files fw_poly ;;   

   let shrinkable_usual_compilable_files fw =
       let fw_poly = fw.Fw_with_githubbing_t.parent in 
       Fw_with_archives.usual_compilable_files fw_poly ;;     

end;;  
      
let above fw = Private.shrinkable_above fw ;;   
let all_endinglesses fw = Private.shrinkable_all_endinglesses fw ;;
let all_ml_absolute_paths fw = Private.shrinkable_all_ml_absolute_paths fw ;;
let all_mlx_files fw = Private.shrinkable_all_mlx_files fw ;;
let all_subdirectories fw = Private.shrinkable_all_subdirectories fw ;;
let below fw = Private.shrinkable_below fw ;; 
let directly_below fw = Private.shrinkable_directly_below fw ;; 
let ancestors_for_module fw mn = Private.shrinkable_ancestors_for_module fw mn ;;
let check_that_no_change_has_occurred = Private.shrinkable_check_that_no_change_has_occurred ;;
let clean_debug_dir fw = Fw_with_batch_compilation.clean_debug_dir (Private.parent fw) ;;
let clean_exec_dir fw = Fw_with_batch_compilation.clean_exec_dir (Private.parent fw) ;;
let configuration fw = Private.shrinkable_config fw ;;
let decipher_path fw = Private.shrinkable_decipher_path fw ;; 
let decipher_module fw = Private.shrinkable_decipher_module fw ;; 
let dep_ordered_modules fw = Private.shrinkable_dep_ordered_modules fw ;;  
let direct_fathers_for_module fw = Private.shrinkable_direct_fathers_for_module fw ;;
let duplicate_module fw = Private.shrinkable_duplicate_module fw ;; 
let empty_one = Private.empty_one ;;
let endingless_at_module = Private.shrinkable_endingless_at_module ;;   
let find_subdir_from_suffix fw = Private.shrinkable_find_subdir_from_suffix fw ;; 
let forget_modules = Private.forget_modules ;; 
let forget_nonmodular_rootlesses = Private.forget_nonmodular_rootlesses ;;  
let github_configuration = Private.github_config ;;
let gitpush_after_backup fw= fw.Fw_with_githubbing_t.gitpush_after_backup;;     
let latest_changes = Private.shrinkable_latest_changes ;;     
let list_values_from_module fw mn = 
Fw_with_batch_compilation.list_values_from_module  (Private.parent fw) mn ;;
let modern_recompile fw changed_modules_in_any_order = 
let new_parent = Fw_with_batch_compilation.modern_recompile (Private.parent fw) changed_modules_in_any_order in 
Private.set_parent fw new_parent ;; 
let modules_using_value = Private.shrinkable_modules_using_value ;;     
let noncompilable_files = Private.shrinkable_noncompilable_files ;;    
let number_of_modules fw = Fw_with_batch_compilation.number_of_modules (Private.parent fw) ;;    
let of_configuration config backup_dir gab git_url enc_files = 
{
 Fw_with_githubbing_t.parent = Fw_with_batch_compilation.of_configuration config;
 dir_for_backup = backup_dir;
 gitpush_after_backup = gab;
 github_url = git_url;
 encoding_protected_files = enc_files;
};;

let of_concrete_object = Private.of_concrete_object ;;  
let of_fw_with_batch_compilation batch_compiler backup_dir gab git_url enc_files = 
{
 Fw_with_githubbing_t.parent = batch_compiler ;
 dir_for_backup = backup_dir;
 gitpush_after_backup = gab;
 github_url = git_url;
 encoding_protected_files = enc_files;
};;

let preq_types_with_extra_info fw = 
Fw_with_batch_compilation.preq_types_with_extra_info (Private.parent fw) ;; 
let read_persistent_version = Private.read_persistent_version ;;  
let register_rootless_paths = Private.register_rootless_paths ;;      
let relocate_module_to  = Private.relocate_module_to ;;  
let remove_files fw rps = 
 let new_parent = Fw_with_batch_compilation.remove_files (Private.parent fw) rps in 
 Private.set_parent fw new_parent ;;        
let rename_module = Private.rename_module ;;   
let rename_subdirectory_as = Private.rename_subdirectory_as ;;     
let replace_string = Private.replace_string ;;  
let replace_value = Private.replace_value ;;   
let root = Private.root ;;
let set_gitpush_after_backup fw bowl = 
{
 fw with
 Fw_with_githubbing_t.gitpush_after_backup = bowl;
};;   
let show_value_occurrences fw t = 
Fw_with_batch_compilation.show_value_occurrences  (Private.parent fw) t ;;  
let start_debugging fw = Fw_with_batch_compilation.start_debugging (Private.parent fw) ;; 
let start_executing fw short_path = Fw_with_batch_compilation.start_executing (Private.parent fw) short_path;;  
let to_concrete_object = Private.to_concrete_object ;;
let up_to_date_elesses fw = 
Fw_with_batch_compilation.up_to_date_elesses (Private.parent fw) ;; 
let all_endinglesses fw = Private.shrinkable_all_endinglesses fw ;;  
let usual_compilable_files fw = Private.shrinkable_usual_compilable_files fw ;;  
let usual_recompile = Private.usual_recompile ;;