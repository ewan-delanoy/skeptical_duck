(* 

#use"lib/Filewatching/fw_final_poly.ml";;

*)

type t = Final of Fw_flattened_poly_t.t;;

module Lfw_configuration = struct 

    let root (Final fw) = Fw_configuration.root fw ;; 

    let test_for_admissibility  (Final fw) rl = Fw_configuration.test_for_admissibility fw rl ;; 
      
end ;;   

module Lfw_file_watcher = struct 

    let ignored_files (Final fw) = Fw_poly.ignored_files fw ;; 

    let ignored_subdirectories (Final fw) = Fw_poly.ignored_subdirectories fw ;;  
      
end ;;   



module Lfw_with_archives = struct 

  let check_that_no_change_has_occurred   (Final fw)=
    Fw_with_archives.check_that_no_change_has_occurred fw ;; 

  let latest_changes (Final fw)=  Fw_with_archives.latest_changes fw ;; 

  let noncompilable_files   (Final fw)=
    Fw_with_archives.noncompilable_files fw ;;   

  let usual_compilable_files   (Final fw)=
    Fw_with_archives.usual_compilable_files fw ;;     
    
end ;;    
    
module Lfw_with_dependencies = struct 

  let ancestors_for_module (Final fw) mn=
    Fw_with_dependencies.ancestors_for_module fw mn ;; 

  let all_endinglesses  (Final fw)=
    Fw_with_dependencies.all_endinglesses fw ;; 

  let all_moduled_mlx_files  (Final fw)=
    Fw_with_dependencies.all_moduled_mlx_files fw ;;  
    
  let all_subdirectories  (Final fw)=
    Fw_with_dependencies.all_subdirectories fw ;; 

  let below (Final fw) mn = 
        Fw_with_dependencies.below fw mn ;;  

  let decipher_module   (Final fw) capitalized_or_not_x=
    Fw_with_dependencies.decipher_module fw capitalized_or_not_x ;; 

  let decipher_path   (Final fw) x=
    Fw_with_dependencies.decipher_path fw x ;;   

  let dep_ordered_modules   (Final fw)=
    Fw_with_dependencies.dep_ordered_modules fw ;; 

  let direct_fathers_for_module (Final fw) mn = 
        Fw_with_dependencies.direct_fathers_for_module fw mn ;;   

  let directly_below (Final fw) mn = 
      Fw_with_dependencies.directly_below fw mn ;;  

  let duplicate_module (Final fw) old_t1 old_t2 = 
      Fw_with_dependencies.duplicate_module fw old_t1 old_t2 ;;      

  let endingless_at_module (Final fw) mn = 
      Fw_with_dependencies.endingless_at_module fw mn ;;

  let find_subdir_from_suffix (Final fw) possibly_slashed_suffix = 
        Fw_with_dependencies.find_subdir_from_suffix fw possibly_slashed_suffix;;

  let list_values_from_module (Final fw) module_name=
        Fw_with_dependencies.list_values_from_module fw module_name;; 
  let modules_using_value (Final fw) value_name=
        Fw_with_dependencies.modules_using_value fw value_name;; 
  let number_of_modules (Final fw)=
   Fw_with_dependencies.number_of_modules fw ;; 
        
end ;;    
    
module Lfw_with_batch_compilation = struct 
    
    let clean_debug_dir (Final fw)=
      Fw_with_batch_compilation.clean_debug_dir fw ;; 
    let clean_exec_dir (Final fw)=
      Fw_with_batch_compilation.clean_exec_dir fw ;;   
    let modern_recompile (Final fw) changed_modules_in_any_order=
      Final(Fw_with_batch_compilation.modern_recompile fw changed_modules_in_any_order) ;; 
    let preq_types_with_extra_info (Final fw)=
      Fw_with_batch_compilation.preq_types_with_extra_info fw ;; 
    let show_value_occurrences (Final fw) mn =
        Fw_with_batch_compilation.show_value_occurrences fw mn;;  
    let start_debugging (Final fw) =
      Fw_with_batch_compilation.start_debugging fw ;;     
    let start_executing (Final fw) short_path=
      Fw_with_batch_compilation.start_executing fw short_path;;     

    let up_to_date_elesses (Final fw)=
      Fw_with_batch_compilation.up_to_date_elesses fw ;; 
          
end ;; 


module Lfwc_with_githubbing = struct 

let forget_modules (Final fw) mods=
    Final(Fwc_with_githubbing.forget_modules fw mods) ;;
let forget_nonmodular_rootlesses (Final fw) rls=
    Final(Fwc_with_githubbing.forget_nonmodular_rootlesses fw rls) ;;  
let gitpush_after_backup (Final fw) =
    Fw_poly.gitpush_after_backup fw ;;  
let of_concrete_object cr = Final(Fwc_with_githubbing.of_concrete_object cr) ;;    
let of_fw_config_and_github_config fw_config github_config=
    Final(Fwc_with_githubbing.of_fw_config_and_github_config 
    fw_config github_config) ;;        
let of_fw_with_batch_compilation fw_batch backup_dir gab git_url enc_files=
    Final(Fwc_with_githubbing.of_fw_with_batch_compilation 
    fw_batch backup_dir gab git_url enc_files) ;;  
let plunge_fw_config_with_github_config fw_config github_config=
    Final(Fwc_with_githubbing.plunge_fw_config_with_github_config 
    fw_config github_config) ;;
let register_rootless_paths (Final fw) rls=
    Final(Fwc_with_githubbing.register_rootless_paths fw rls) ;;
let relocate_module_to (Final fw) mod_name new_subdir=
    Final(Fwc_with_githubbing.relocate_module_to 
    fw mod_name new_subdir) ;;
let rename_module (Final fw) old_middle_name new_nonslashed_name=
    Final(Fwc_with_githubbing.rename_module 
    fw old_middle_name new_nonslashed_name) ;;

let rename_subdirectory_as (Final fw) (old_subdir,new_subdir)=
    Final(Fwc_with_githubbing.rename_subdirectory_as 
    fw (old_subdir,new_subdir)) ;;

let replace_string (Final fw) old_s new_s=
    Final(Fwc_with_githubbing.replace_string 
    fw old_s new_s) ;;

let replace_value (Final fw) ((preceding_files,path),(old_v,new_v))=
    Final(Fwc_with_githubbing.replace_value 
    fw ((preceding_files,path),(old_v,new_v)) ) ;;

let set_gitpush_after_backup (Final fw) gab = 
      Final(Fwc_with_githubbing.set_gitpush_after_backup fw gab) ;; 

let to_concrete_object (Final fw) = Fwc_with_githubbing.to_concrete_object fw ;;

let to_fw_configuration (Final fw) = Fwc_with_githubbing.to_fw_configuration fw ;;

let to_github_configuration (Final fw) = Fwc_with_githubbing.to_github_configuration fw ;;

let usual_recompile (Final fw) opt_comment=
    Final(Fwc_with_githubbing.usual_recompile fw opt_comment) ;;

end ;;  

