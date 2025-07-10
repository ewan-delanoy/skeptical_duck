(* 

#use"lib/Filewatching/fw_final_poly.ml";;

*)

exception Absent_method of string ;;

type t = 
  |Configuration of Fwc_configuration.t
  |Github_configuration of  Fwc_github_configuration.t
  |Watcher of Fwc_file_watcher.t
  |With_archives of Fwc_with_archives.t
  |With_dependencies of Fwc_with_dependencies.t
  |With_batch_compilation of Fwg_with_batch_compilation.t
  |With_githubbing of Fwg_with_githubbing.t
;;



let all_endinglesses final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.all_endinglesses" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.all_endinglesses" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.all_endinglesses" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.all_endinglesses" )
  |With_dependencies (fw) -> Fwc_with_dependencies.all_endinglesses fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.all_endinglesses fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.all_endinglesses fw ;;

let all_moduled_mlx_files final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.all_moduled_mlx_files" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.all_moduled_mlx_files" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.all_moduled_mlx_files" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.all_moduled_mlx_files" )
  |With_dependencies (fw) -> Fwc_with_dependencies.all_moduled_mlx_files fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.all_moduled_mlx_files fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.all_moduled_mlx_files fw ;;

let all_subdirectories final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.all_subdirectories" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.all_subdirectories" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.all_subdirectories" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.all_subdirectories" )
  |With_dependencies (fw) -> Fwc_with_dependencies.all_subdirectories fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.all_subdirectories fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.all_subdirectories fw ;;

let ancestors_for_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.ancestors_for_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.ancestors_for_module" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.ancestors_for_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.ancestors_for_module" )
  |With_dependencies (fw) -> Fwc_with_dependencies.ancestors_for_module fw mn
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.ancestors_for_module fw mn
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.ancestors_for_module fw mn ;;

let below final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.below" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.below" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.below" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.below" )
  |With_dependencies (fw) -> Fwc_with_dependencies.below fw mn
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.below fw mn
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.below fw mn ;;

let check_that_no_change_has_occurred final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.check_that_no_change_has_occurred" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.check_that_no_change_has_occurred" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.check_that_no_change_has_occurred" )
  |With_archives (fw) -> Fwc_with_archives.check_that_no_change_has_occurred fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.check_that_no_change_has_occurred fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.check_that_no_change_has_occurred fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw ;;

let clean_debug_dir final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.clean_debug_dir" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.clean_debug_dir" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.clean_debug_dir" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.clean_debug_dir" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.clean_debug_dir" )
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.clean_debug_dir fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.clean_debug_dir fw ;;

let clean_exec_dir final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.clean_exec_dir" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.clean_exec_dir" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.clean_exec_dir" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.clean_exec_dir" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.clean_exec_dir" )
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.clean_exec_dir fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.clean_exec_dir fw ;;

let decipher_module final_fw capitalized_or_not_x = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.decipher_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.decipher_module" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.decipher_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.decipher_module" )
  |With_dependencies (fw) -> Fwc_with_dependencies.decipher_module fw capitalized_or_not_x
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.decipher_module fw capitalized_or_not_x
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.decipher_module fw capitalized_or_not_x ;;

let decipher_path final_fw x = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.decipher_path" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.decipher_path" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.decipher_path" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.decipher_path" )
  |With_dependencies (fw) -> Fwc_with_dependencies.decipher_path fw x
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.decipher_path fw x
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.decipher_path fw x ;;

let dep_ordered_modules final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.dep_ordered_modules" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.dep_ordered_modules" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.dep_ordered_modules" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.dep_ordered_modules" )
  |With_dependencies (fw) -> Fwc_with_dependencies.dep_ordered_modules fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.dep_ordered_modules fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.dep_ordered_modules fw ;;

let directly_below final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.directly_below" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.directly_below" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.directly_below" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.directly_below" )
  |With_dependencies (fw) -> Fwc_with_dependencies.directly_below fw mn
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.directly_below fw mn
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.directly_below fw mn ;;

let direct_fathers_for_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.direct_fathers_for_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.direct_fathers_for_module" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.direct_fathers_for_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.direct_fathers_for_module" )
  |With_dependencies (fw) -> Fwc_with_dependencies.direct_fathers_for_module fw mn
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.direct_fathers_for_module fw mn
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.direct_fathers_for_module fw mn ;;

let duplicate_module final_fw old_t1 old_t2 = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.duplicate_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.duplicate_module" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.duplicate_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.duplicate_module" )
  |With_dependencies (fw) -> Fwc_with_dependencies.duplicate_module fw old_t1 old_t2
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.duplicate_module fw old_t1 old_t2
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.duplicate_module fw old_t1 old_t2 ;;

let endingless_at_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.endingless_at_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.endingless_at_module" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.endingless_at_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.endingless_at_module" )
  |With_dependencies (fw) -> Fwc_with_dependencies.endingless_at_module fw mn
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.endingless_at_module fw mn
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.endingless_at_module fw mn ;;

let find_subdir_from_suffix final_fw possibly_slashed_suffix = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.find_subdir_from_suffix" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.find_subdir_from_suffix" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.find_subdir_from_suffix" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.find_subdir_from_suffix" )
  |With_dependencies (fw) -> Fwc_with_dependencies.find_subdir_from_suffix fw possibly_slashed_suffix
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.find_subdir_from_suffix fw possibly_slashed_suffix
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.find_subdir_from_suffix fw possibly_slashed_suffix ;;

let forget_modules final_fw mods = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.forget_modules" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.forget_modules" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.forget_modules" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.forget_modules" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.forget_modules" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.forget_modules" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.forget_modules fw mods ) ;;

let forget_nonmodular_rootlesses final_fw rootlesses = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.forget_nonmodular_rootlesses" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.forget_nonmodular_rootlesses" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.forget_nonmodular_rootlesses" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.forget_nonmodular_rootlesses" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.forget_nonmodular_rootlesses" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.forget_nonmodular_rootlesses" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.forget_nonmodular_rootlesses fw rootlesses ) ;;


let github_configuration final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.github_configuration" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.github_configuration" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.github_configuration" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.github_configuration" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.github_configuration" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.github_configuration" )
  |With_githubbing (fw) ->  Fwg_with_githubbing.github_configuration fw  ;;



let gitpush_after_backup final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.gitpush_after_backup" )
  |Github_configuration (fw) -> Fwc_github_configuration.gitpush_after_backup fw
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.gitpush_after_backup" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.gitpush_after_backup" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.gitpush_after_backup" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.gitpush_after_backup" )
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.gitpush_after_backup fw
 ;;

let ignored_files final_fw  = match final_fw with 
  |Configuration (fw) -> Fwc_configuration.ignored_files fw
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.ignored_files" )
  |Watcher (fw) -> Fwc_file_watcher.Inherited.ignored_files fw
  |With_archives (fw) -> Fwc_with_archives.Inherited.ignored_files fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.ignored_files fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.ignored_files fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.ignored_files fw ;;

let ignored_subdirectories final_fw  = match final_fw with 
  |Configuration (fw) -> Fwc_configuration.ignored_subdirectories fw
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.ignored_subdirectories" )
  |Watcher (fw) -> Fwc_file_watcher.Inherited.ignored_subdirectories fw
  |With_archives (fw) -> Fwc_with_archives.Inherited.ignored_subdirectories fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.ignored_subdirectories fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.ignored_subdirectories fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.ignored_subdirectories fw ;;

let latest_changes final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.latest_changes" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.latest_changes" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.latest_changes" )
  |With_archives (fw) -> Fwc_with_archives.latest_changes fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.latest_changes fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.latest_changes fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.latest_changes fw ;;

let list_values_from_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.list_values_from_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.list_values_from_module" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.list_values_from_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.list_values_from_module" )
  |With_dependencies (fw) -> Fwc_with_dependencies.list_values_from_module fw mn
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.list_values_from_module fw mn
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.list_values_from_module fw mn ;;

let modern_recompile final_fw changed_mods = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.modern_recompile" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.modern_recompile" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.modern_recompile" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.modern_recompile" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.modern_recompile" )
  |With_batch_compilation (fw) -> With_batch_compilation( Fwc_with_batch_compilation.modern_recompile fw changed_mods )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.Inherited.modern_recompile fw changed_mods ) ;;


let modules_using_value final_fw value_name = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.modules_using_value" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.modules_using_value" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.modules_using_value" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.modules_using_value" )
  |With_dependencies (fw) -> Fwc_with_dependencies.modules_using_value fw value_name
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.modules_using_value fw value_name
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.modules_using_value fw value_name ;;

let noncompilable_files final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.noncompilable_files" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.noncompilable_files" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.noncompilable_files" )
  |With_archives (fw) -> Fwc_with_archives.noncompilable_files fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.noncompilable_files fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.noncompilable_files fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.noncompilable_files fw ;;

let number_of_modules final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.number_of_modules" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.number_of_modules" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.number_of_modules" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.number_of_modules" )
  |With_dependencies (fw) -> Fwc_with_dependencies.number_of_modules fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.number_of_modules fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.number_of_modules fw ;;

let of_concrete_object crobj = With_githubbing( Fwc_with_githubbing.of_concrete_object crobj ) ;;
let of_fw_config_and_github_config fw_config github_config = With_githubbing( Fwc_with_githubbing.of_fw_config_and_github_config fw_config github_config ) ;;
let of_fw_with_batch_compilation fw_batch backup_dir gab git_url enc_files = With_githubbing( Fwc_with_githubbing.of_fw_with_batch_compilation fw_batch backup_dir gab git_url enc_files ) ;;
let plunge_fw_config_with_github_config fw_config github_config = With_githubbing( Fwc_with_githubbing.plunge_fw_config_with_github_config fw_config github_config ) ;;
let preq_types_with_extra_info final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.preq_types_with_extra_info" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.preq_types_with_extra_info" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.preq_types_with_extra_info" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.preq_types_with_extra_info" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.preq_types_with_extra_info" )
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.preq_types_with_extra_info fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.preq_types_with_extra_info fw ;;

let register_rootless_paths final_fw rootlesses = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.register_rootless_paths" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.register_rootless_paths" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.register_rootless_paths" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.register_rootless_paths" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.register_rootless_paths" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.register_rootless_paths" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.register_rootless_paths fw rootlesses ) ;;

let relocate_module_to final_fw mn new_subdir = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.relocate_module_to" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.relocate_module_to" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.relocate_module_to" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.relocate_module_to" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.relocate_module_to" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.relocate_module_to" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.relocate_module_to fw mn new_subdir ) ;;

let rename_module final_fw old_middle_name new_nonslashed_name = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.rename_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.rename_module" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.rename_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.rename_module" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.rename_module" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.rename_module" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.rename_module fw old_middle_name new_nonslashed_name ) ;;

let rename_subdirectory_as final_fw (old_subdir,new_subdir) = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.rename_subdirectory_as" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.rename_subdirectory_as" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.rename_subdirectory_as" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.rename_subdirectory_as" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.rename_subdirectory_as" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.rename_subdirectory_as" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.rename_subdirectory_as fw (old_subdir,new_subdir) ) ;;

let replace_string final_fw old_s new_s = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.replace_string" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.replace_string" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.replace_string" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.replace_string" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.replace_string" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.replace_string" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.replace_string fw old_s new_s ) ;;

let replace_value final_fw pair_of_pairs = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.replace_value" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.replace_value" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.replace_value" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.replace_value" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.replace_value" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.replace_value" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.replace_value fw pair_of_pairs ) ;;

let root final_fw  = match final_fw with 
  |Github_configuration (fw) -> Fwc_github_configuration.root fw
  |Configuration (fw) -> Fwc_configuration.root fw
  |Watcher (fw) -> Fwc_file_watcher.Inherited.root fw
  |With_archives (fw) -> Fwc_with_archives.Inherited.root fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.root fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.root fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.root fw ;;

let set_gitpush_after_backup final_fw gab = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.set_gitpush_after_backup" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.set_gitpush_after_backup" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.set_gitpush_after_backup" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.set_gitpush_after_backup" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.set_gitpush_after_backup" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.set_gitpush_after_backup" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.Inherited.set_gitpush_after_backup fw gab ) ;;

let show_value_occurrences final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.show_value_occurrences" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.show_value_occurrences" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.show_value_occurrences" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.show_value_occurrences" )
  |With_dependencies (fw) -> Fwc_with_dependencies.show_value_occurrences fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.show_value_occurrences fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.show_value_occurrences fw ;;

let start_debugging final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.start_debugging" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.start_debugging" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.start_debugging" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.start_debugging" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.start_debugging" )
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.start_debugging fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.start_debugging fw ;;

let start_executing final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.start_executing" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.start_executing" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.start_executing" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.start_executing" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.start_executing" )
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.start_executing fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.start_executing fw ;;


let test_for_admissibility final_fw rl = match final_fw with 
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.test_for_admissibility" )
  |Configuration (fw) -> Fwc_configuration.test_for_admissibility fw rl
  |Watcher (fw) -> Fwc_file_watcher.Inherited.test_for_admissibility fw rl
  |With_archives (fw) -> Fwc_with_archives.Inherited.test_for_admissibility fw rl
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.test_for_admissibility fw rl
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.test_for_admissibility fw rl
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.test_for_admissibility fw rl ;;


let to_concrete_object final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.to_concrete_object" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.to_concrete_object" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.to_concrete_object" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.to_concrete_object" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.to_concrete_object" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.to_concrete_object" )
  |With_githubbing (fw) ->  Fwc_with_githubbing.to_concrete_object fw  ;;

let to_fw_configuration final_fw  = match final_fw with 
  |Configuration (fw)-> fw
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.to_fw_configuration" )
  |Watcher (fw) -> Fwc_file_watcher.Inherited.to_fw_configuration fw
  |With_archives (fw) -> Fwc_with_archives.Inherited.to_fw_configuration fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.to_fw_configuration fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.to_fw_configuration fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.to_fw_configuration fw  ;;



let up_to_date_elesses final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.up_to_date_elesses" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.up_to_date_elesses" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.up_to_date_elesses" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.up_to_date_elesses" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.up_to_date_elesses" )
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.up_to_date_elesses fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.up_to_date_elesses fw ;;

let usual_compilable_files final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.usual_compilable_files" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.usual_compilable_files" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.usual_compilable_files" )
  |With_archives (fw) -> Fwc_with_archives.usual_compilable_files fw
  |With_dependencies (fw) -> Fwc_with_dependencies.Inherited.usual_compilable_files fw
  |With_batch_compilation (fw) -> Fwc_with_batch_compilation.Inherited.usual_compilable_files fw
  |With_githubbing (fw) -> Fwc_with_githubbing.Inherited.usual_compilable_files fw ;;

let usual_recompile final_fw opt_comment = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.usual_recompile" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.usual_recompile" )
  |Watcher _ -> raise ( Absent_method "Fw_file_watcher.usual_recompile" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.usual_recompile" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.usual_recompile" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.usual_recompile" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.usual_recompile fw opt_comment ) ;;

  