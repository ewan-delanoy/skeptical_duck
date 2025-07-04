(* 

#use"lib/Filewatching/fw_final_poly.ml";;

*)

exception Absent_method of string ;;

type t = 
  |Configuration of Fw_flattened_poly_t.t
  |Github_configuration of Fw_flattened_poly_t.t
  |Watcher of Fw_flattened_poly_t.t
  |With_archives of Fw_flattened_poly_t.t
  |With_dependencies of Fw_flattened_poly_t.t
  |With_batch_compilation of Fw_flattened_poly_t.t
  |With_githubbing of Fwg_with_githubbing.t
;;


module Private = struct 

let  all_endinglesses_on_fw_with_batch_compilation fw = Fw_with_dependencies.all_endinglesses(Fw_poly.parent fw)  ;;
let  all_endinglesses_on_fw_with_githubbing fw = all_endinglesses_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  all_moduled_mlx_files_on_fw_with_batch_compilation fw = Fw_with_dependencies.all_moduled_mlx_files(Fw_poly.parent fw)  ;;
let  all_moduled_mlx_files_on_fw_with_githubbing fw = all_moduled_mlx_files_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  all_subdirectories_on_fw_with_batch_compilation fw = Fw_with_dependencies.all_subdirectories(Fw_poly.parent fw)  ;;
let  all_subdirectories_on_fw_with_githubbing fw = all_subdirectories_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  ancestors_for_module_on_fw_with_batch_compilation fw mn = Fw_with_dependencies.ancestors_for_module(Fw_poly.parent fw) mn ;;
let  ancestors_for_module_on_fw_with_githubbing fw mn = ancestors_for_module_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) mn ;;

let  below_on_fw_with_batch_compilation fw mn = Fw_with_dependencies.below(Fw_poly.parent fw) mn ;;
let  below_on_fw_with_githubbing fw mn = below_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) mn ;;

let  check_that_no_change_has_occurred_on_fw_with_dependencies fw = Fw_with_archives.check_that_no_change_has_occurred(Fw_poly.parent fw)  ;;
let  check_that_no_change_has_occurred_on_fw_with_batch_compilation fw = check_that_no_change_has_occurred_on_fw_with_dependencies(Fw_poly.parent fw)  ;;
let  check_that_no_change_has_occurred_on_fw_with_githubbing fw = check_that_no_change_has_occurred_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  clean_debug_dir_on_fw_with_githubbing fw = Fw_with_batch_compilation.clean_debug_dir(Fwg_with_githubbing.parent fw)  ;;

let  clean_exec_dir_on_fw_with_githubbing fw = Fw_with_batch_compilation.clean_exec_dir(Fwg_with_githubbing.parent fw)  ;;

let  decipher_module_on_fw_with_batch_compilation fw capitalized_or_not_x = Fw_with_dependencies.decipher_module(Fw_poly.parent fw) capitalized_or_not_x ;;
let  decipher_module_on_fw_with_githubbing fw capitalized_or_not_x = decipher_module_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) capitalized_or_not_x ;;

let  decipher_path_on_fw_with_batch_compilation fw x = Fw_with_dependencies.decipher_path(Fw_poly.parent fw) x ;;
let  decipher_path_on_fw_with_githubbing fw x = decipher_path_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) x ;;

let  dep_ordered_modules_on_fw_with_batch_compilation fw = Fw_with_dependencies.dep_ordered_modules(Fw_poly.parent fw)  ;;
let  dep_ordered_modules_on_fw_with_githubbing fw = dep_ordered_modules_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  directly_below_on_fw_with_batch_compilation fw mn = Fw_with_dependencies.directly_below(Fw_poly.parent fw) mn ;;
let  directly_below_on_fw_with_githubbing fw mn = directly_below_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) mn ;;

let  direct_fathers_for_module_on_fw_with_batch_compilation fw mn = Fw_with_dependencies.direct_fathers_for_module(Fw_poly.parent fw) mn ;;
let  direct_fathers_for_module_on_fw_with_githubbing fw mn = direct_fathers_for_module_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) mn ;;

let  duplicate_module_on_fw_with_batch_compilation fw old_t1 old_t2 = Fw_with_dependencies.duplicate_module(Fw_poly.parent fw) old_t1 old_t2 ;;
let  duplicate_module_on_fw_with_githubbing fw old_t1 old_t2 = duplicate_module_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) old_t1 old_t2 ;;

let  endingless_at_module_on_fw_with_batch_compilation fw mn = Fw_with_dependencies.endingless_at_module(Fw_poly.parent fw) mn ;;
let  endingless_at_module_on_fw_with_githubbing fw mn = endingless_at_module_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) mn ;;

let  find_subdir_from_suffix_on_fw_with_batch_compilation fw possibly_slashed_suffix = Fw_with_dependencies.find_subdir_from_suffix(Fw_poly.parent fw) possibly_slashed_suffix ;;
let  find_subdir_from_suffix_on_fw_with_githubbing fw possibly_slashed_suffix = find_subdir_from_suffix_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) possibly_slashed_suffix ;;







let  ignored_files_on_fw_with_archives fw = Fw_poly.ignored_files(Fw_poly.parent fw)  ;;
let  ignored_files_on_fw_with_dependencies fw = ignored_files_on_fw_with_archives(Fw_poly.parent fw)  ;;
let  ignored_files_on_fw_with_batch_compilation fw = ignored_files_on_fw_with_dependencies(Fw_poly.parent fw)  ;;
let  ignored_files_on_fw_with_githubbing fw = ignored_files_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  ignored_subdirectories_on_fw_with_archives fw = Fw_poly.ignored_subdirectories(Fw_poly.parent fw)  ;;
let  ignored_subdirectories_on_fw_with_dependencies fw = ignored_subdirectories_on_fw_with_archives(Fw_poly.parent fw)  ;;
let  ignored_subdirectories_on_fw_with_batch_compilation fw = ignored_subdirectories_on_fw_with_dependencies(Fw_poly.parent fw)  ;;
let  ignored_subdirectories_on_fw_with_githubbing fw = ignored_subdirectories_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  latest_changes_on_fw_with_dependencies fw = Fw_with_archives.latest_changes(Fw_poly.parent fw)  ;;
let  latest_changes_on_fw_with_batch_compilation fw = latest_changes_on_fw_with_dependencies(Fw_poly.parent fw)  ;;
let  latest_changes_on_fw_with_githubbing fw = latest_changes_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  list_values_from_module_on_fw_with_batch_compilation fw mn = Fw_with_dependencies.list_values_from_module(Fw_poly.parent fw) mn ;;
let  list_values_from_module_on_fw_with_githubbing fw mn = list_values_from_module_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) mn ;;

let  modern_recompile_on_fw_with_githubbing fw changed_mods = Fwc_with_githubbing.Field.set_parent fw (Fw_with_batch_compilation.modern_recompile(Fwg_with_githubbing.parent fw) changed_mods ) ;;

let  modules_using_value_on_fw_with_batch_compilation fw value_name = Fw_with_dependencies.modules_using_value(Fw_poly.parent fw) value_name ;;
let  modules_using_value_on_fw_with_githubbing fw value_name = modules_using_value_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) value_name ;;

let  noncompilable_files_on_fw_with_dependencies fw = Fw_with_archives.noncompilable_files(Fw_poly.parent fw)  ;;
let  noncompilable_files_on_fw_with_batch_compilation fw = noncompilable_files_on_fw_with_dependencies(Fw_poly.parent fw)  ;;
let  noncompilable_files_on_fw_with_githubbing fw = noncompilable_files_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;

let  number_of_modules_on_fw_with_batch_compilation fw = Fw_with_dependencies.number_of_modules(Fw_poly.parent fw)  ;;
let  number_of_modules_on_fw_with_githubbing fw = number_of_modules_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;









let  preq_types_with_extra_info_on_fw_with_githubbing fw = Fw_with_batch_compilation.preq_types_with_extra_info(Fwg_with_githubbing.parent fw)  ;;













let  root_on_file_watcher fw = Fw_configuration.root(Fw_poly.parent fw)  ;;
let  root_on_fw_with_archives fw = root_on_file_watcher(Fw_poly.parent fw)  ;;
let  root_on_fw_with_dependencies fw = root_on_fw_with_archives(Fw_poly.parent fw)  ;;
let  root_on_fw_with_batch_compilation fw = root_on_fw_with_dependencies(Fw_poly.parent fw)  ;;
let  root_on_fw_with_githubbing fw = root_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;



let  show_value_occurrences_on_fw_with_githubbing fw = Fw_with_batch_compilation.show_value_occurrences(Fwg_with_githubbing.parent fw)  ;;

let  start_debugging_on_fw_with_githubbing fw = Fw_with_batch_compilation.start_debugging(Fwg_with_githubbing.parent fw)  ;;

let  start_executing_on_fw_with_githubbing fw = Fw_with_batch_compilation.start_executing(Fwg_with_githubbing.parent fw)  ;;

let  test_for_admissibility_on_file_watcher fw rl = Fw_configuration.test_for_admissibility(Fw_poly.parent fw) rl ;;
let  test_for_admissibility_on_fw_with_archives fw rl = test_for_admissibility_on_file_watcher(Fw_poly.parent fw) rl ;;
let  test_for_admissibility_on_fw_with_dependencies fw rl = test_for_admissibility_on_fw_with_archives(Fw_poly.parent fw) rl ;;
let  test_for_admissibility_on_fw_with_batch_compilation fw rl = test_for_admissibility_on_fw_with_dependencies(Fw_poly.parent fw) rl ;;
let  test_for_admissibility_on_fw_with_githubbing fw rl = test_for_admissibility_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw) rl ;;







let  up_to_date_elesses_on_fw_with_githubbing fw = Fw_with_batch_compilation.up_to_date_elesses(Fwg_with_githubbing.parent fw)  ;;

let  usual_compilable_files_on_fw_with_dependencies fw = Fw_with_archives.usual_compilable_files(Fw_poly.parent fw)  ;;
let  usual_compilable_files_on_fw_with_batch_compilation fw = usual_compilable_files_on_fw_with_dependencies(Fw_poly.parent fw)  ;;
let  usual_compilable_files_on_fw_with_githubbing fw = usual_compilable_files_on_fw_with_batch_compilation(Fwg_with_githubbing.parent fw)  ;;



end ;;


let all_endinglesses final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.all_endinglesses" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.all_endinglesses" )
  |Watcher _ -> raise ( Absent_method "File_watcher.all_endinglesses" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.all_endinglesses" )
  |With_dependencies (fw) -> Fw_with_dependencies.all_endinglesses fw
  |With_batch_compilation (fw) -> Private.all_endinglesses_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.all_endinglesses_on_fw_with_githubbing fw ;;

let all_moduled_mlx_files final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.all_moduled_mlx_files" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.all_moduled_mlx_files" )
  |Watcher _ -> raise ( Absent_method "File_watcher.all_moduled_mlx_files" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.all_moduled_mlx_files" )
  |With_dependencies (fw) -> Fw_with_dependencies.all_moduled_mlx_files fw
  |With_batch_compilation (fw) -> Private.all_moduled_mlx_files_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.all_moduled_mlx_files_on_fw_with_githubbing fw ;;

let all_subdirectories final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.all_subdirectories" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.all_subdirectories" )
  |Watcher _ -> raise ( Absent_method "File_watcher.all_subdirectories" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.all_subdirectories" )
  |With_dependencies (fw) -> Fw_with_dependencies.all_subdirectories fw
  |With_batch_compilation (fw) -> Private.all_subdirectories_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.all_subdirectories_on_fw_with_githubbing fw ;;

let ancestors_for_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.ancestors_for_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.ancestors_for_module" )
  |Watcher _ -> raise ( Absent_method "File_watcher.ancestors_for_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.ancestors_for_module" )
  |With_dependencies (fw) -> Fw_with_dependencies.ancestors_for_module fw mn
  |With_batch_compilation (fw) -> Private.ancestors_for_module_on_fw_with_batch_compilation fw mn
  |With_githubbing (fw) -> Private.ancestors_for_module_on_fw_with_githubbing fw mn ;;

let below final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.below" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.below" )
  |Watcher _ -> raise ( Absent_method "File_watcher.below" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.below" )
  |With_dependencies (fw) -> Fw_with_dependencies.below fw mn
  |With_batch_compilation (fw) -> Private.below_on_fw_with_batch_compilation fw mn
  |With_githubbing (fw) -> Private.below_on_fw_with_githubbing fw mn ;;

let check_that_no_change_has_occurred final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.check_that_no_change_has_occurred" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.check_that_no_change_has_occurred" )
  |Watcher _ -> raise ( Absent_method "File_watcher.check_that_no_change_has_occurred" )
  |With_archives (fw) -> Fw_with_archives.check_that_no_change_has_occurred fw
  |With_dependencies (fw) -> Private.check_that_no_change_has_occurred_on_fw_with_dependencies fw
  |With_batch_compilation (fw) -> Private.check_that_no_change_has_occurred_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.check_that_no_change_has_occurred_on_fw_with_githubbing fw ;;

let clean_debug_dir final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.clean_debug_dir" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.clean_debug_dir" )
  |Watcher _ -> raise ( Absent_method "File_watcher.clean_debug_dir" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.clean_debug_dir" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.clean_debug_dir" )
  |With_batch_compilation (fw) -> Fw_with_batch_compilation.clean_debug_dir fw
  |With_githubbing (fw) -> Private.clean_debug_dir_on_fw_with_githubbing fw ;;

let clean_exec_dir final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.clean_exec_dir" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.clean_exec_dir" )
  |Watcher _ -> raise ( Absent_method "File_watcher.clean_exec_dir" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.clean_exec_dir" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.clean_exec_dir" )
  |With_batch_compilation (fw) -> Fw_with_batch_compilation.clean_exec_dir fw
  |With_githubbing (fw) -> Private.clean_exec_dir_on_fw_with_githubbing fw ;;

let decipher_module final_fw capitalized_or_not_x = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.decipher_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.decipher_module" )
  |Watcher _ -> raise ( Absent_method "File_watcher.decipher_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.decipher_module" )
  |With_dependencies (fw) -> Fw_with_dependencies.decipher_module fw capitalized_or_not_x
  |With_batch_compilation (fw) -> Private.decipher_module_on_fw_with_batch_compilation fw capitalized_or_not_x
  |With_githubbing (fw) -> Private.decipher_module_on_fw_with_githubbing fw capitalized_or_not_x ;;

let decipher_path final_fw x = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.decipher_path" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.decipher_path" )
  |Watcher _ -> raise ( Absent_method "File_watcher.decipher_path" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.decipher_path" )
  |With_dependencies (fw) -> Fw_with_dependencies.decipher_path fw x
  |With_batch_compilation (fw) -> Private.decipher_path_on_fw_with_batch_compilation fw x
  |With_githubbing (fw) -> Private.decipher_path_on_fw_with_githubbing fw x ;;

let dep_ordered_modules final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.dep_ordered_modules" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.dep_ordered_modules" )
  |Watcher _ -> raise ( Absent_method "File_watcher.dep_ordered_modules" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.dep_ordered_modules" )
  |With_dependencies (fw) -> Fw_with_dependencies.dep_ordered_modules fw
  |With_batch_compilation (fw) -> Private.dep_ordered_modules_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.dep_ordered_modules_on_fw_with_githubbing fw ;;

let directly_below final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.directly_below" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.directly_below" )
  |Watcher _ -> raise ( Absent_method "File_watcher.directly_below" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.directly_below" )
  |With_dependencies (fw) -> Fw_with_dependencies.directly_below fw mn
  |With_batch_compilation (fw) -> Private.directly_below_on_fw_with_batch_compilation fw mn
  |With_githubbing (fw) -> Private.directly_below_on_fw_with_githubbing fw mn ;;

let direct_fathers_for_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.direct_fathers_for_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.direct_fathers_for_module" )
  |Watcher _ -> raise ( Absent_method "File_watcher.direct_fathers_for_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.direct_fathers_for_module" )
  |With_dependencies (fw) -> Fw_with_dependencies.direct_fathers_for_module fw mn
  |With_batch_compilation (fw) -> Private.direct_fathers_for_module_on_fw_with_batch_compilation fw mn
  |With_githubbing (fw) -> Private.direct_fathers_for_module_on_fw_with_githubbing fw mn ;;

let duplicate_module final_fw old_t1 old_t2 = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.duplicate_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.duplicate_module" )
  |Watcher _ -> raise ( Absent_method "File_watcher.duplicate_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.duplicate_module" )
  |With_dependencies (fw) -> Fw_with_dependencies.duplicate_module fw old_t1 old_t2
  |With_batch_compilation (fw) -> Private.duplicate_module_on_fw_with_batch_compilation fw old_t1 old_t2
  |With_githubbing (fw) -> Private.duplicate_module_on_fw_with_githubbing fw old_t1 old_t2 ;;

let endingless_at_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.endingless_at_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.endingless_at_module" )
  |Watcher _ -> raise ( Absent_method "File_watcher.endingless_at_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.endingless_at_module" )
  |With_dependencies (fw) -> Fw_with_dependencies.endingless_at_module fw mn
  |With_batch_compilation (fw) -> Private.endingless_at_module_on_fw_with_batch_compilation fw mn
  |With_githubbing (fw) -> Private.endingless_at_module_on_fw_with_githubbing fw mn ;;

let find_subdir_from_suffix final_fw possibly_slashed_suffix = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.find_subdir_from_suffix" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.find_subdir_from_suffix" )
  |Watcher _ -> raise ( Absent_method "File_watcher.find_subdir_from_suffix" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.find_subdir_from_suffix" )
  |With_dependencies (fw) -> Fw_with_dependencies.find_subdir_from_suffix fw possibly_slashed_suffix
  |With_batch_compilation (fw) -> Private.find_subdir_from_suffix_on_fw_with_batch_compilation fw possibly_slashed_suffix
  |With_githubbing (fw) -> Private.find_subdir_from_suffix_on_fw_with_githubbing fw possibly_slashed_suffix ;;

let forget_modules final_fw mods = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.forget_modules" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.forget_modules" )
  |Watcher _ -> raise ( Absent_method "File_watcher.forget_modules" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.forget_modules" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.forget_modules" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.forget_modules" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.forget_modules fw mods ) ;;

let forget_nonmodular_rootlesses final_fw rootlesses = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.forget_nonmodular_rootlesses" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.forget_nonmodular_rootlesses" )
  |Watcher _ -> raise ( Absent_method "File_watcher.forget_nonmodular_rootlesses" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.forget_nonmodular_rootlesses" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.forget_nonmodular_rootlesses" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.forget_nonmodular_rootlesses" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.forget_nonmodular_rootlesses fw rootlesses ) ;;

let gitpush_after_backup final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.gitpush_after_backup" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.gitpush_after_backup" )
  |Watcher _ -> raise ( Absent_method "File_watcher.gitpush_after_backup" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.gitpush_after_backup" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.gitpush_after_backup" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.gitpush_after_backup" )
  |With_githubbing _ -> raise ( Absent_method "Fwc_with_githubbing.gitpush_after_backup" )
 ;;

let ignored_files final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.ignored_files" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.ignored_files" )
  |Watcher (fw) -> Fw_poly.ignored_files fw
  |With_archives (fw) -> Private.ignored_files_on_fw_with_archives fw
  |With_dependencies (fw) -> Private.ignored_files_on_fw_with_dependencies fw
  |With_batch_compilation (fw) -> Private.ignored_files_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.ignored_files_on_fw_with_githubbing fw ;;

let ignored_subdirectories final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.ignored_subdirectories" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.ignored_subdirectories" )
  |Watcher (fw) -> Fw_poly.ignored_subdirectories fw
  |With_archives (fw) -> Private.ignored_subdirectories_on_fw_with_archives fw
  |With_dependencies (fw) -> Private.ignored_subdirectories_on_fw_with_dependencies fw
  |With_batch_compilation (fw) -> Private.ignored_subdirectories_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.ignored_subdirectories_on_fw_with_githubbing fw ;;

let latest_changes final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.latest_changes" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.latest_changes" )
  |Watcher _ -> raise ( Absent_method "File_watcher.latest_changes" )
  |With_archives (fw) -> Fw_with_archives.latest_changes fw
  |With_dependencies (fw) -> Private.latest_changes_on_fw_with_dependencies fw
  |With_batch_compilation (fw) -> Private.latest_changes_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.latest_changes_on_fw_with_githubbing fw ;;

let list_values_from_module final_fw mn = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.list_values_from_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.list_values_from_module" )
  |Watcher _ -> raise ( Absent_method "File_watcher.list_values_from_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.list_values_from_module" )
  |With_dependencies (fw) -> Fw_with_dependencies.list_values_from_module fw mn
  |With_batch_compilation (fw) -> Private.list_values_from_module_on_fw_with_batch_compilation fw mn
  |With_githubbing (fw) -> Private.list_values_from_module_on_fw_with_githubbing fw mn ;;

let modern_recompile final_fw changed_mods = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.modern_recompile" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.modern_recompile" )
  |Watcher _ -> raise ( Absent_method "File_watcher.modern_recompile" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.modern_recompile" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.modern_recompile" )
  |With_batch_compilation (fw) -> With_batch_compilation( Fw_with_batch_compilation.modern_recompile fw changed_mods )
  |With_githubbing (fw) -> With_githubbing( Private.modern_recompile_on_fw_with_githubbing fw changed_mods ) ;;

let modules_using_value final_fw value_name = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.modules_using_value" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.modules_using_value" )
  |Watcher _ -> raise ( Absent_method "File_watcher.modules_using_value" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.modules_using_value" )
  |With_dependencies (fw) -> Fw_with_dependencies.modules_using_value fw value_name
  |With_batch_compilation (fw) -> Private.modules_using_value_on_fw_with_batch_compilation fw value_name
  |With_githubbing (fw) -> Private.modules_using_value_on_fw_with_githubbing fw value_name ;;

let noncompilable_files final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.noncompilable_files" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.noncompilable_files" )
  |Watcher _ -> raise ( Absent_method "File_watcher.noncompilable_files" )
  |With_archives (fw) -> Fw_with_archives.noncompilable_files fw
  |With_dependencies (fw) -> Private.noncompilable_files_on_fw_with_dependencies fw
  |With_batch_compilation (fw) -> Private.noncompilable_files_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.noncompilable_files_on_fw_with_githubbing fw ;;

let number_of_modules final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.number_of_modules" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.number_of_modules" )
  |Watcher _ -> raise ( Absent_method "File_watcher.number_of_modules" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.number_of_modules" )
  |With_dependencies (fw) -> Fw_with_dependencies.number_of_modules fw
  |With_batch_compilation (fw) -> Private.number_of_modules_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.number_of_modules_on_fw_with_githubbing fw ;;

let of_concrete_object crobj = With_githubbing( Fwc_with_githubbing.of_concrete_object crobj ) ;;
let of_fw_config_and_github_config fw_config github_config = With_githubbing( Fwc_with_githubbing.of_fw_config_and_github_config fw_config github_config ) ;;
let of_fw_with_batch_compilation fw_batch backup_dir gab git_url enc_files = With_githubbing( Fwc_with_githubbing.of_fw_with_batch_compilation fw_batch backup_dir gab git_url enc_files ) ;;
let plunge_fw_config_with_github_config fw_config github_config = With_githubbing( Fwc_with_githubbing.plunge_fw_config_with_github_config fw_config github_config ) ;;
let preq_types_with_extra_info final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.preq_types_with_extra_info" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.preq_types_with_extra_info" )
  |Watcher _ -> raise ( Absent_method "File_watcher.preq_types_with_extra_info" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.preq_types_with_extra_info" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.preq_types_with_extra_info" )
  |With_batch_compilation (fw) -> Fw_with_batch_compilation.preq_types_with_extra_info fw
  |With_githubbing (fw) -> Private.preq_types_with_extra_info_on_fw_with_githubbing fw ;;

let register_rootless_paths final_fw rootlesses = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.register_rootless_paths" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.register_rootless_paths" )
  |Watcher _ -> raise ( Absent_method "File_watcher.register_rootless_paths" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.register_rootless_paths" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.register_rootless_paths" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.register_rootless_paths" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.register_rootless_paths fw rootlesses ) ;;

let relocate_module_to final_fw mn new_subdir = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.relocate_module_to" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.relocate_module_to" )
  |Watcher _ -> raise ( Absent_method "File_watcher.relocate_module_to" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.relocate_module_to" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.relocate_module_to" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.relocate_module_to" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.relocate_module_to fw mn new_subdir ) ;;

let rename_module final_fw old_middle_name new_nonslashed_name = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.rename_module" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.rename_module" )
  |Watcher _ -> raise ( Absent_method "File_watcher.rename_module" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.rename_module" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.rename_module" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.rename_module" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.rename_module fw old_middle_name new_nonslashed_name ) ;;

let rename_subdirectory_as final_fw (old_subdir,new_subdir) = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.rename_subdirectory_as" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.rename_subdirectory_as" )
  |Watcher _ -> raise ( Absent_method "File_watcher.rename_subdirectory_as" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.rename_subdirectory_as" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.rename_subdirectory_as" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.rename_subdirectory_as" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.rename_subdirectory_as fw (old_subdir,new_subdir) ) ;;

let replace_string final_fw old_s new_s = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.replace_string" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.replace_string" )
  |Watcher _ -> raise ( Absent_method "File_watcher.replace_string" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.replace_string" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.replace_string" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.replace_string" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.replace_string fw old_s new_s ) ;;

let replace_value final_fw pair_of_pairs = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.replace_value" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.replace_value" )
  |Watcher _ -> raise ( Absent_method "File_watcher.replace_value" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.replace_value" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.replace_value" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.replace_value" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.replace_value fw pair_of_pairs ) ;;

let root final_fw  = match final_fw with 
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.root" )
  |Configuration (fw) -> Fw_configuration.root fw
  |Watcher (fw) -> Private.root_on_file_watcher fw
  |With_archives (fw) -> Private.root_on_fw_with_archives fw
  |With_dependencies (fw) -> Private.root_on_fw_with_dependencies fw
  |With_batch_compilation (fw) -> Private.root_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.root_on_fw_with_githubbing fw ;;

let set_gitpush_after_backup final_fw gab = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.set_gitpush_after_backup" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.set_gitpush_after_backup" )
  |Watcher _ -> raise ( Absent_method "File_watcher.set_gitpush_after_backup" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.set_gitpush_after_backup" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.set_gitpush_after_backup" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.set_gitpush_after_backup" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.set_gitpush_after_backup fw gab ) ;;

let show_value_occurrences final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.show_value_occurrences" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.show_value_occurrences" )
  |Watcher _ -> raise ( Absent_method "File_watcher.show_value_occurrences" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.show_value_occurrences" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.show_value_occurrences" )
  |With_batch_compilation (fw) -> Fw_with_batch_compilation.show_value_occurrences fw
  |With_githubbing (fw) -> Private.show_value_occurrences_on_fw_with_githubbing fw ;;

let start_debugging final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.start_debugging" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.start_debugging" )
  |Watcher _ -> raise ( Absent_method "File_watcher.start_debugging" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.start_debugging" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.start_debugging" )
  |With_batch_compilation (fw) -> Fw_with_batch_compilation.start_debugging fw
  |With_githubbing (fw) -> Private.start_debugging_on_fw_with_githubbing fw ;;

let start_executing final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.start_executing" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.start_executing" )
  |Watcher _ -> raise ( Absent_method "File_watcher.start_executing" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.start_executing" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.start_executing" )
  |With_batch_compilation (fw) -> Fw_with_batch_compilation.start_executing fw
  |With_githubbing (fw) -> Private.start_executing_on_fw_with_githubbing fw ;;

let test_for_admissibility final_fw rl = match final_fw with 
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.test_for_admissibility" )
  |Configuration (fw) -> Fw_configuration.test_for_admissibility fw rl
  |Watcher (fw) -> Private.test_for_admissibility_on_file_watcher fw rl
  |With_archives (fw) -> Private.test_for_admissibility_on_fw_with_archives fw rl
  |With_dependencies (fw) -> Private.test_for_admissibility_on_fw_with_dependencies fw rl
  |With_batch_compilation (fw) -> Private.test_for_admissibility_on_fw_with_batch_compilation fw rl
  |With_githubbing (fw) -> Private.test_for_admissibility_on_fw_with_githubbing fw rl ;;

let to_concrete_object final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.to_concrete_object" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.to_concrete_object" )
  |Watcher _ -> raise ( Absent_method "File_watcher.to_concrete_object" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.to_concrete_object" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.to_concrete_object" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.to_concrete_object" )
  |With_githubbing (fw) ->  Fwc_with_githubbing.to_concrete_object fw  ;;

let to_fw_configuration final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.to_fw_configuration" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.to_fw_configuration" )
  |Watcher _ -> raise ( Absent_method "File_watcher.to_fw_configuration" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.to_fw_configuration" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.to_fw_configuration" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.to_fw_configuration" )
  |With_githubbing (fw) -> Fwc_with_githubbing.to_fw_configuration fw  ;;

let to_github_configuration final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.to_github_configuration" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.to_github_configuration" )
  |Watcher _ -> raise ( Absent_method "File_watcher.to_github_configuration" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.to_github_configuration" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.to_github_configuration" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.to_github_configuration" )
  |With_githubbing (fw) ->  Fwc_with_githubbing.to_github_configuration fw  ;;

let up_to_date_elesses final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.up_to_date_elesses" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.up_to_date_elesses" )
  |Watcher _ -> raise ( Absent_method "File_watcher.up_to_date_elesses" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.up_to_date_elesses" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.up_to_date_elesses" )
  |With_batch_compilation (fw) -> Fw_with_batch_compilation.up_to_date_elesses fw
  |With_githubbing (fw) -> Private.up_to_date_elesses_on_fw_with_githubbing fw ;;

let usual_compilable_files final_fw  = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.usual_compilable_files" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.usual_compilable_files" )
  |Watcher _ -> raise ( Absent_method "File_watcher.usual_compilable_files" )
  |With_archives (fw) -> Fw_with_archives.usual_compilable_files fw
  |With_dependencies (fw) -> Private.usual_compilable_files_on_fw_with_dependencies fw
  |With_batch_compilation (fw) -> Private.usual_compilable_files_on_fw_with_batch_compilation fw
  |With_githubbing (fw) -> Private.usual_compilable_files_on_fw_with_githubbing fw ;;

let usual_recompile final_fw opt_comment = match final_fw with 
  |Configuration _ -> raise ( Absent_method "Fw_configuration.usual_recompile" )
  |Github_configuration _ -> raise ( Absent_method "Fw_github_configuration.usual_recompile" )
  |Watcher _ -> raise ( Absent_method "File_watcher.usual_recompile" )
  |With_archives _ -> raise ( Absent_method "Fw_with_archives.usual_recompile" )
  |With_dependencies _ -> raise ( Absent_method "Fw_with_dependencies.usual_recompile" )
  |With_batch_compilation _ -> raise ( Absent_method "Fw_with_batch_compilation.usual_recompile" )
  |With_githubbing (fw) -> With_githubbing( Fwc_with_githubbing.usual_recompile fw opt_comment ) ;;
