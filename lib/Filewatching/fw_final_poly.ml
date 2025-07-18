(* 

#use"lib/Filewatching/fw_final_poly.ml";;

*)

exception Absent_method of string ;;




let all_endinglesses = Fwc_with_modular_infrastructure.all_endinglesses ;;
  
let all_moduled_mlx_files = Fwc_with_modular_infrastructure.all_moduled_mlx_files ;;

let all_subdirectories =  Fwc_with_modular_infrastructure.all_subdirectories ;;

let ancestors_for_module = Fwc_with_modular_infrastructure.ancestors_for_module  ;;

let below = Fwc_with_modular_infrastructure.below  ;;

let check_that_no_change_has_occurred = Fwc_with_archives.check_that_no_change_has_occurred  ;;

let decipher_module = Fwc_with_modular_infrastructure.decipher_module  ;;

let decipher_path =  Fwc_with_modular_infrastructure.decipher_path ;;

let dep_ordered_modules =  Fwc_with_modular_infrastructure.dep_ordered_modules  ;;

let directly_below = Fwc_with_modular_infrastructure.directly_below  ;;

let direct_fathers_for_module = Fwc_with_modular_infrastructure.direct_fathers_for_module  ;;

let duplicate_module = Fwc_with_modular_infrastructure.duplicate_module  ;;

let endingless_at_module = Fwc_with_modular_infrastructure.endingless_at_module  ;;

let find_subdir_from_suffix = Fwc_with_modular_infrastructure.find_subdir_from_suffix  ;;

let forget_modules =  Fwc_with_githubbing.forget_modules ;;
let forget_nonmodular_rootlesses = 
  Fwc_with_githubbing.forget_nonmodular_rootlesses ;;

let github_configuration = Fwc_with_githubbing.github_configuration ;;

let gitpush_after_backup = 
  Fwc_github_configuration.gitpush_after_backup ;;
let ignored_files = Fwc_configuration.ignored_files  ;;

let ignored_subdirectories = Fwc_configuration.ignored_subdirectories ;;

let latest_changes = Fwc_with_archives.latest_changes ;;

let list_values_from_module = Fwc_with_modular_infrastructure.list_values_from_module  ;;


let modules_using_value = Fwc_with_modular_infrastructure.modules_using_value ;;

let noncompilable_files = Fwc_with_archives.noncompilable_files ;;

let number_of_modules =  Fwc_with_modular_infrastructure.number_of_modules ;;
let of_concrete_object = Fwc_with_githubbing.Crobj.of_concrete_object ;;

let plunge_fw_config_with_github_config fw_config github_config =  Fwc_with_githubbing.plunge_fw_config_with_github_config fw_config github_config ;;
let register_rootless_paths = Fwc_with_githubbing.register_rootless_paths  ;;

let relocate_module_to = Fwc_with_githubbing.relocate_module_to  ;;
let rename_module =  Fwc_with_githubbing.rename_module  ;;

let rename_subdirectory_as = Fwc_with_githubbing.rename_subdirectory_as ;;

let replace_string =  Fwc_with_githubbing.replace_string ;;

let replace_value = Fwc_with_githubbing.replace_value ;;

let root= Fwc_configuration.root  ;;

let set_gitpush_after_backup = Fwc_github_configuration.set_gitpush_after_backup ;;

let show_value_occurrences = Fwc_with_modular_infrastructure.show_value_occurrences  ;;


let test_for_admissibility = Fwc_configuration.test_for_admissibility  ;;

let to_concrete_object = Fwc_with_githubbing.Crobj.to_concrete_object ;;
let usual_compilable_files = Fwc_with_archives.usual_compilable_files  ;;

  