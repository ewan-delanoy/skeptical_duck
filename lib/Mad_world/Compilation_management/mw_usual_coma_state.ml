(* 

#use"lib/Mad_world/Compilation_management/mw_usual_coma_state.ml";;

*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.This_World.triple in 
  let fw_config = Mw_configuration.of_root root 
  and github_config = Mw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Coma_big_constant.github_url
  ~encoding_protected_files:[]
  in 
  ref(Mw_with_persisting.Constructor.plunge_fw_config_with_github_config  fw_config github_config);;
end;;

let all_endinglesses ()=Mw_with_persisting.Inherited.all_endinglesses (!(Private.main_ref)) ;; 

let clean_debug_dir ()=Mw_with_persisting.Inherited.clean_debug_dir (!(Private.main_ref));;
let clean_exec_dir ()=Mw_with_persisting.Inherited.clean_exec_dir (!(Private.main_ref));;

let duplicate_module old_t1 old_t2=
Mw_with_persisting.Inherited.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_endingless modname = 
  Mw_with_persisting.Inherited.endingless_at_module
   (!(Private.main_ref)) (Dfa_module.of_line (String.capitalize_ascii modname));;

let forget_one modname=Mw_modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=Mw_modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;

let initialize ()=Mw_modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Mw_modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let initialize ()=Mw_modify_coma_state.Reference.initialize Private.main_ref ;; 

let internet_access () = Mw_with_persisting.Inherited.gitpush_after_backup (!(Private.main_ref)) ;;

let latest_changes ()= Mw_with_persisting.Inherited.latest_changes (!(Private.main_ref));;

let list_values_from_module_in_modulesystem module_name=
Mw_with_persisting.Inherited.list_values_from_module (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

let modules_using_value x = Mw_with_persisting.Inherited.modules_using_value (!(Private.main_ref)) x;;

let recompile opt=Mw_modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=Mw_modify_coma_state.Reference.refresh Private.main_ref;;

let register_rootless_line x=Mw_modify_coma_state.Syntactic_sugar.register_one Private.main_ref x;;
  
let register_rootless_lines x=Mw_modify_coma_state.Syntactic_sugar.register_several Private.main_ref x;;

let relocate_module_to old_module_name new_subdir=
   Mw_modify_coma_state.Syntactic_sugar.relocate_module_to Private.main_ref old_module_name new_subdir;;

let rename_subdirectory old_subdirname new_subdirname=
    Mw_modify_coma_state.Syntactic_sugar.rename_subdirectory Private.main_ref old_subdirname new_subdirname;;

let rename_module old_name new_name=
   Mw_modify_coma_state.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;


let rename_string_or_value old_name new_name=
   Mw_modify_coma_state.Syntactic_sugar.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let set_internet_access bowl=Mw_modify_coma_state.Reference.internet_access Private.main_ref bowl;;


let show_value_occurrences_in_modulesystem module_name=
Mw_with_persisting.Inherited.show_value_occurrences (!(Private.main_ref)) module_name;;

let start_debugging ()= Mw_with_persisting.Inherited.start_debugging (!(Private.main_ref));;
let start_executing short_path= Mw_with_persisting.Inherited.start_executing (!(Private.main_ref)) short_path;;


let sugared_above capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Image.image Dfa_module.to_line
  (Mw_with_persisting.Inherited.ancestors_for_module (!(Private.main_ref)) mn0);;

let sugared_below capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Image.image Dfa_module.to_line
  (Mw_with_persisting.Inherited.below (!(Private.main_ref)) mn0);;

let sugared_directly_above capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Image.image Dfa_module.to_line
  (Mw_with_persisting.Inherited.direct_fathers_for_module (!(Private.main_ref)) mn0);;

let sugared_directly_below capitalized_or_not_module_name=
let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
Image.image Dfa_module.to_line
(Mw_with_persisting.Inherited.directly_below (!(Private.main_ref)) mn0);;
