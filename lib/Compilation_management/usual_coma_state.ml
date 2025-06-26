(* 

#use"lib/Compilation_management/usual_coma_state.ml";;

*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.This_World.triple in 
  let fw_config = Fw_configuration.of_root root 
  and github_config = Fw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Coma_big_constant.github_url
  ~encoding_protected_files:[]
  in 
  ref(Fwc_final_poly.Lfwc_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config);;

  let ref_for_unofficial_changes = ref(None : (string list) option) ;; 

end;;

let all_endinglesses ()=Fwc_final_poly.Lfw_with_dependencies.all_endinglesses (!(Private.main_ref)) ;; 

let changed_files_in_foreign_copy ()=
   let temp1=Fwc_final_poly.Lfw_with_dependencies.all_moduled_mlx_files (!(Private.main_ref)) in 
   let this_root = Dfa_root.connectable_to_subpath (Coma_big_constant.This_World.root) 
   and next_root = Dfa_root.connectable_to_subpath (Coma_big_constant.Next_World.root) in 
   let temp2=Explicit.filter (
      fun full_path->
         let rootless = Dfn_full.to_rootless full_path in 
         if rootless = Coma_constant.rootless_path_for_parametersfile 
         then false 
         else 
         let path = Dfn_rootless.to_line rootless in 
         let ap1=Absolute_path.of_string(this_root^path) 
         and ap2=Absolute_path.of_string(next_root^path) in 
         Io.read_whole_file(ap1)<>Io.read_whole_file(ap2)
   ) temp1 in 
   let answer = Image.image (fun full_path->
      Dfn_rootless.to_line(Dfn_full.to_rootless full_path)  
   ) temp2 in 
   let _ = (Private.ref_for_unofficial_changes := Some answer) in 
   answer;;    



let clean_debug_dir ()=Fwc_final_poly.Lfw_with_batch_compilation.clean_debug_dir (!(Private.main_ref));;
let clean_exec_dir ()=Fwc_final_poly.Lfw_with_batch_compilation.clean_exec_dir (!(Private.main_ref));;

let create_foreign_copy summary=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  let (next_dest,next_backup,next_gab) = Coma_big_constant.Next_World.triple in 
  let _=Create_world_copy.fully_developed_copy
  (!Private.main_ref) summary
  ~destination:next_dest ~destbackupdir:next_backup ~destgab:next_gab
  in 
  () ;; 

let duplicate_module old_t1 old_t2=
Fwc_final_poly.Lfw_with_dependencies.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_endingless modname = 
  Fwc_final_poly.Lfw_with_dependencies.endingless_at_module
   (!(Private.main_ref)) (Dfa_module.of_line (String.capitalize_ascii modname));;

let forget_one modname=Modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=Modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;

let initialize ()=Modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let initialize ()=Modify_coma_state.Reference.initialize Private.main_ref ;; 

let internet_access () = Fwc_final_poly.Lfwc_with_githubbing.gitpush_after_backup (!(Private.main_ref)) ;;

let latest_changes ()= Fwc_final_poly.Lfw_with_archives.latest_changes (!(Private.main_ref));;

let list_values_from_module_in_modulesystem module_name=
Fwc_final_poly.Lfw_with_dependencies.list_values_from_module (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

let modules_using_value x=Fwc_final_poly.Lfw_with_dependencies.modules_using_value (!(Private.main_ref)) x;;

let recompile opt=Modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=Modify_coma_state.Reference.refresh Private.main_ref;;

let register_rootless_line x=Modify_coma_state.Syntactic_sugar.register_one Private.main_ref x;;
  
let register_rootless_lines x=Modify_coma_state.Syntactic_sugar.register_several Private.main_ref x;;

let relocate_module_to old_module_name new_subdir=
   Modify_coma_state.Syntactic_sugar.relocate_module_to Private.main_ref old_module_name new_subdir;;

let rename_subdirectory old_subdirname new_subdirname=
    Modify_coma_state.Syntactic_sugar.rename_subdirectory Private.main_ref old_subdirname new_subdirname;;

let rename_module old_name new_name=
   Modify_coma_state.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;


let rename_string_or_value old_name new_name=
   Modify_coma_state.Syntactic_sugar.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let set_internet_access bowl=Modify_coma_state.Reference.internet_access Private.main_ref bowl;;


let show_value_occurrences_in_modulesystem module_name=
  Fwc_final_poly.Lfw_with_batch_compilation.show_value_occurrences (!(Private.main_ref)) module_name;;

let start_debugging ()=Fwc_final_poly.Lfw_with_batch_compilation.start_debugging (!(Private.main_ref));;
let start_executing short_path= Fwc_final_poly.Lfw_with_batch_compilation.start_executing (!(Private.main_ref)) short_path;;


let sugared_above capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Image.image Dfa_module.to_line
  (Fwc_final_poly.Lfw_with_dependencies.ancestors_for_module (!(Private.main_ref)) mn0);;

let sugared_below capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Image.image Dfa_module.to_line
  (Fwc_final_poly.Lfw_with_dependencies.below (!(Private.main_ref)) mn0);;

let sugared_directly_above capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  Image.image Dfa_module.to_line
  (Fwc_final_poly.Lfw_with_dependencies.direct_fathers_for_module (!(Private.main_ref)) mn0);;

let sugared_directly_below capitalized_or_not_module_name=
let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
Image.image Dfa_module.to_line
(Fwc_final_poly.Lfw_with_dependencies.directly_below (!(Private.main_ref)) mn0);;
