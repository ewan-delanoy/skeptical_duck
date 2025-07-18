(* 

#use"lib/Filewatching/usual_coma_state.ml";;

*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Fw_big_constant.This_World.triple in 
  let fw_config = Fwc_configuration.of_root root 
  and github_config = Fwc_github_configuration.make 
  ~v_root:root
  ~v_dir_for_backup:backup_dir
  ~v_gitpush_after_backup:githubbing
  ~v_github_url:Fw_big_constant.github_url
  ~v_encoding_protected_files:[] in 
  let final_fw = Fwc_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config in
  ref(final_fw);;

  let ref_for_unofficial_changes = ref(None : (string list) option) ;; 

  let commands_for_change_officialization l=
   let this_root = Dfa_root.connectable_to_subpath (Fw_big_constant.This_World.root) 
   and next_root = Dfa_root.connectable_to_subpath (Fw_big_constant.Next_World.root) in 
   Image.image (
      fun path->
         "cp "^next_root^path^" "^this_root^path 
   ) l;;    
    

end;;

let all_endinglesses ()=
  let fw_git = (!(Private.main_ref)) in 
  Fwc_with_githubbing.Inherited.all_endinglesses fw_git ;; 

let changed_files_in_foreign_copy ()= 
   let fw_git = (!(Private.main_ref)) in 
   let temp1=Fwc_with_githubbing.Inherited.all_moduled_mlx_files fw_git in 
   let this_root = Dfa_root.connectable_to_subpath (Fw_big_constant.This_World.root) 
   and next_root = Dfa_root.connectable_to_subpath (Fw_big_constant.Next_World.root) in 
   let temp2=Explicit.filter (
      fun full_path->
         let rootless = Dfn_full.to_rootless full_path in 
         if rootless = Fw_constant.rootless_path_for_parametersfile 
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



let clean_debug_dir ()=
  let fw_git = (!(Private.main_ref)) in 
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fw_debugging.clean_debug_dir fw_deps;;


let create_foreign_copy summary =
  let fw_git = (!(Private.main_ref)) in 
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  let _ = (Private.ref_for_unofficial_changes:=None) in
  let (next_dest,next_backup,next_gab) = Fw_big_constant.Next_World.triple in 
  Fw_world_copying.copy
  fw_deps summary
  ~destination:next_dest ~destbackupdir:next_backup ~destgab:next_gab ;; 

let current_state ()=
  let fw_git = (!(Private.main_ref)) in 
  fw_git;;


let duplicate_module old_t1 old_t2=
  let fw_git = (!(Private.main_ref)) in 
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fwc_with_dependencies.duplicate_module fw_deps old_t1 old_t2;;

let find_endingless modname = 
  let fw_git = (!(Private.main_ref)) in 
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fwc_with_dependencies.endingless_at_module
   fw_deps (Dfa_module.of_line (String.capitalize_ascii modname));;

let forget_one modname=Fw_syntactic_sugar.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=Fw_syntactic_sugar.Syntactic_sugar.forget Private.main_ref modnames;;

let load_persisted_version ()=Fw_act_on_reference.load_persisted_version Private.main_ref ;; 


let internet_access () = 
  let fw_git = (!(Private.main_ref)) in 
  Fwc_with_githubbing.Inherited.gitpush_after_backup fw_git ;;

let latest_changes ()= 
  let fw_git = (!(Private.main_ref)) in 
  let fw_arch = Fwc_with_githubbing.Inherited.to_fw_with_archives fw_git in 
    Fwc_with_archives.latest_changes fw_arch;;

let list_values_from_module_in_modulesystem module_name=
  let fw_git = (!(Private.main_ref)) in 
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fwc_with_dependencies.list_values_from_module fw_deps module_name;;


let modules_using_value x=
  let fw_git = (!(Private.main_ref)) in 
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fwc_with_dependencies.modules_using_value fw_deps x;;

let officialize_foreign_changes () =
   let temp1 = changed_files_in_foreign_copy () in 
   let cmds = Private.commands_for_change_officialization temp1 in
   Unix_command.conditional_multiple_uc cmds;;

   

let refresh ()=Fw_act_on_reference.refresh Private.main_ref;;

let register_rootless_line x=Fw_syntactic_sugar.Syntactic_sugar.register_one Private.main_ref x;;
  
let register_rootless_lines x=Fw_syntactic_sugar.Syntactic_sugar.register_several Private.main_ref x;;

let relocate_module_to old_module_name new_subdir=
   Fw_syntactic_sugar.Syntactic_sugar.relocate_module_to Private.main_ref old_module_name new_subdir;;

let rename_subdirectory old_subdirname new_subdirname=
    Fw_syntactic_sugar.Syntactic_sugar.rename_subdirectory Private.main_ref old_subdirname new_subdirname;;

let rename_module old_name new_name=
   Fw_syntactic_sugar.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;


let rename_string_or_value old_name new_name=
   Fw_syntactic_sugar.Syntactic_sugar.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let save_latest_changes opt=Fw_act_on_reference.save_latest_changes Private.main_ref opt;;


let self_contained_module_copy ~prefix hm= 
  let fw_git = (!(Private.main_ref)) in 
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fw_self_contained_module_copy.copy fw_deps ~prefix hm ;;

let set_internet_access bowl=Fw_act_on_reference.set_internet_access Private.main_ref bowl;;


let show_value_occurrences_in_modulesystem module_name=
  let fw_git = (!(Private.main_ref)) in  
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fwc_with_dependencies.show_value_occurrences fw_deps module_name;;

let start_debugging ()=
let fw_git = (!(Private.main_ref)) in  
let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Fw_debugging.start_debugging fw_deps;;


let sugared_above capitalized_or_not_module_name=
   
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let fw_git = (!(Private.main_ref)) in  
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Image.image Dfa_module.to_line
  (Fwc_with_dependencies.ancestors_for_module fw_deps mn0);;

let sugared_below capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let fw_git = (!(Private.main_ref)) in  
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Image.image Dfa_module.to_line
  (Fwc_with_dependencies.below fw_deps mn0);;

let sugared_directly_above capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let fw_git = (!(Private.main_ref)) in  
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in 
  Image.image Dfa_module.to_line
  (Fwc_with_dependencies.direct_fathers_for_module fw_deps mn0);;
 
let sugared_directly_below capitalized_or_not_module_name=
  let mn0 = Dfa_module.of_line(String.uncapitalize_ascii capitalized_or_not_module_name) in
  let fw_git = (!(Private.main_ref)) in  
  let fw_deps = Fwc_with_githubbing.Inherited.to_fw_with_dependencies fw_git in  
  Image.image Dfa_module.to_line
  (Fwc_with_dependencies.directly_below fw_deps mn0);;
