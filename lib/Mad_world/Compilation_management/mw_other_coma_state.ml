(* 
#use"lib/Mad_world/Compilation_management/mw_other_coma_state.ml";;
*)



exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.Next_World.triple in 
  let fw_config = Mw_configuration.of_root root 
  and github_config = Mw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Coma_big_constant.github_url
  ~encoding_protected_files:[]
  in 
  ref(Mw_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config);;

let ref_for_unofficial_changes = ref(None : (string list) option) ;;  

let force_compute_unofficial_changes ()=
   let temp1=Mw_with_dependencies.all_moduled_mlx_files (!main_ref) in 
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
   let _ = (ref_for_unofficial_changes := Some answer) in answer;;    

   let see_yet_unofficial_changes ()=
      match (!ref_for_unofficial_changes) with 
      None -> force_compute_unofficial_changes ()
      |Some answer -> answer ;;
     

   let commands_for_change_officialization l=
   let this_root = Dfa_root.connectable_to_subpath (Coma_big_constant.This_World.root) 
   and next_root = Dfa_root.connectable_to_subpath (Coma_big_constant.Next_World.root) in 
   Image.image (
      fun path->
         "cp "^next_root^path^" "^this_root^path 
   ) l;;    
   

end;;

let above modname=Mw_with_dependencies.ancestors_for_module (!(Private.main_ref)) modname;;


let below modname=Mw_with_dependencies.below (!(Private.main_ref)) modname;;


let duplicate_module old_t1 old_t2=
  Mw_with_dependencies.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

  let find_endingless modname = 
   Mw_with_dependencies.endingless_at_module
    (!(Private.main_ref)) (Dfa_module.of_line (String.capitalize_ascii modname));;

let forget_one modname=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Mw_modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Mw_modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;
  

let initialize ()=Mw_modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Mw_modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Mw_with_dependencies.list_values_from_module (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;



let officialize_changes () =
   let temp1 = Private.see_yet_unofficial_changes () in 
   let cmds = Private.commands_for_change_officialization temp1 in
   let everything_ok = Unix_command.conditional_multiple_uc cmds in 
   let _=(if everything_ok then Private.ref_for_unofficial_changes:=None) in 
   everything_ok;;



let recompile opt=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Mw_modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=
  let _ = (Private.ref_for_unofficial_changes:=None) in   
 Mw_modify_coma_state.Reference.refresh Private.main_ref;;

let register_rootless_line x=
let _ = (Private.ref_for_unofficial_changes:=None) in 
  Mw_modify_coma_state.Syntactic_sugar.register_one Private.main_ref x;;

let rename_module old_name new_name=
   let _ = (Private.ref_for_unofficial_changes:=None) in  
   Mw_modify_coma_state.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;

let rename_string_or_value old_name new_name=
let _ = (Private.ref_for_unofficial_changes:=None) in 
   Mw_modify_coma_state.Syntactic_sugar.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let repopulate summary=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  let (next_dest,next_backup,next_gab) = Coma_big_constant.Next_World.triple in 
  let cs=Mw_create_world_copy.fully_developed_copy
  (!Mw_usual_coma_state.main_ref) summary
  ~destination:next_dest ~destbackupdir:next_backup ~destgab:next_gab
  in 
  (Private.main_ref := cs ;Mw_with_persisting.persist cs) ;; 

let see_yet_unofficial_changes = Private.see_yet_unofficial_changes ;;    

let show_value_occurrences_in_modulesystem module_name=
   Mw_with_batch_compilation.show_value_occurrences
   (!(Private.main_ref)) module_name;; 


