(* 
#use"Compilation_management/other_coma_state.ml";;
*)


exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.Next_World.triple 
  and url=Coma_big_constant.github_url in  
  let config = Fw_configuration.of_root root in 
  ref(Fw_with_githubbing.empty_one  config backup_dir githubbing url []);;

let ref_for_unofficial_changes = ref(None : (string list) option) ;;  

let force_compute_unofficial_changes ()=
   let temp1=Fw_with_githubbing.all_mlx_files (!main_ref) in 
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

let above modname=Fw_with_githubbing.ancestors_for_module (!(Private.main_ref)) modname;;


let below modname=Fw_with_githubbing.below (!(Private.main_ref)) modname;;


let duplicate_module old_t1 old_t2=
  Fw_with_githubbing.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

  let find_endingless modname = 
   Fw_with_githubbing.endingless_at_module
    (!(Private.main_ref)) (Dfa_module.of_line (String.capitalize_ascii modname));;

let forget_one modname=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;
  

let initialize ()=Modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Fw_with_githubbing.list_values_from_module (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;



let officialize_changes () =
   let temp1 = Private.see_yet_unofficial_changes () in 
   let cmds = Private.commands_for_change_officialization temp1 in
   let everything_ok = Unix_command.conditional_multiple_uc cmds in 
   let _=(if everything_ok then Private.ref_for_unofficial_changes:=None) in 
   everything_ok;;



let recompile opt=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=
  let _ = (Private.ref_for_unofficial_changes:=None) in   
 Modify_coma_state.Reference.refresh Private.main_ref;;

let register_rootless_line x=
let _ = (Private.ref_for_unofficial_changes:=None) in 
  Modify_coma_state.Syntactic_sugar.register_one Private.main_ref x;;

let rename_module old_name new_name=
   let _ = (Private.ref_for_unofficial_changes:=None) in  
   Modify_coma_state.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;

let rename_string_or_value old_name new_name=
let _ = (Private.ref_for_unofficial_changes:=None) in 
   Modify_coma_state.Syntactic_sugar.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let repopulate summary=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  let (next_dest,next_backup,next_gab) = Coma_big_constant.Next_World.triple in 
  let cs=Create_world_copy.fully_developed_copy
  (!Usual_coma_state.main_ref) summary
  ~destination:next_dest ~destbackupdir:next_backup ~destgab:next_gab
  in 
  (Private.main_ref := cs ;Fw_with_persisting.persist cs) ;; 

let see_yet_unofficial_changes = Private.see_yet_unofficial_changes ;;    

let show_value_occurrences_in_modulesystem module_name=
   Fw_with_githubbing.show_value_occurrences
   (!(Private.main_ref)) module_name;;


