(* 
#use"Compilation_management/other_coma_state.ml";;
*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.Next_World.triple 
  and url=Coma_big_constant.github_url in  
  let config = Fw_configuration.constructor (root,backup_dir,githubbing,url,[]) in 
  ref(Coma_state.empty_one  config);;

let ref_for_unofficial_changes = ref(None : (string list) option) ;;  

let force_compute_unofficial_changes ()=
   let temp1=Coma_state.all_mlx_files (!main_ref) in 
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

let above modname=Coma_state.Almost_concrete.local_above (!(Private.main_ref)) modname;;


let below modname=Coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;



let decipher_path pathname= Coma_state.decipher_path (!(Private.main_ref)) pathname;;
let decipher_module modname= Coma_state.decipher_module (!(Private.main_ref)) modname;;

let directly_below modname=Coma_state.Almost_concrete.local_directly_below (!(Private.main_ref)) modname;;

let duplicate_module old_t1 old_t2=
  Coma_state.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_endingless modname= 
  Coma_state.endingless_from_mildly_capitalized_module_name (!(Private.main_ref)) modname;;

let forget_one modname=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  Modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;
  

let initialize ()=Modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Coma_state.list_values_from_module (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;



let officialize_changes () =
   let temp1 = Private.see_yet_unofficial_changes () in 
   let cmds = Private.commands_for_change_officialization temp1 in
   Unix_command.conditional_multiple_uc cmds;;



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
   Modify_coma_state.Reference.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let repopulate summary=
  let _ = (Private.ref_for_unofficial_changes:=None) in 
  let (next_dest,next_backup,next_gab) = Coma_big_constant.Next_World.triple in 
  let cs=Create_world_copy.fully_developed_copy
  (!Usual_coma_state.main_ref) summary
  ~destination:next_dest ~destbackupdir:next_backup ~destgab:next_gab
  in 
  (Private.main_ref := cs ;Save_coma_state.save cs) ;; 

let see_yet_unofficial_changes = Private.see_yet_unofficial_changes ;;    

let show_value_occurrences_in_modulesystem module_name=
   Coma_state.show_value_occurrences
   (!(Private.main_ref)) module_name;;


