(* 
#use"lib/Compilation_management/experimental_version.ml";;
*)


module Private = struct 

let ref_for_unofficial_changes = ref (None: Dircopy_diff_t.t option) ;;

let force_compute_changes_in_experimental_version () =
  Prepare_dircopy_update.compute_restricted_diff
  Coma_big_constant.This_World.root Coma_big_constant.Next_World.root ([],[]) ;;

end ;;  

let create_experimental_copy_from_scratch () =
  let old_cs = (!(Usual_coma_state.main_ref)) in 
  let (next_dest,next_backup,next_gab) = 
   Coma_big_constant.Next_World.triple in 
 let _ = (
   Hashtbl.clear   Fw_with_dependencies.Private.Modularized_details.the_hashtbl ;
   Hashtbl.clear   Fw_with_dependencies.Private.Order.the_hashtbl ;
 ) in 
  Chronometer.it
 (Create_world_copy.fully_developed_copy
   ~destination:next_dest ~destbackupdir:next_backup ~destgab:next_gab
   old_cs) Needed_data_summary_t.Everything
  ;;   

let empty_and_refill_experimental_copy () = 
  let this_root = Dfa_root.connectable_to_subpath Coma_big_constant.This_World.root 
  and next_root = Dfa_root.connectable_to_subpath Coma_big_constant.Next_World.root 
  and s_lib = Dfa_subdirectory.connectable_to_subpath Coma_constant.dune_lib_subdir 
  and s_watched = Dfa_subdirectory.connectable_to_subpath Coma_constant.dune_watched_subdir in 
  let lib1 = this_root^s_lib
  and watched1 = this_root^s_watched 
  and lib2 = next_root^s_lib
  and watched2 = next_root^s_watched in 
  let _ = Unix_again.empty_directory lib2 in 
  let _ = Unix_again.empty_directory watched2 in 
  let cmd1 = "cp -R "^lib1^"* "^lib2 
  and cmd2 = "cp -R "^watched1^"* "^watched2 in 
  Unix_command.conditional_multiple_uc [cmd1;cmd2] ;; 

   
let changes_in_experimental_version () = 
  match (!(Private.ref_for_unofficial_changes)) with 
  Some old_answer -> old_answer 
  |None ->
   let answer = Private.force_compute_changes_in_experimental_version () in 
   let _ = (Private.ref_for_unofficial_changes:=Some answer) in 
   answer ;;

let force_compute_changes_in_experimental_version = Private.force_compute_changes_in_experimental_version ;;

let officialize_changes () =
  let diff = changes_in_experimental_version () in 
  let cmds = Prepare_dircopy_update.commands_for_update 
  (Coma_big_constant.This_World.root,
   Coma_big_constant.Next_World.root) diff in 
  Unix_command.conditional_multiple_uc cmds ;; 
     
