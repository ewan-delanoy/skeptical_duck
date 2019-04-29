
(* 

#use"Compilation_management/modify_coma_state_after_checking.ml";;

*)

exception Recompilation_needed of Naked_module_t.t list;;

module Private = struct

let check_for_change_at_index_and_ending cs idx edg=
   let hm=Coma_state.hm_at_idx cs idx in 
   (Coma_state.md_recompute_modification_time hm edg)
   <>(Coma_state.get_modification_time cs idx edg);;
  
let check_for_change_at_index  cs idx=
  List.exists
    (check_for_change_at_index_and_ending cs idx) 
  [
    Ocaml_ending.Mli ;
    (Coma_state.principal_ending_at_idx cs idx)
  ] ;;

let detect_changes cs =
  let n=Coma_state.size cs in 
  Option.filter_and_unpack (
    fun idx->
     if check_for_change_at_index cs idx 
     then Some(Coma_state.module_at_idx cs idx)
     else None
  ) (Ennig.ennig 1 n);;

let check_for_changes cs = 
  let changes = detect_changes cs in 
  if changes<>[]
  then raise(Recompilation_needed(changes))
  else ();;

end;;    


(*
let forget cs x=
   let (cs2,diff)=Coma_state.Almost_concrete.forget cs x in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let recompile cs opt_comment=
   let (cs2,diff)=Coma_state.Almost_concrete.recompile_without_githubbing cs  in 
   let _=Private.backup cs2 diff opt_comment in 
   cs2;; 

(* No backup during refresh *)   

let register_short_path cs x=
   let (cs2,diff)=Coma_state.Almost_concrete.register_short_path cs x  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let relocate_module cs old_hm_name new_subdir=
   let (cs2,diff)=Coma_state.Almost_concrete.local_relocate_module cs old_hm_name new_subdir  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let rename_directory  cs old_subdir new_subdirname=
   let (cs2,diff)=Coma_state.Almost_concrete.local_rename_directory  cs old_subdir new_subdirname  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 


let rename_module cs old_hm_name new_subdir=
   let (cs2,diff)=Coma_state.Almost_concrete.local_rename_module cs old_hm_name new_subdir  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let rename_string_or_value cs old_hm_name new_subdir=
   let (cs2,diff)=Coma_state.Almost_concrete.rename_string_or_value cs old_hm_name new_subdir  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

*)
