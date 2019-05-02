
(* 

#use"Compilation_management/modify_coma_state_and_backup.ml";;

*)

module Private = struct

      let backup cs diff opt= 
      if not(Dircopy_diff.is_empty diff) 
      then Backup_coma_state.backup
            (Coma_state.root cs,Coma_state.backup_dir cs,Coma_state.github_after_backup cs) 
            diff opt
      else (print_string "No recent changes to commit ...";flush stdout);;

end;;    

let forget cs x=
   let (cs2,diff)=Modify_coma_state_after_checking.forget cs x in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let recompile cs opt_comment=
   let (cs2,diff)=Coma_state.Almost_concrete.recompile_without_githubbing cs  in 
   let _=Private.backup cs2 diff opt_comment in 
   cs2;; 

(* No backup during refresh *)   

let register_short_path cs x=
   let (cs2,diff)=Modify_coma_state_after_checking.register_short_path cs x  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let relocate_module cs old_hm_name new_subdir=
   let (cs2,diff)=Modify_coma_state_after_checking.relocate_module cs old_hm_name new_subdir  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let rename_directory  cs old_subdir new_subdirname=
   let (cs2,diff)=Modify_coma_state_after_checking.rename_directory  cs old_subdir new_subdirname  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 


let rename_module cs old_hm_name new_subdir=
   let (cs2,diff)=Modify_coma_state_after_checking.rename_module cs old_hm_name new_subdir  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 

let rename_string_or_value cs old_hm_name new_subdir=
   let (cs2,diff)=Modify_coma_state_after_checking.rename_string_or_value cs old_hm_name new_subdir  in 
   let _=Private.backup cs2 diff None in 
   cs2;; 


