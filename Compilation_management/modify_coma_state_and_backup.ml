
(* 

#use"Compilation_management/modify_coma_state_and_backup.ml";;

*)

module Private = struct

let backup cs diff opt= Backup_coma_state.backup
  (Coma_state.root cs,Coma_state.backup_dir cs,Coma_state.github_after_backup cs) 
    diff opt;;

end;;    

let forget cs x=
   let (cs2,diff)=Coma_state.Almost_concrete.forget cs x in 
   let _=Private.backup cs2 diff None in 
   cs2;; 


(*
let forget cs x=
   let cs2=Coma_state.Almost_concrete.forget cs x in 
   let _=Save_coma_state.save cs2 in 
   cs2;;
  
let recompile cs opt_comment=
   let cs2=Coma_state.Almost_concrete.recompile cs opt_comment in 
   let _=Save_coma_state.save cs2 in 
   cs2;;
  
let refresh cs =
   let cs2=Coma_state.Almost_concrete.refresh_with_backup cs  in 
   let _=Save_coma_state.save cs2 in 
   cs2;;  

let register_short_path cs short_path=
   let cs2=Coma_state.Almost_concrete.register_short_path cs short_path in 
   let _=Save_coma_state.save cs2 in 
   cs2;;  

let relocate_module cs old_hm_name new_subdir=
  let cs2 = Coma_state.Almost_concrete.local_relocate_module cs old_hm_name new_subdir in 
  let _=Save_coma_state.save cs2 in 
  cs2;;   


let rename_module cs old_name new_name=
   let cs2=Coma_state.Almost_concrete.local_rename_module cs old_name new_name in 
   let _=Save_coma_state.save cs2 in 
   cs2;;  

let rename_directory cs old_subdir new_subdirname=
   let cs2=Coma_state.Almost_concrete.local_rename_directory cs old_subdir new_subdirname in 
   let _=Save_coma_state.save cs2 in 
   cs2;;  



let rename_string_or_value cs old_name new_name=
   let cs2=Coma_state.Almost_concrete.rename_string_or_value cs old_name new_name in 
   let _=Save_coma_state.save cs2 in 
   cs2;;  
*)