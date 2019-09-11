
(* 

#use"Compilation_management/modify_coma_state.ml";;

*)

module After_checking = struct

      exception Recompilation_needed of Dfa_module_t.t list;;

      module Private = struct

            let check_for_change_at_module_and_ending cs mn edg=
               let hm=Coma_state.endingless_at_module cs mn in 
               (Coma_state.md_recompute_modification_time hm edg)
               <>(Coma_state.get_modification_time cs mn edg);;

            let check_for_change_at_module  cs mn=
            List.exists
               (check_for_change_at_module_and_ending cs mn) 
            [
               Dfa_ending.mli ;
               (Coma_state.principal_ending_at_module cs mn)
            ] ;;

            let detect_changes cs =
            Option.filter_and_unpack (
               fun mn->
               if check_for_change_at_module cs mn 
               then Some(mn)
               else None
            ) (Coma_state.ordered_list_of_modules cs);;

            let check_for_changes cs = 
            let changes = detect_changes cs in 
            if changes<>[]
            then raise(Recompilation_needed(changes))
            else ();;

      end;;    

      let forget cs x=
         let _=Private.check_for_changes cs in 
         Coma_state.Almost_concrete.forget cs x;; 

      (* No check needed before recompiling *)

      (* No check needed before refreshing *)

      let register_short_path cs x=
         let _=Private.check_for_changes cs in 
         Coma_state.Almost_concrete.register_rootless_path cs x;; 

      let relocate_module cs old_hm_name new_subdir=
         let _=Private.check_for_changes cs in 
         Coma_state.Almost_concrete.local_relocate_module cs old_hm_name new_subdir;; 

      let rename_directory  cs old_subdir new_subdirname=
         let _=Private.check_for_changes cs in 
         Coma_state.Almost_concrete.local_rename_directory  cs old_subdir new_subdirname;; 

      let rename_module cs old_hm_name new_name=
         let _=Private.check_for_changes cs in 
         Coma_state.Almost_concrete.local_rename_module cs old_hm_name new_name;; 

      let rename_string_or_value cs old_hm_name new_name=
         let _=Private.check_for_changes cs in 
         Coma_state.Almost_concrete.rename_string_or_value cs old_hm_name new_name;; 

end;;

module And_backup = struct

      module Private = struct

            let backup cs diff opt= 
            if not(Dircopy_diff.is_empty diff) 
            then Backup_coma_state.backup
                  (Coma_state.root cs,Coma_state.backup_dir cs,Coma_state.gitpush_after_backup cs) 
                  diff opt
            else (print_string "No recent changes to commit ...";flush stdout);;

      end;;    

      let forget cs x=
         let (cs2,diff)=After_checking.forget cs x in 
         let _=Private.backup cs2 diff None in 
         cs2;; 

      let recompile cs opt_comment=
         let (cs2,diff)=Coma_state.Almost_concrete.recompile cs  in 
         let _=Private.backup cs2 diff opt_comment in 
         cs2;; 

      (* No backup during refresh *)   

      let register_short_path cs x=
         let (cs2,diff)=After_checking.register_short_path cs x  in 
         let msg="register "^x in 
         let _=Private.backup cs2 diff (Some msg) in 
         cs2;; 

      let relocate_module cs old_hm_name new_subdir=
         let (cs2,diff)=After_checking.relocate_module cs old_hm_name new_subdir  in
         let msg="move "^old_hm_name^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
         let _=Private.backup cs2 diff (Some msg) in  
         cs2;; 

      let rename_directory  cs old_subdir new_subdirname=
         let (cs2,diff)=After_checking.rename_directory  cs old_subdir new_subdirname  in 
         let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^" as "^new_subdirname in 
         let _=Private.backup cs2 diff (Some msg) in  
         cs2;; 


      let rename_module cs old_hm_name new_name=
         let (cs2,diff)=After_checking.rename_module cs old_hm_name new_name  in 
         let msg="rename "^old_hm_name^" as "^new_name in 
         let _=Private.backup cs2 diff (Some msg) in 
         cs2;; 

      let rename_string_or_value cs old_name new_name=
         let (cs2,diff)=After_checking.rename_string_or_value cs old_name new_name  in 
         let msg="rename "^old_name^" as "^new_name in 
         let _=Private.backup cs2 diff (Some msg) in 
         cs2;; 

end;;


module And_save = struct 

      let forget cs x=
         let cs2=And_backup.forget cs x in 
         let _=Save_coma_state.save cs2 in 
         cs2;;

      let internet_access cs bowl=   
         let cs2=Coma_state_field.set_push_after_backup cs bowl in 
         let _=Save_coma_state.save cs2 in 
         cs2;;
      
      let recompile cs opt_comment=
         let cs2=And_backup.recompile cs opt_comment in 
         let _=Save_coma_state.save cs2 in 
         cs2;;
      
      let refresh cs =
         let (cs2,_)=Coma_state.refresh cs  in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

      let register_short_path cs short_path=
         let cs2=And_backup.register_short_path cs short_path in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

      let relocate_module cs old_hm_name new_subdir=
      let cs2 = And_backup.relocate_module cs old_hm_name new_subdir in 
      let _=Save_coma_state.save cs2 in 
      cs2;;   

      let rename_directory cs old_subdir new_subdirname=
         let cs2=And_backup.rename_directory cs old_subdir new_subdirname in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  


      let rename_module cs old_name new_name=
         let cs2=And_backup.rename_module cs old_name new_name in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  


      let rename_string_or_value cs old_name new_name=
         let cs2=And_backup.rename_string_or_value cs old_name new_name in 
         let _=Save_coma_state.save cs2 in 
         cs2;;  

end ;;

module Reference = struct 

      let forget pcs x=
         let new_cs = And_save.forget (!pcs) x in 
         pcs:=new_cs;;


      let initialize pcs =
      let new_cs = Coma_state.read_persistent_version (!pcs) in 
      pcs:=new_cs;;

      let initialize_if_empty pcs =
         if Coma_state.system_size (!pcs)  = 0 
         then initialize pcs;;

      let internet_access pcs bowl=
         let new_cs = And_save.internet_access (!pcs) bowl in 
          pcs:=new_cs;;

      let recompile pcs opt_comment=
         let new_cs = And_save.recompile (!pcs) opt_comment in 
         pcs:=new_cs;;


      let refresh pcs =
         let new_cs = And_save.refresh (!pcs)  in 
         pcs:=new_cs;;

      let register_short_path pcs short_path=
         let new_cs = And_save.register_short_path (!pcs) short_path in 
         pcs:=new_cs;;


      let relocate_module pcs old_hm_name new_subdir=
         let new_cs = And_save.relocate_module (!pcs) old_hm_name new_subdir in 
         pcs:=new_cs;;  


      let rename_directory pcs old_subdir new_subdirname=
         let new_cs = And_save.rename_directory (!pcs) old_subdir new_subdirname in 
         pcs:=new_cs;;
         

      let rename_module pcs old_name new_name=
         let new_cs = And_save.rename_module (!pcs) old_name new_name in 
         pcs:=new_cs;;


      let rename_string_or_value pcs old_name new_name=
         let new_cs = And_save.rename_string_or_value (!pcs) old_name new_name in 
         pcs:=new_cs;;


end ;;
