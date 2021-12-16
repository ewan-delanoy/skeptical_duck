(* 

#use"Compilation_management/modify_coma_state.ml";;

*)




   
module Internal = struct


   let rename_module cs old_middle_name new_nonslashed_name (cs2,changes) =
     let root_dir=Coma_state.root cs in 
     let old_nm=Dfn_middle.to_module old_middle_name in 
     let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) in 
     let s_root=Dfa_root.connectable_to_subpath root_dir in   
     let s_build_dir=Dfa_subdirectory.connectable_to_subpath (Coma_constant.usual_build_subdir) in  
     let _=Unix_command.uc
         ("rm -f "^s_root^s_build_dir^
         (Dfa_module.to_line old_nm)^
         ".cm* ") in            
     let cs3=Coma_state.modern_recompile cs2 [new_nm] in 
     cs3;;
   
   
   let rename_string_or_value cs changed_modules_in_any_order = 
      Coma_state.modern_recompile cs changed_modules_in_any_order ;; 
   
   end;;
   
module Physical_followed_by_internal = struct
   
   exception Forget_modules_exn of Dfa_module_t.t  list ;;
   
   let forget_modules cs mod_names= 
     let check = Coma_state.check_module_sequence_for_forgettability cs mod_names in 
     if check <> []
     then raise(Forget_modules_exn(check))
     else 
     Coma_state.forget_modules cs mod_names  ;;    
        
   exception Forget_rootless_paths_exn of Dfa_module_t.t list ;;
   
   let forget_nonmodular_rootlesses cs rootless_paths= 
     Coma_state.remove_files cs rootless_paths  ;;
   
   let recompile cs = 
     let (cs2,changed_uc)=Coma_state.inspect_and_update cs  in 
     let unordered_mods = Image.image Dfn_rootless.to_module changed_uc in  
     Coma_state.modern_recompile cs2 unordered_mods ;;
     
   let refresh cs = Coma_state.of_configuration (Coma_state.configuration cs)  ;;


   let register_rootless_paths cs rootless_paths= 
      let (cs2,rls)=Coma_state.register_rootless_paths cs rootless_paths in 
      let unordered_mods = Image.image Dfn_rootless.to_module rls in  
      Coma_state.modern_recompile  cs2 unordered_mods;;
   

   let relocate_module_to cs mod_name new_subdir= 
     Coma_state.relocate_module_to cs mod_name  new_subdir ;;
   
   
   let rename_module cs old_middle_name new_nonslashed_name=
      let (cs2,changes)=Coma_state.rename_module cs old_middle_name new_nonslashed_name in
      Internal.rename_module cs old_middle_name new_nonslashed_name (cs2,changes);;
   
   let rename_subdirectory cs old_subdir new_subdir=
      Coma_state.rename_subdirectory_as cs (old_subdir,new_subdir) ;;
   
   let rename_string_or_value cs old_sov new_sov =
      let (cs2,changed_modules_in_any_order)=Coma_state.rename_string_or_value cs old_sov new_sov in
      Internal.rename_string_or_value cs2 changed_modules_in_any_order;;
   
   end;;
   
   
   module After_checking = struct
   
         let forget_modules cs mod_names=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Physical_followed_by_internal.forget_modules cs mod_names;; 
   
         let forget_nonmodular_rootlesses cs rootless_paths=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Physical_followed_by_internal.forget_nonmodular_rootlesses cs rootless_paths;;    
   
         (* No check needed before recompiling *)
   

         (* No check needed before refreshing *)
   
         let register_rootless_paths cs rootless_paths=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Physical_followed_by_internal.register_rootless_paths cs rootless_paths;; 
   
         let relocate_module_to cs old_module new_subdir=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Physical_followed_by_internal.relocate_module_to cs old_module new_subdir;; 
   
         let rename_subdirectory  cs old_subdir new_subdir=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Physical_followed_by_internal.rename_subdirectory  cs old_subdir new_subdir;; 
   
         let rename_module cs old_middle_name new_nonslashed_name=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Physical_followed_by_internal.rename_module cs old_middle_name new_nonslashed_name;; 
   
         let rename_string_or_value cs old_sov new_sov=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Physical_followed_by_internal.rename_string_or_value cs old_sov new_sov;; 
   
   end;;
   
   module And_backup = struct
   
         module Private = struct
   
               let backup cs diff opt= 
               if not(Dircopy_diff.is_empty diff) 
               then Reflect_change_in_github.backup
                     (Coma_state.configuration cs) 
                     diff opt
               else (print_string "No recent changes to commit ...";flush stdout);;
   
         end;;    
   
         let forget_modules cs mod_names=
            let  cs2=After_checking.forget_modules cs mod_names in 
            Coma_state.reflect_latest_changes_in_github cs2 None;; 
   
         let forget_nonmodular_rootlesses cs rootless_paths=
            let cs2=After_checking.forget_nonmodular_rootlesses cs rootless_paths in 
            Coma_state.reflect_latest_changes_in_github cs2 None ;; 
   
   
         let recompile cs opt_comment=
            let cs2=Physical_followed_by_internal.recompile cs  in 
            Coma_state.reflect_latest_changes_in_github cs2 opt_comment ;; 
   
         (* No backup during refresh *)   
   
         let register_rootless_paths cs rootless_paths=
            let descr = String.concat " , " (Image.image Dfn_rootless.to_line rootless_paths) in 
            let cs2=After_checking.register_rootless_paths cs rootless_paths  in 
            let msg="register "^descr in 
            Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 
   
         let relocate_module_to cs old_module new_subdir=
            let cs2=After_checking.relocate_module_to cs old_module new_subdir  in
            let msg="move "^(Dfa_module.to_line old_module)^" to "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
            Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 
   
   
         let rename_module cs old_middle_name new_nonslashed_name=
            let cs2=After_checking.rename_module cs old_middle_name new_nonslashed_name  in 
            let msg="rename "^(Dfa_module.to_line(Dfn_middle.to_module old_middle_name))^
                    " as "^(No_slashes.to_string new_nonslashed_name) in       
            Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 
   
         let rename_subdirectory  cs old_subdir new_subdir=
            let cs2=After_checking.rename_subdirectory  cs old_subdir new_subdir  in 
            let msg="rename "^(Dfa_subdirectory.connectable_to_subpath old_subdir)^
                       " as "^(Dfa_subdirectory.connectable_to_subpath new_subdir) in 
            Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 
   
         let rename_string_or_value cs old_sov new_sov=
            let cs2=After_checking.rename_string_or_value cs old_sov new_sov  in 
            let msg="rename "^old_sov^" as "^new_sov in 
            Coma_state.reflect_latest_changes_in_github cs2 (Some msg) ;; 
   
   end;;
   
   
   
   module And_save = struct 
   
         let forget_modules cs mod_names=
            let cs2=And_backup.forget_modules cs mod_names in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
   
         let forget_nonmodular_rootlesses cs rootless_paths=
            let cs2=And_backup.forget_nonmodular_rootlesses cs rootless_paths in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
   
         let internet_access cs bowl=   
            let cs2=Coma_state.set_gitpush_after_backup cs bowl in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
         
         let recompile cs opt_comment=
            let cs2=And_backup.recompile cs opt_comment in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
         
   
         let refresh cs =
            let cs2=Physical_followed_by_internal.refresh cs  in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
         let register_rootless_paths cs rootless_path=
            let cs2=And_backup.register_rootless_paths cs rootless_path in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
         let relocate_module_to cs old_module new_subdir=
         let cs2 = And_backup.relocate_module_to cs old_module new_subdir in 
         let _=Save_coma_state.save cs2 in 
         cs2;;   
   
         let rename_subdirectory cs old_subdir new_subdir=
            let cs2=And_backup.rename_subdirectory cs old_subdir new_subdir in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
   
         let rename_module cs old_middle_name new_nonslashed_name=
            let cs2=And_backup.rename_module cs old_middle_name new_nonslashed_name in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
   
         let rename_string_or_value cs old_sov new_sov=
            let cs2=And_backup.rename_string_or_value cs old_sov new_sov in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
   end ;;
   
   module Reference = struct 
   
         let forget_modules pcs mod_names=
            let new_cs = And_save.forget_modules (!pcs) mod_names in 
            pcs:=new_cs;;
   
         let forget_nonmodular_rootlesses pcs rootless_paths=
            let new_cs = And_save.forget_nonmodular_rootlesses (!pcs) rootless_paths in 
            pcs:=new_cs;; 
   
         let initialize pcs =
         let new_cs = Coma_state.read_persistent_version (!pcs) in 
         pcs:=new_cs;;

         let initialize_if_empty pcs =
            if Coma_state.number_of_modules (!pcs) = 0 
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
   
         let register_rootless_paths pcs rootless_paths=
            let new_cs = And_save.register_rootless_paths (!pcs) rootless_paths in 
            pcs:=new_cs;;
   
   
         let relocate_module_to pcs old_module new_subdir=
            let new_cs = And_save.relocate_module_to (!pcs) old_module new_subdir in 
            pcs:=new_cs;;  
   
   
         let rename_subdirectory pcs old_subdir new_subdir=
            let new_cs = And_save.rename_subdirectory (!pcs) old_subdir new_subdir in 
            pcs:=new_cs;;
            
   
         let rename_module pcs old_middle_name new_nonslashed_name=
            let new_cs = And_save.rename_module (!pcs) old_middle_name new_nonslashed_name in 
            pcs:=new_cs;;
   
   
         let rename_string_or_value pcs old_sov new_sov=
            let new_cs = And_save.rename_string_or_value (!pcs) old_sov new_sov in 
            pcs:=new_cs;;
   
   
   end ;;
   
   
   module Syntactic_sugar = struct 
   
   let forget cs_ref data = 
      let ref_for_modules = ref []
      and ref_for_paths = ref [] in 
      let _=List.iter (
         fun descr ->
           if String.contains descr '.'
           then ref_for_paths:= (Dfn_rootless.of_line descr)::(!ref_for_paths)
           else ref_for_modules:= (Dfa_module.of_line descr) ::(!ref_for_modules)
      ) data in
      let all_paths = List.rev(!ref_for_paths) 
      and all_modules =  List.rev(!ref_for_modules) in 
      let _=(if all_paths=[] then () else Reference.forget_nonmodular_rootlesses cs_ref all_paths) in 
      let _=(if all_modules=[] then () else Reference.forget_modules cs_ref all_modules) in 
      ();;
   
   
   let register_several cs_ref lines =
      let rootless_paths = Image.image Dfn_rootless.of_line lines in 
      Reference.register_rootless_paths cs_ref  rootless_paths ;;
   
   let register_one cs_ref line = register_several cs_ref [line];;
   
   let relocate_module_to cs_ref old_module_name new_subdir=
       let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
       Reference.relocate_module_to cs_ref mn new_subdir ;;
   
   let rename_module cs_ref old_module_name new_name=
      let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
      let old_eless = Coma_state.endingless_at_module (!cs_ref) mn in
      let old_middle_name = Dfn_endingless.to_middle old_eless in    
      let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii new_name) in 
      Reference.rename_module cs_ref old_middle_name new_nonslashed_name;; 
   
   
   let rename_subdirectory cs_ref old_subdirname new_subdir_short_name=
       let old_subdir = Coma_state.find_subdir_from_suffix (!cs_ref) old_subdirname  in
       let new_subdir = Coma_state.compute_long_subdir_name (!cs_ref) old_subdir new_subdir_short_name  in 
       Reference.rename_subdirectory cs_ref old_subdir new_subdir ;;
   
   
   end;;