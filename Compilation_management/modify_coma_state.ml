(* 

#use"Compilation_management/modify_coma_state.ml";;

*)

module Tools = struct 

   let compute_long_subdir_name old_subdir new_subdir_short_name =
      let temp1 =  Cull_string.trim_slashes_on_the_right new_subdir_short_name in
      let long_name = (
      if String.contains temp1 '/'
      then temp1 
      else let old_subdir_name = Dfa_subdirectory.without_trailing_slash old_subdir in 
           let father_name = Cull_string.before_rightmost old_subdir_name '/' in 
           if father_name = ""
           then temp1
           else father_name^"/"^temp1 ) in 
      Dfa_subdirectory.of_line long_name ;;    

end ;;   

   
   module After_checking = struct

         let forget_modules cs mod_names=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Coma_state.forget_modules cs mod_names ;; 
   
         let forget_nonmodular_rootlesses cs rootless_paths=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Coma_state.forget_nonmodular_rootlesses cs rootless_paths ;; 

         (* No check needed before recompiling *)
         
         (* No check needed before refreshing *)   
   
         let register_rootless_paths cs rootless_paths=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Coma_state.register_rootless_paths cs rootless_paths ;; 
   

         let relocate_module_to cs old_module new_subdir=
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Coma_state.relocate_module_to cs old_module new_subdir ;; 
   

         let rename_module cs old_middle_name new_nonslashed_name=
            let _=Coma_state.check_that_no_change_has_occurred cs in  
            fst(Coma_state.rename_module cs old_middle_name new_nonslashed_name);; 


         let rename_subdirectory  cs old_subdir new_subdir= 
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Coma_state.rename_subdirectory_as  cs (old_subdir,new_subdir) ;; 
  
   
         let replace_string cs old_s new_s =
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Coma_state.replace_string cs old_s new_s ;;

         let replace_value cs ((preceding_files,path),(old_v,new_v)) =
            let _=Coma_state.check_that_no_change_has_occurred cs in 
            Coma_state.replace_value cs ((preceding_files,path),(old_v,new_v)) ;;   

   end;;
   
   
   
   module And_save = struct 
   
         let forget_modules cs mod_names=
            let cs2=After_checking.forget_modules cs mod_names in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
   
         let forget_nonmodular_rootlesses cs rootless_paths=
            let cs2=After_checking.forget_nonmodular_rootlesses cs rootless_paths in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
   
         let internet_access cs bowl=   
            let cs2=Coma_state.set_gitpush_after_backup cs bowl in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
         
         let recompile cs opt_comment=
            let cs2= Coma_state.usual_recompile cs opt_comment in 
            let _=Save_coma_state.save cs2 in 
            cs2;;
         

         let refresh cs =
            let cs2= Coma_state.of_configuration 
            (Coma_state.configuration cs) 
              (cs.Coma_state_t.dir_for_backup) 
                (cs.Coma_state_t.gitpush_after_backup) 
                  (cs.Coma_state_t.github_url)
                  (cs.Coma_state_t.encoding_protected_files)  in 
            let _=Save_coma_state.save cs2 in 
            cs2;;       

         let register_rootless_paths cs rootless_path=
            let cs2=After_checking.register_rootless_paths cs rootless_path in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
         let relocate_module_to cs old_module new_subdir=
         let cs2 = After_checking.relocate_module_to cs old_module new_subdir in 
         let _=Save_coma_state.save cs2 in 
         cs2;;   
   
         let rename_subdirectory cs old_subdir new_subdir=
            let cs2=After_checking.rename_subdirectory cs old_subdir new_subdir in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  
   
   
         let rename_module cs old_middle_name new_nonslashed_name=
            let cs2=After_checking.rename_module cs old_middle_name new_nonslashed_name in 
            let _=Save_coma_state.save cs2 in 
            cs2;;  


         let replace_string cs old_s new_s=
            let cs2=After_checking.replace_string cs old_s new_s in 
            let _=Save_coma_state.save cs2 in 
            cs2;;     
         
         let replace_value cs ((preceding_files,path),(old_v,new_v))=
            let cs2=After_checking.replace_value cs ((preceding_files,path),(old_v,new_v)) in 
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
   
         let replace_string pcs old_s new_s=
            let new_cs = And_save.replace_string (!pcs) old_s new_s in 
            pcs:=new_cs;;   
         
         let replace_value pcs ((preceding_files,path),(old_v,new_v))=
            let new_cs = And_save.replace_value (!pcs) ((preceding_files,path),(old_v,new_v)) in 
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
   
   
   exception Rename_string_or_value_exn of string ;;

   let rename_string_or_value cs_ref old_sov new_sov =
         if not(String.contains old_sov '.')
         then Reference.replace_string cs_ref old_sov new_sov 
         else 
         let j=Substring.leftmost_index_of_in "." old_sov in
         if j<0 
         then raise(Rename_string_or_value_exn(old_sov))
         else let module_name=Cull_string.beginning (j-1) old_sov in
         let endingless= Coma_state.decipher_module (!cs_ref)  module_name 
         and path= Coma_state.decipher_path (!cs_ref)  module_name in 
         let nm=Dfn_endingless.to_module endingless in
         let pre_temp2=(Coma_state.ancestors_for_module (!cs_ref) nm)@[nm] in
         let temp2=Image.image (Coma_state.endingless_at_module (!cs_ref)) pre_temp2 in
         let preceding_files=Image.image  (fun eless2->
                           Dfn_full.to_absolute_path(Dfn_join.to_ending eless2 Dfa_ending.ml)
         ) temp2 in
         Reference.replace_value cs_ref ((preceding_files,path),(old_sov,new_sov)) ;;       

   let rename_subdirectory cs_ref old_subdirname new_subdir_short_name=
       let old_subdir = Coma_state.find_subdir_from_suffix (!cs_ref) old_subdirname  in
       let new_subdir = Tools.compute_long_subdir_name old_subdir new_subdir_short_name  in 
       Reference.rename_subdirectory cs_ref old_subdir new_subdir ;;
   
   
   end;;