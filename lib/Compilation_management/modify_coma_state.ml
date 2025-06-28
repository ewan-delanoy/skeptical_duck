(* 

#use"lib/Compilation_management/modify_coma_state.ml";;

*)


   
   
   module And_save = struct 
   
         let forget_modules fw mod_names=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2 = Fw_final_poly.Lfwc_with_githubbing.forget_modules fw mod_names in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;
   
         let forget_nonmodular_rootlesses fw rootless_paths=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2 = Fw_final_poly.Lfwc_with_githubbing.forget_nonmodular_rootlesses fw rootless_paths in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;
   
         let internet_access fw bowl=   
            let fw2=Fw_final_poly.Lfwc_with_githubbing.set_gitpush_after_backup fw bowl in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;
         
         let recompile fw opt_comment=
            let fw2= Fw_final_poly.Lfwc_with_githubbing.usual_recompile fw opt_comment in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;
         

         let refresh fw =
            let fw2= Fw_final_poly.Lfwc_with_githubbing.of_fw_config_and_github_config 
            (Fw_final_poly.Lfwc_with_githubbing.to_fw_configuration fw) 
            (Fw_final_poly.Lfwc_with_githubbing.to_github_configuration fw)  in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;       

         let register_rootless_paths fw rootless_path=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2 = Fw_final_poly.Lfwc_with_githubbing.register_rootless_paths fw rootless_path in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;  
   
         let relocate_module_to fw old_module new_subdir=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2 = Fw_final_poly.Lfwc_with_githubbing.relocate_module_to fw old_module new_subdir in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;   
   
         let rename_module fw old_middle_name new_nonslashed_name=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2=Fw_final_poly.Lfwc_with_githubbing.rename_module fw old_middle_name new_nonslashed_name in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;  

         let rename_subdirectory fw old_subdir new_subdir=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2=Fw_final_poly.Lfwc_with_githubbing.rename_subdirectory_as fw (old_subdir,new_subdir) in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;  


         let replace_string fw old_s new_s=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2=Fw_final_poly.Lfwc_with_githubbing.replace_string fw old_s new_s in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;     
    
         
         let replace_value fw ((preceding_files,path),(old_v,new_v))=
            let _=Fw_final_poly.Lfw_with_archives.check_that_no_change_has_occurred fw in 
            let fw2= Fw_final_poly.Lfwc_with_githubbing.replace_value fw ((preceding_files,path),(old_v,new_v)) in 
            let _=Fw_with_persisting.persist fw2 in 
            fw2;;        
   
   end ;;
   
   module Reference = struct 
   
         let forget_modules pfw mod_names=
            let new_fw = And_save.forget_modules (!pfw) mod_names in 
            pfw:=new_fw;;
   
         let forget_nonmodular_rootlesses pfw rootless_paths=
            let new_fw = And_save.forget_nonmodular_rootlesses (!pfw) rootless_paths in 
            pfw:=new_fw;; 
   
         let initialize pfw =
         let new_fw = Fw_with_persisting.read_persistent_version (!pfw) in 
         pfw:=new_fw;;

         let initialize_if_empty pfw =
            if Fw_final_poly.Lfw_with_dependencies.number_of_modules (!pfw) = 0 
            then initialize pfw;;
   
         let internet_access pfw bowl=
            let new_fw = And_save.internet_access (!pfw) bowl in 
             pfw:=new_fw;;
   
         let recompile pfw opt_comment=
            let new_fw = And_save.recompile (!pfw) opt_comment in 
            pfw:=new_fw;;
   
   
         let refresh pfw =
            let new_fw = And_save.refresh (!pfw)  in 
            pfw:=new_fw;;
   
         let register_rootless_paths pfw rootless_paths=
            let new_fw = And_save.register_rootless_paths (!pfw) rootless_paths in 
            pfw:=new_fw;;
   
   
         let relocate_module_to pfw old_module new_subdir=
            let new_fw = And_save.relocate_module_to (!pfw) old_module new_subdir in 
            pfw:=new_fw;;  
   
   
         let rename_subdirectory pfw old_subdir new_subdir=
            let new_fw = And_save.rename_subdirectory (!pfw) old_subdir new_subdir in 
            pfw:=new_fw;;
            
   
         let rename_module pfw old_middle_name new_nonslashed_name=
            let new_fw = And_save.rename_module (!pfw) old_middle_name new_nonslashed_name in 
            pfw:=new_fw;;
   
         let replace_string pfw old_s new_s=
            let new_fw = And_save.replace_string (!pfw) old_s new_s in 
            pfw:=new_fw;;   
         
         let replace_value pfw ((preceding_files,path),(old_v,new_v))=
            let new_fw = And_save.replace_value (!pfw) ((preceding_files,path),(old_v,new_v)) in 
            pfw:=new_fw;;      
   
   end ;;
   
   
   module Syntactic_sugar = struct 
   
   let display_number_of_modules fw_ref =
      let number_of_modules = List.length(Fw_final_poly.Lfw_with_dependencies.all_endinglesses (!fw_ref)) in 
      let msg = "\nThere are now "^(string_of_int number_of_modules)^" modules defined.\n" in 
      (print_string msg;flush stdout) ;; 

   let forget fw_ref data = 
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
      let _=(if all_paths=[] then () else Reference.forget_nonmodular_rootlesses fw_ref all_paths) in 
      let _=(if all_modules=[] then () else Reference.forget_modules fw_ref all_modules) in 
      display_number_of_modules fw_ref;;
   
   
   let register_several fw_ref lines =
      let rootless_paths = Image.image Dfn_rootless.of_line lines in 
      let _ = Reference.register_rootless_paths fw_ref  rootless_paths in 
      display_number_of_modules fw_ref ;;
   
   let register_one fw_ref line = 
      register_several fw_ref [line]  ;;
   
   let relocate_module_to fw_ref old_module_name new_subdir=
       let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
       Reference.relocate_module_to fw_ref mn new_subdir ;;
   
   let rename_module fw_ref old_module_name new_name=
      let mn = Dfa_module.of_line(String.uncapitalize_ascii old_module_name) in
      let old_eless = Fw_final_poly.Lfw_with_dependencies.endingless_at_module (!fw_ref) mn in
      let old_middle_name = Dfn_endingless.to_middle old_eless in    
      let new_nonslashed_name = No_slashes.of_string (String.uncapitalize_ascii new_name) in 
      Reference.rename_module fw_ref old_middle_name new_nonslashed_name;; 
   
   exception Rename_string_or_value_exn of string ;;

   let rename_string_or_value fw_ref old_sov new_sov =
         if not(String.contains old_sov '.')
         then Reference.replace_string fw_ref old_sov new_sov 
         else 
         let j_opt=Substring.leftmost_index_of_in_from_opt "." old_sov 1 in
         if j_opt= None
         then raise(Rename_string_or_value_exn(old_sov))
         else 
         let j = Option.get j_opt in 
         let module_name=Cull_string.beginning (j-1) old_sov in
         let endingless= Fw_final_poly.Lfw_with_dependencies.decipher_module (!fw_ref)  module_name 
         and path= Fw_final_poly.Lfw_with_dependencies.decipher_path (!fw_ref)  module_name in 
         let nm=Dfn_endingless.to_module endingless in
         let pre_temp2=(Fw_final_poly.Lfw_with_dependencies.ancestors_for_module (!fw_ref) nm)@[nm] in
         let temp2=Image.image (Fw_final_poly.Lfw_with_dependencies.endingless_at_module (!fw_ref)) pre_temp2 in
         let preceding_files=Image.image  (fun eless2->
                           Dfn_full.to_absolute_path(Dfn_join.to_ending eless2 Dfa_ending.ml)
         ) temp2 in
         Reference.replace_value fw_ref ((preceding_files,path),(old_sov,new_sov)) ;;       

   let rename_subdirectory fw_ref old_subdirname new_subdir_short_name=
       let old_subdir = Fw_final_poly.Lfw_with_dependencies.find_subdir_from_suffix (!fw_ref) old_subdirname  in
       let new_subdir = Dfa_subdirectory.compute_long_subdir_name old_subdir new_subdir_short_name  in 
       Reference.rename_subdirectory fw_ref old_subdir new_subdir ;;
   
   
   end;;