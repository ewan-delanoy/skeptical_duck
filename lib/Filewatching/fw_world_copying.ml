(*

#use"lib/Filewatching/fw_world_copying.ml";;

*)

module Private = struct

   module Data_summary = struct 
      let compute_all_needed_elesses fw needed_modules needed_subdirs =
         let all_elesses = Fwc_with_dependencies.all_endinglesses fw in 
         let step1_modules = List.filter_map 
         (fun eless->
            if List.mem (Dfn_endingless.to_subdirectory eless) 
               needed_subdirs
            then Some(Dfn_endingless.to_module eless)
            else None) all_elesses in
         let step2_modules = needed_modules@step1_modules in 
         let modules_above=List.flatten (Image.image (fun nm->
               Fwc_with_dependencies.ancestors_for_module fw nm
         ) step2_modules)  in 
         let list_of_modules_with_nonstandard_ordering = 
                   Ordered.sort Total_ordering.standard 
                   (modules_above@step2_modules) in 
         List.filter 
            (fun eless-> List.mem (Dfn_endingless.to_module eless) 
            list_of_modules_with_nonstandard_ordering)
             all_elesses ;;
         
             
         
      let expand fw summary =
         let all_needed_elesses =
         (match summary with 
            None -> Fwc_with_dependencies.all_endinglesses fw
          |Some(needed_modules,needed_subdirs)-> 
            compute_all_needed_elesses fw needed_modules needed_subdirs
         ) in 
         let all_needed_subdirs = 
            Ordered.sort Total_ordering.standard 
                  (Image.image Dfn_endingless.to_subdirectory all_needed_elesses) 
         and all_needed_modules = 
                  Image.image Dfn_endingless.to_module all_needed_elesses in      
         let original_noncompilables = Fwc_with_dependencies.Inherited.noncompilable_files fw in
         (*
            we do not know a priori if the noncompilables in other subdirectories
            are needed, so we include them all by default 
         *)      
         let noncompilables =
         (match summary with 
            None -> original_noncompilables
           |Some(_needed_modules,_needed_subdirs)-> 
                       List.filter (
                         fun rless-> List.mem (Dfn_rootless.to_subdirectory rless) all_needed_subdirs 
                     ) original_noncompilables) in        
         let compilables= List.filter (
            fun rless->List.mem (Dfn_rootless.to_module rless) all_needed_modules 
         ) (Fwc_with_dependencies.Inherited.usual_compilable_files fw) in
         (all_needed_modules,compilables,noncompilables);;

   end ;;   

   let text_for_big_constants_file_in_other_world destination destbackupdir destgab =
     let ds = Particular_string.double_semicolon in 
     let proj_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash destination) '/'
     and git_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash destbackupdir) '/' in
     let proj_name2 = Cull_string.after_rightmost (Dfa_root.without_trailing_slash Fw_big_constant.Third_World.root) '/'
     and git_name2 = Cull_string.after_rightmost (Dfa_root.without_trailing_slash Fw_big_constant.Third_World.backup_dir) '/' in
     let passage_appearing_twice =
     [
       "let root=Dfa_root.of_line  (root_of_root^\""^proj_name2^"\") "^ds;
       "let backup_dir=Dfa_root.of_line (root_of_root^\""^git_name2^"\") "^ds;
       "let githubbing="^(string_of_bool Fw_big_constant.Third_World.githubbing)^ds;
       "let triple = (root,backup_dir,githubbing)"^ds^"\n"; 
     ] in        
     String.concat "\n" ([
       "\n(* "; 
       "#use\"Compilation_management/coma_big_constant.ml\""^ds;
      "*)\n"; 
      "let github_url = \""^(Fw_big_constant.github_url)^"\""^ds;
      "let home = Sys.getenv \"HOME\" "^ds;
      "let root_of_root = home^\"/Teuliou/OCaml/\" "^ds^"\n";
      "module This_World=struct\n";
      "let root=Dfa_root.of_line (root_of_root^\""^proj_name^"\") "^ds;
      "let backup_dir=Dfa_root.of_line (root_of_root^\""^git_name^"\") "^ds;
      "let githubbing="^(string_of_bool destgab)^ds;
      "let triple = (root,backup_dir,githubbing)"^ds^"\n"; 
      "end"^ds;
      "module Next_World=struct\n"
     ]
     @passage_appearing_twice@
     [
      "end"^ds;
      "module Third_World=struct\n"
      ]
      @passage_appearing_twice@
      [ 
      "end"^ds;
      "\n\n\n"
      ]);;
   
     let commands_for_copying source rootlesses destination git_dir=
        let s_old_root=Dfa_root.connectable_to_subpath(source) 
        and s_new_root=Dfa_root.connectable_to_subpath destination 
        and s_git_root=Dfa_root.connectable_to_subpath git_dir in
        let unordered_subdirs = Image.image Dfn_rootless.to_subdirectory rootlesses in  
        let needed_subdirs = Ordered.sort Total_ordering.standard unordered_subdirs in 
        let dir_commands1 = Image.image (
          fun subdir ->"mkdir -p "^s_new_root^(Dfa_subdirectory.without_trailing_slash subdir)
        ) needed_subdirs 
        and dir_commands2 = Image.image (
         fun subdir ->"mkdir -p "^s_git_root^(Dfa_subdirectory.without_trailing_slash subdir)
       ) needed_subdirs 
        and file_commands1 = Image.image (fun rootless->
            let line = Dfn_rootless.to_line rootless in 
            "cp "^s_old_root^line^" "^s_new_root^(Cull_string.before_rightmost line '/')
       ) rootlesses 
       and file_commands2 = Image.image (fun rootless->
         let line = Dfn_rootless.to_line rootless in 
         "cp "^s_old_root^line^" "^s_git_root^(Cull_string.before_rightmost line '/')
      ) rootlesses in 
       dir_commands1 @ dir_commands2 @ file_commands1 @ file_commands2;;
   
     
   
   let default_backup_dir=Fw_big_constant.Next_World.backup_dir;;
 
 
   let frozen_copy fw ~destination ?(destbackupdir=default_backup_dir) ?(destgab=false)  summary =
       let proj_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash destination) '/' in
       let (conv_files,needed_dirs) = (
         if summary = None
         then (Fw_constant.conventional_files_with_full_content proj_name,
               Fw_constant.full_set_of_needed_dirs)
         else (Fw_constant.conventional_files_with_minimal_content proj_name,
               Fw_constant.minimal_set_of_needed_dirs)
       ) in 
       let _=Unix_again.delete_directory destination in 
       let old_dir = Sys.getcwd () in 
       let _ = Sys.chdir (Fw_big_constant.root_of_root) in 
       let _ = Unix_command.uc ("dune init proj "^proj_name) in 
       let _ = Sys.chdir old_dir in  
       let _=(Unix_again.create_subdirs_and_fill_files
       destination needed_dirs conv_files) in 
       let (_modules_in_good_order,compilables,noncompilables) = 
           Data_summary.expand fw summary in 
       let old_root = Fwc_with_dependencies.Inherited.root fw in
       let _=Image.image Unix_command.uc 
        (commands_for_copying old_root (compilables@noncompilables) destination destbackupdir) in
        let parameters_ap = Absolute_path.of_string 
        (Dfn_common.recompose_potential_absolute_path destination 
         Fw_constant.rootless_path_for_parametersfile) 
       and new_content = text_for_big_constants_file_in_other_world destination destbackupdir destgab in
       let _=Io.overwrite_with parameters_ap new_content in 
       ();;  
 
         
 
 
   end ;;   
   
   let copy fw ~destination ?(destbackupdir=Private.default_backup_dir) ?(destgab=false) summary=
      let _ = Private.frozen_copy fw ~destination ~destbackupdir ~destgab summary in 
      () ;;
 
   
