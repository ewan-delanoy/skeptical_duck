(* 

#use"Compilation_management/create_world_copy.ml";;

*)

module Private = struct

  let text_for_big_constants_file_in_other_world destination destbackupdir destgab =
    let ds = Particular_string.double_semicolon in 
    String.concat "\n" [
      "\n(* "; 
      "#use\"Compilation_management/coma_big_constant.ml\""^ds;
     "*)\n"; 
     "let github_url = \""^(Coma_big_constant.github_url)^"\""^ds;
     "module This_World=struct\n";
     "let root=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash destination)^"\""^ds;
     "let backup_dir=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash destbackupdir)^"\""^ds;
     "let githubbing="^(string_of_bool destgab)^ds;
     "let triple = (root,backup_dir,githubbing)"^ds^"\n"; 
     "end"^ds;
     "module Next_World=struct\n";
     "let root=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash Coma_big_constant.Third_World.root)^"\""^ds;
     "let backup_dir=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash Coma_big_constant.Third_World.backup_dir)^"\""^ds;
     "let githubbing="^(string_of_bool Coma_big_constant.Third_World.githubbing)^ds;
     "let triple = (root,backup_dir,githubbing)"^ds^"\n"; 
     "end"^ds;
     "module Third_World=struct\n";
     "let root=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash Coma_big_constant.Third_World.root)^"\""^ds;
     "let backup_dir=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash Coma_big_constant.Third_World.backup_dir)^"\""^ds;
     "let githubbing="^(string_of_bool Coma_big_constant.Third_World.githubbing)^ds;
     "let triple = (root,backup_dir,githubbing)"^ds^"\n"; 
     "end"^ds;
     "\n\n\n"
     ];;
  
    let commands_for_copying cs rootlesses destination=
       let s_old_root=Dfa_root.connectable_to_subpath(Fw_with_githubbing.root cs) 
       and s_new_root=Dfa_root.connectable_to_subpath destination in 
       let unordered_subdirs = Image.image Dfn_rootless.to_subdirectory rootlesses in  
       let needed_subdirs = Ordered.sort Total_ordering.standard unordered_subdirs in 
       let dir_commands = Image.image (
         fun subdir ->"mkdir -p "^s_new_root^(Dfa_subdirectory.without_trailing_slash subdir)
       ) needed_subdirs 
       and file_commands = Image.image (fun rootless->
           let line = Dfn_rootless.to_line rootless in 
           "cp "^s_old_root^line^" "^s_new_root^(Cull_string.before_rightmost line '/')
      ) rootlesses in 
      dir_commands @ file_commands;;
  
    
  
  let default_backup_dir=Coma_big_constant.Next_World.backup_dir;;

  let frozen_copy cs ~destination ?(destbackupdir=default_backup_dir) ?(destgab=false)  summary =
      let (conv_files,needed_dirs) = (
        if Needed_data_summary.is_everything summary
        then (Coma_constant.conventional_files_with_full_content,
              Coma_constant.full_set_of_needed_dirs)
        else (Coma_constant.conventional_files_with_minimal_content,
              Coma_constant.minimal_set_of_needed_dirs)
      ) in 
      let _=More_unix.clear_directory_contents destination in 

      let _=(More_unix.create_subdirs_and_fill_files
      destination needed_dirs conv_files) in 
      let (modules_in_good_order,compilables,noncompilables) = 
          Needed_data_summary.expand cs summary in 
      let _=Image.image Unix_command.uc 
       (commands_for_copying cs (compilables@noncompilables) destination) in
      let faraway_config = Fw_configuration.of_root destination in 
      let faraway_fw1 = Fw_with_dependencies.of_configuration_and_list (faraway_config,compilables@noncompilables) in  
      let (faraway_fw,_) =Fw_with_dependencies.overwrite_file_if_it_exists faraway_fw1 
                     (Coma_constant.rootless_path_for_parametersfile, 
                       text_for_big_constants_file_in_other_world destination destbackupdir destgab) in 
      (modules_in_good_order,faraway_fw);; 

  let fully_developed_copy cs ~destination ?(destbackupdir=default_backup_dir) ?(destgab=false) summary=
      let (modules_in_good_order,faraway_fw) = frozen_copy cs ~destination ~destbackupdir ~destgab summary in 
      let faraway_cs1 = Fw_with_githubbing.of_fw_with_batch_compilation 
                          (Fw_with_batch_compilation.of_fw_with_dependencies faraway_fw) 
                             destbackupdir destgab Coma_big_constant.github_url [] in 
      let all_modules = Fw_with_githubbing.dep_ordered_modules faraway_cs1 in 
      let faraway_cs2 = Fw_with_githubbing.modern_recompile faraway_cs1 all_modules in 
      let _=Fw_with_persisting.save faraway_cs2 in   
      faraway_cs2;;                      
      

    let unfreeze_copy cs destroot =
        let old_config = Fw_with_githubbing.configuration cs in 
        let remote_config = {
           old_config with 
           Fw_configuration_t.root = destroot ;
        } in 
        let remote_cs = Fw_with_githubbing.empty_one remote_config default_backup_dir false Coma_big_constant.github_url [] in 
        let remote_cs2 =
           Fw_with_githubbing.of_configuration 
            (Fw_with_githubbing.configuration remote_cs) 
              (remote_cs.Fw_with_githubbing_t.dir_for_backup) 
                (remote_cs.Fw_with_githubbing_t.gitpush_after_backup) 
                  (remote_cs.Fw_with_githubbing_t.github_url)
                  (remote_cs.Fw_with_githubbing_t.encoding_protected_files) in 
        let _ = Fw_with_persisting.save remote_cs2 in 
        remote_cs2;;    
        


  end ;;   
  
  let frozen_copy cs ~destination ?(destbackupdir=Private.default_backup_dir) ?(destgab=false) summary=
     let _ = Private.frozen_copy cs ~destination ~destbackupdir ~destgab summary in 
     () ;;
  
  let fully_developed_copy = Private.fully_developed_copy ;; 
  
  let unfreeze_copy = Private.unfreeze_copy ;;
  
  
  