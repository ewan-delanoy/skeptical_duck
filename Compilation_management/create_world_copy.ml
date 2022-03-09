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
       let s_old_root=Dfa_root.connectable_to_subpath(Fw_poly.root cs) 
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
      let all_modules = Fw_with_dependencies.dep_ordered_modules faraway_cs1 in 
      let faraway_cs2 = Fw_with_githubbing.modern_recompile faraway_cs1 all_modules in 
      let _=Fw_with_persisting.persist faraway_cs2 in   
      faraway_cs2;;                      
      

    let unfreeze_copy cs destroot =
        let old_fw_config = Fw_with_githubbing.to_fw_configuration cs in 
        let remote_fw_config = Fw_poly.set_root old_fw_config  destroot in   
        let remote_github_config = Fw_poly.construct_github_configuration 
        ~root:destroot
        ~dir_for_backup:default_backup_dir
        ~gitpush_after_backup:false
        ~github_url:Coma_big_constant.github_url
        ~encoding_protected_files:[] in 
        let remote_cs = Fw_with_githubbing.of_fw_config_and_github_config remote_fw_config remote_github_config in 
        let _ = Fw_with_persisting.persist remote_cs in 
        remote_cs;;    
        


  end ;;   
  
  let frozen_copy cs ~destination ?(destbackupdir=Private.default_backup_dir) ?(destgab=false) summary=
     let _ = Private.frozen_copy cs ~destination ~destbackupdir ~destgab summary in 
     () ;;
  
  let fully_developed_copy = Private.fully_developed_copy ;; 
  
  let unfreeze_copy = Private.unfreeze_copy ;;
  
  
  