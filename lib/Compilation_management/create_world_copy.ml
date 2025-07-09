(* 

#use"lib/Compilation_management/create_world_copy.ml";;

*)

module Private = struct

  let text_for_big_constants_file_in_other_world destination destbackupdir destgab =
    let ds = Particular_string.double_semicolon in 
    let proj_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash destination) '/'
    and git_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash destbackupdir) '/' in
    let proj_name2 = Cull_string.after_rightmost (Dfa_root.without_trailing_slash Coma_big_constant.Third_World.root) '/'
    and git_name2 = Cull_string.after_rightmost (Dfa_root.without_trailing_slash Coma_big_constant.Third_World.backup_dir) '/' in
    let passage_appearing_twice =
    [
      "let root=Dfa_root.of_line  (root_of_root^\""^proj_name2^"\") "^ds;
      "let backup_dir=Dfa_root.of_line (root_of_root^\""^git_name2^"\") "^ds;
      "let githubbing="^(string_of_bool Coma_big_constant.Third_World.githubbing)^ds;
      "let triple = (root,backup_dir,githubbing)"^ds^"\n"; 
    ] in        
    String.concat "\n" ([
      "\n(* "; 
      "#use\"Compilation_management/coma_big_constant.ml\""^ds;
     "*)\n"; 
     "let github_url = \""^(Coma_big_constant.github_url)^"\""^ds;
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
  
    let commands_for_copying cs rootlesses destination git_dir=
       let s_old_root=Dfa_root.connectable_to_subpath(Fw_final_poly.root cs) 
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
  
    
  
  let default_backup_dir=Coma_big_constant.Next_World.backup_dir;;

  let command_for_dune_untexting root =
    let s_root = Dfa_root.connectable_to_subpath root in 
    "mv "^s_root^"bin/dune.txt "^s_root^"bin/dune "^
    " && "^
    "mv "^s_root^"lib/dune.txt "^s_root^"lib/dune "^
    " && "^
    "mv "^s_root^"watched/dune.txt "^s_root^"watched/dune ";;

  let frozen_copy fw ~destination ?(destbackupdir=default_backup_dir) ?(destgab=false)  summary =
      let proj_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash destination) '/' in
      let (conv_files,needed_dirs) = (
        if Needed_data_summary.is_everything summary
        then (Coma_constant.conventional_files_with_full_content proj_name,
              Coma_constant.full_set_of_needed_dirs)
        else (Coma_constant.conventional_files_with_minimal_content proj_name,
              Coma_constant.minimal_set_of_needed_dirs)
      ) in 
      let _=Unix_again.delete_directory destination in 
      let old_dir = Sys.getcwd () in 
      let _ = Sys.chdir (Coma_big_constant.root_of_root) in 
      let _ = Unix_command.uc ("dune init proj "^proj_name) in 
      let _ = Sys.chdir old_dir in  
      let _=(Unix_again.create_subdirs_and_fill_files
      destination needed_dirs conv_files) in 
      let _ = Unix_command.uc (command_for_dune_untexting destination) in 
      let (modules_in_good_order,compilables,noncompilables) = 
          Needed_data_summary.expand fw summary in 
      let _=Image.image Unix_command.uc 
       (commands_for_copying fw (compilables@noncompilables) destination destbackupdir) in
       let parameters_ap = Absolute_path.of_string 
       (Dfn_common.recompose_potential_absolute_path destination 
        Coma_constant.rootless_path_for_parametersfile) 
      and new_content = text_for_big_constants_file_in_other_world destination destbackupdir destgab in
      let _=Io.overwrite_with parameters_ap new_content in  
      let faraway_config = Fwc_configuration.of_root destination in 
      let faraway_fw = Fwc_with_dependencies.of_configuration_and_list (faraway_config,compilables@noncompilables) in  
      (modules_in_good_order,faraway_fw);;  

  let fully_developed_copy cs ~destination ?(destbackupdir=default_backup_dir) ?(destgab=false) summary=
      let (_,faraway_fw) = frozen_copy cs ~destination ~destbackupdir ~destgab summary in 
      let faraway_cs1 = Fw_final_poly.of_fw_with_batch_compilation 
                          (Fwc_with_batch_compilation.of_fw_with_dependencies faraway_fw) 
                             destbackupdir destgab Coma_big_constant.github_url [] in 
      let all_modules = Fw_final_poly.dep_ordered_modules faraway_cs1 in 
      let faraway_cs2 = Fw_final_poly.modern_recompile faraway_cs1 all_modules in 
      let _= Fw_with_persisting.persist faraway_cs2 in   
      faraway_cs2;;                      
      

    let unfreeze_copy cs destroot =
        let old_fw_config = Fw_poly.to_fw_configuration cs in 
        let remote_fw_config = Fw_poly.set_root old_fw_config  destroot in   
        let remote_github_config = Fwg_github_configuration.make 
        ~v_root:destroot
        ~v_dir_for_backup:default_backup_dir
        ~v_gitpush_after_backup:false
        ~v_github_url:Coma_big_constant.github_url
        ~v_encoding_protected_files:[] in 
        let remote_cs = Fw_final_poly.of_fw_config_and_github_config remote_fw_config remote_github_config in 
        let _ = Fw_with_persisting.persist remote_cs in 
        remote_cs;;    
        


  end ;;   
  
  let frozen_copy cs ~destination ?(destbackupdir=Private.default_backup_dir) ?(destgab=false) summary=
     let _ = Private.frozen_copy cs ~destination ~destbackupdir ~destgab summary in 
     () ;;
  
  let fully_developed_copy = Private.fully_developed_copy ;; 
  
  let unfreeze_copy = Private.unfreeze_copy ;;
  
  
  