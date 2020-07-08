
(* 

#use"Compilation_management/create_compiler_copy.ml";;

*)


module Private = struct

let text_for_big_constants_file_in_next_world =
  let ds = Double_semicolon.ds in 
  String.concat "\n" [
    "\n(* "; 
    "#use\"Compilation_management/coma_big_constant.ml\""^ds;
   "*)\n"; 
   "module This_World=struct\n";
   "let root=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash Coma_big_constant.Next_World.root)^"\""^ds;
   "let backup_dir=Dfa_root.of_line \""^(Dfa_root.without_trailing_slash Coma_big_constant.Next_World.backup_dir)^"\""^ds;
   "let githubbing="^(string_of_bool Coma_big_constant.Next_World.githubbing)^ds;
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

  let rootlesses_coming_from_modules cs needed_modules = 
          let modules_above=Image.image (fun nm->
             Coma_state.above cs 
             (Coma_state.endingless_at_module cs nm)
          ) needed_modules  in 
          let all_elesses = Coma_state.all_modules cs in 
          let modules_in_good_order = Option.filter_and_unpack 
              (fun eless->
                if List.exists(
                  fun l->List.mem (Dfn_endingless.to_module eless) l
              )(needed_modules::modules_above) 
              then Some(Dfn_endingless.to_module eless)
              else None)
          all_elesses in 
          let collected_acolytes=List.flatten 
            (Image.image (Coma_state.acolytes_at_module cs) 
              modules_in_good_order) in 
          (modules_in_good_order,Image.image Dfn_full.to_rootless collected_acolytes);;
  
     

  let rootlesses_to_be_copied cs opt_selection =
     let fw = cs.Coma_state_t.frontier_with_unix_world in 
     match opt_selection with 
     None -> let sr = Image.image (fun (rootless,_)->rootless) in 
             ( 
               Coma_state.ordered_list_of_modules cs,
               sr (fw.Fw_wrapper_t.compilable_files),
               sr (fw.Fw_wrapper_t.noncompilable_files) )
    |Some(needed_modules,imposed_subdirs)-> 
          let selector = Option.filter_and_unpack(
             fun (rootless,_)->
               if List.mem( Dfn_rootless.to_subdirectory(rootless) ) imposed_subdirs 
               then Some(rootless)
               else None
          ) in   
          let compilables= selector (fw.Fw_wrapper_t.compilable_files)
          and noncompilables= selector (fw.Fw_wrapper_t.noncompilable_files) in 
          let all_needed_modules= (Image.image Dfn_rootless.to_module compilables) 
                           @ needed_modules in 
          let (modules_in_good_order,all_nonspecials)=rootlesses_coming_from_modules cs all_needed_modules in 
          (modules_in_good_order,all_nonspecials,noncompilables);;

  let commands_for_copying cs rootlesses=
     let s_old_root=Dfa_root.connectable_to_subpath(Coma_state_field.root cs) 
     and s_new_root=Dfa_root.connectable_to_subpath(Coma_big_constant.Next_World.root) in 
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

  
  let path_for_big_constants_in_next_world =
    Dfn_join.root_to_rootless 
      Coma_big_constant.Next_World.root 
        Coma_constant.rootless_path_for_parametersfile;;
  

end ;;   
  


let cwc cs opt_selection=
    let (destdir,destbackupdir,destgab)=Coma_big_constant.Next_World.triple in 
    let conv_files = (
      match opt_selection with 
      None -> Coma_constant.conventional_files_with_usual_content
      |Some(_)-> Coma_constant.conventional_files_with_minimal_content
    ) in 
    let _=More_unix.clear_directory_contents destdir in 
    let _=(More_unix.create_subdirs_and_fill_files
       destdir
      Coma_constant.git_ignored_subdirectories 
        conv_files) in 
    let (modules_in_good_order,compilables,noncompoilables) = 
        Private.rootlesses_to_be_copied cs opt_selection in 
    let _=Image.image Unix_command.uc 
     (Private.commands_for_copying cs (compilables@noncompoilables)) in
    let faraway_config = Fw_configuration.constructor (destdir,destbackupdir,destgab) in 
    let faraway_fw1 = Fw_initialize.second_init faraway_config (compilables,noncompoilables) in  
    let faraway_fw = Fw_wrapper.overwrite_nonspecial_file_if_it_exists faraway_fw1 
                   Coma_constant.rootless_path_for_parametersfile 
                     Private.text_for_big_constants_file_in_next_world in 
    let restricted_cs=(match opt_selection with 
        None -> cs 
        |Some(_)->Coma_state_field.restrict cs modules_in_good_order
    ) in 
    let faraway_cs1 = Coma_state_field.transplant 
       restricted_cs faraway_fw in 
    let faraway_cs = Coma_state.update_just_one_module faraway_cs1  Coma_constant.rootless_path_for_parametersfile in   
    let faraway_cs2 = Modify_coma_state.Internal.recompile (faraway_cs,[]) in 
    let _=Save_coma_state.save faraway_cs2 in   
    faraway_cs2;;


