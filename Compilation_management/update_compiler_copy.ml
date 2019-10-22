
(* 

#use"Compilation_management/update_compiler_copy.ml";;

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

  let commands_for_copying cs =
    let sourcedir=Coma_state.root cs in 
    let usual_rootless_paths=Coma_state.all_rootless_paths cs 
    and special_rootless_paths=Image.image Dfn_rootless.to_line Coma_constant.rootless_paths_needed_for_compiler_copy in
    let rootless_paths = usual_rootless_paths@ special_rootless_paths in 
    let main_diff=Prepare_dircopy_update.compute_diff 
          (sourcedir,rootless_paths) Coma_big_constant.Next_World.root in
    Prepare_dircopy_update.commands_for_update 
     (sourcedir,Coma_big_constant.Next_World.root) main_diff;;
  
  let path_for_big_constants_in_next_world ()=
    Dfn_join.root_to_rootless Coma_big_constant.Next_World.root Coma_constant.rootless_path_for_parametersfile;;
  
end ;;   
  

let ucc cs =
    let destdir=Coma_big_constant.Next_World.root in 
    let s_dir=Dfa_root.connectable_to_subpath destdir in 
    let _=Image.image (
       fun subdir ->
        Unix_command.uc ("mkdir -p "^s_dir^(Dfa_subdirectory.without_trailing_slash subdir))
    ) [
        Coma_constant.automatically_generated_subdir;
        Coma_constant.build_subdir;
        Coma_constant.debug_build_subdir;
        Coma_constant.exec_build_subdir;
        Coma_constant.parameters_subdir;
        Coma_constant.persistent_compilation_data_subdir;
      ] in
    (* remember to modify the special files AFTER copying every file ! *)
    let _=Image.image Unix_command.uc (Private.commands_for_copying cs) in 
    (* the mass copying just done includes the big constants file *)
    let bc_path=Dfn_full.to_absolute_path(Private.path_for_big_constants_in_next_world()) in
    let _=Io.overwrite_with bc_path Private.text_for_big_constants_file_in_next_world in
    let (other_cs,new_tgts2,preqt)=Coma_state.Target_system_creation.from_main_directory 
      Coma_big_constant.Next_World.root 
        Coma_big_constant.Next_World.backup_dir 
          Coma_big_constant.Next_World.githubbing in 
    let other_cs2=Coma_state.set_preq_types other_cs preqt in
    let _=Save_coma_state.save other_cs2 in
    (other_cs2,new_tgts2,preqt);;


         
