
(* 

#use"Compilation_management/update_compiler_copy.ml";;

*)


module Private = struct

let text_for_big_constants_file_in_next_world =
  String.concat "\n" [
    "\n(* "; 
    "#use\"Makefile_makers/coma_big_constant.ml\";;";
   "*)\n"; 
   "module This_World=struct\n";
   "let root=Root_directory.of_string \""^(Root_directory.without_trailing_slash Coma_big_constant.Next_World.root)^"\";;";
   "let backup_dir=Root_directory.of_string \""^(Root_directory.without_trailing_slash Coma_big_constant.Next_World.root)^"\";;";
   "let githubbing="^(string_of_bool Coma_big_constant.Next_World.githubbing)^";;";
   "let triple = (root,backup_dir,githubbing);;\n"; 
   "end;;";
   "module Next_World=struct\n";
   "let root=Root_directory.of_string \""^(Root_directory.without_trailing_slash Coma_big_constant.Third_World.root)^"\";;";
   "let backup_dir=Root_directory_t.R \""^(Root_directory.without_trailing_slash Coma_big_constant.Third_World.root)^"\";;";
   "let githubbing="^(string_of_bool Coma_big_constant.Third_World.githubbing)^";;";
   "let triple = (root,backup_dir,githubbing);;\n"; 
   "end;;";
   "module Third_World=struct\n";
   "let root=Root_directory.of_string \""^(Root_directory.without_trailing_slash Coma_big_constant.Third_World.root)^"\";;";
   "let backup_dir=Root_directory.of_string \""^(Root_directory.without_trailing_slash Coma_big_constant.Third_World.root)^"\";;";
   "let githubbing="^(string_of_bool Coma_big_constant.Third_World.githubbing)^";;";
   "let triple = (root,backup_dir,githubbing);;\n"; 
   "end;;";
   "\n\n\n"
   ];;

  let commands_for_copying cs =
    let sourcedir=Coma_state.root cs in 
    let l1=Coma_state.all_short_paths cs in
    let main_diff=Prepare_dircopy_update.compute_diff 
          (sourcedir,l1) Coma_big_constant.Next_World.root in
    Prepare_dircopy_update.commands_for_update 
     (sourcedir,Coma_big_constant.Next_World.root) main_diff;;
  
  let path_for_big_constants_in_next_world ()=
    Root_directory.join Coma_big_constant.Next_World.root Coma_constant.path_for_parametersfile;;
  
end ;;   
  
let ucc cs =
    let destdir=Coma_big_constant.Next_World.root in 
    let s_dir=Root_directory.connectable_to_subpath destdir in 
    let _=Image.image (
       fun subdir ->
        Unix_command.uc ("mkdir -p "^s_dir^(Subdirectory.without_trailing_slash subdir))
    ) [
        Coma_constant.build_subdir;
        Coma_constant.exec_build_subdir;
        Coma_constant.debug_build_subdir;
        Coma_constant.automatically_generated_subdir;
        Coma_constant.parameters_subdir;
      ] in
    (* remember to modify the special files AFTER copying every file ! *)
    let _=Image.image Unix_command.uc (Private.commands_for_copying cs) in 
    (* the mass copying just done includes the big constants file *)
    let bc_path=Absolute_path.of_string(Private.path_for_big_constants_in_next_world()) in
    let _=Io.overwrite_with bc_path Private.text_for_big_constants_file_in_next_world in
    let (other_cs,new_tgts2,preqt)=Coma_state.Target_system_creation.from_main_directory 
      Coma_big_constant.Next_World.root 
        Coma_big_constant.Next_World.backup_dir 
          Coma_big_constant.Next_World.githubbing in 
    let other_cs2=Coma_state.set_preq_types other_cs preqt in
    let _=Save_coma_state.save other_cs2 in
    (other_cs2,new_tgts2,preqt);;


         
