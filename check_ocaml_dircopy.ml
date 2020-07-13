(*

#use"check_ocaml_dircopy.ml";;

Usable on a github clone of the remote master version.

*)

let instance ={
    Dircopy_checker_t.ignored_endings = 
     ["depend";"ocamlinit";"cmi";"cmo";"DS_Store";"txt";"php";"js";
      "ocaml_made";"ocaml_debuggable"] ;
    ignored_subdirs =  [
       Coma_constant.abandoned_ideas_subdir;
       Coma_constant.automatically_generated_subdir;
       Coma_constant.build_subdir;
       Coma_constant.githubbed_archive_subdir;
       Coma_constant.persistent_compilation_data_subdir;
       Coma_constant.temporary_subdir;
       Dfa_subdirectory.of_line ".vscode/";
     ] ;
    ignored_special_files =  ["README";"makefile";"debugged.ml";".merlin"];
    name_of_clone_directroy = "/Users/ewandelanoy/Downloads/Clone" ; 
    clone_command = "git clone https://github.com/ewan-delanoy/skeptical_duck " ;

};;
  
let check = Dircopy_checker.check instance ;;


exception Failure_in_clone_directory_creation;;
exception Failure_during_github_cloning;;

let is_admissible data rl=
  (List.mem (
    (Dfn_rootless.to_ending rl)
  ) Dfa_ending.endings_for_readable_files)
  &&
   (List.for_all (
     fun sd->not(Dfn_rootless.is_in rl sd)
  ) data.Fw_configuration_t.ignored_subdirectories
  )  
  &&
  (
    not(List.mem rl data.Fw_configuration_t.ignored_files)
  )
  ;;

let filter_according_to_admissibility data l_rl=
   Option.filter_and_unpack (
    fun rl -> 
    let s = Dfn_rootless.to_line rl in 
    if is_admissible data rl
    then Some s 
    else None
   ) l_rl;; 
  

(*

let check data root_dir=
  let name_of_clone_directory = Fw_constant.clone_download_location in 
  let i=(
    if Sys.file_exists(name_of_clone_directory)
    then Unix_command.uc("rm -rf "^name_of_clone_directory) 
    else 0
  ) in
  if i<>0
  then raise(Failure_in_clone_directory_creation)
  else 
  let _=Unix_command.uc("mkdir -p "^name_of_clone_directory) in
  let remotedir=Dfa_root.of_line name_of_clone_directory in
  let full_clone_command=
   (data.Dircopy_checker_t.clone_command)^name_of_clone_directory in 
  let j=Unix_command.uc full_clone_command in
  if j<>0
  then raise(Failure_during_github_cloning)
  else 
  let diff=Prepare_dircopy_update.compute_restricted_diff
     root_dir remotedir (Coma_constant.git_ignored_subdirectories,
        data.Dircopy_checker_t.ignored_special_files ) in
  let rc1=filter_according_to_admissibility  data (Dircopy_diff.recently_deleted diff)
  and rc2=filter_according_to_admissibility  data (Dircopy_diff.recently_changed diff)
  and rc3=filter_according_to_admissibility  data (Dircopy_diff.recently_created diff) in
  (rc1,rc2,rc3);;
          
*)