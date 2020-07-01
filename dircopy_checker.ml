(*

#use"dircopy_checker.ml";;

Usable on a github clone of the remote master version.

*)

(*
type t ={
    ignored_endings : string list ;
    ignored_subdirs : Dfa_subdirectory_t.t list ;
    ignored_special_files : string list ;
    name_of_clone_directroy : string ; 
    clone_command : string ;

};;
*)  

exception Failure_in_clone_directory_creation;;
exception Failure_during_github_cloning;;

let is_admissible data s=
  (List.for_all (
     fun edg->not(Supstring.ends_with s ("."^edg))
  ) data.Dircopy_checker_t.ignored_endings)
  &&
   (List.for_all (
     fun beg->not(Supstring.begins_with s beg)
  ) (
    (Image.vorstellung Dfa_subdirectory.connectable_to_subpath
      data.Dircopy_checker_t.ignored_subdirs
    )
    )
   )  
  &&
  (
    not(
    List.mem s
    data.Dircopy_checker_t.ignored_special_files
    )
  )
  ;;

let check data root_dir=
  let name_of_clone_directory = data.Dircopy_checker_t.name_of_clone_directroy in 
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
  let rc1=List.filter (is_admissible data) (Dircopy_diff.recently_deleted diff)
  and rc2=List.filter (is_admissible data) (Dircopy_diff.recently_changed diff)
  and rc3=List.filter (is_admissible data) (Dircopy_diff.recently_created diff) in
  (rc1,rc2,rc3);;
          