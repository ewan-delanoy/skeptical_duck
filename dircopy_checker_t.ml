(*

#use"dircopy_checker_t.ml";;

Usable on a github clone of the remote master version.

*)

type t ={
    ignored_endings : string list ;
    ignored_subdirs : Dfa_subdirectory_t.t list ;
    ignored_special_files : string list ;
    name_of_clone_directroy : string ; 
    clone_command : string ;

};;
  
(*  
let name_of_clone_directory="/Users/ewandelanoy/Downloads/Clone";;  
let github_clone_command=
"git clone https://github.com/ewan-delanoy/skeptical_duck "^
name_of_clone_directory;;  

exception Failure_in_clone_directory_creation;;
exception Failure_during_github_cloning;;

let check root_dir=
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
  let j=Unix_command.uc github_clone_command in
  if j<>0
  then raise(Failure_during_github_cloning)
  else 
  let diff=Prepare_dircopy_update.compute_restricted_diff
     root_dir remotedir (Coma_constant.git_ignored_subdirectories,["README";"makefile";"debugged.ml"]) in
  let rc1=List.filter is_admissible (Dircopy_diff.recently_deleted diff)
  and rc2=List.filter is_admissible (Dircopy_diff.recently_changed diff)
  and rc3=List.filter is_admissible (Dircopy_diff.recently_created diff) in
  (rc1,rc2,rc3);;
*)           