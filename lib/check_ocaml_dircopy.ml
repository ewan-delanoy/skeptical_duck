(*

#use"lib/check_ocaml_dircopy.ml";;

Usable on a github clone of the remote master version.

*)


exception Failure_in_clone_directory_creation;;
exception Failure_during_github_cloning;;

module Private = struct
let clone_download_location = (Sys.getenv "HOME")^"/Downloads/Clone";;

let filter_diff_according_to_admissibility data diff=
   let filter_list = List.filter (Fwc_configuration.test_for_admissibility data) in 
   {
    Dircopy_diff_t.recently_deleted = filter_list diff.Dircopy_diff_t.recently_deleted;
    Dircopy_diff_t.recently_changed = filter_list diff.Dircopy_diff_t.recently_changed;
    Dircopy_diff_t.recently_created = filter_list diff.Dircopy_diff_t.recently_created;
  };;


let commands_for_confidentiality encoding_protected_files =
   Image.image (
     fun (replacer,replacee) ->
       let s_replacer = Dfn_rootless.to_line  replacer in 
       let s_full_path = clone_download_location^"/"^(Dfn_rootless.to_line replacee) in 
       Unix_command.prefix_for_reverse_replacing_patterns^s_replacer^" "^s_full_path
   ) encoding_protected_files ;;

let ref_for_forgotten_changed_files = ref ([]: Absolute_path.t list) ;;

let store_changed_files root diff =
  let temp1 = diff.Dircopy_diff_t.recently_changed in 
  let temp2 = Image.image (
   fun rl -> Dfn_full.to_absolute_path
   (Dfn_join.root_to_rootless root rl)
  ) temp1 in 
  ref_for_forgotten_changed_files:=temp2 ;;

let modify_forgotten_files_artificially () = 
  List.iter (
     fun ap ->Io.append_string_to_file " " ap
  )(!ref_for_forgotten_changed_files) ;;

let check fw_data github_data =
  let name_of_clone_directory = clone_download_location in 
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
    "git clone "^
    (Fwc_github_configuration.github_url github_data)^" "^
    name_of_clone_directory in 
  let j=Unix_command.uc full_clone_command in
  if j<>0
  then raise(Failure_during_github_cloning)
  else 
  let cmds = commands_for_confidentiality (Fwc_github_configuration.encoding_protected_files github_data) in 
  let _= Unix_command.conditional_multiple_uc cmds in 
  let root_dir = Fwc_github_configuration.root github_data in 
  let diff1=Prepare_dircopy_update.compute_restricted_diff
     root_dir remotedir (Fwc_configuration.ignored_subdirectories fw_data,
        (Image.image Dfn_rootless.to_line (Fwc_configuration.ignored_files fw_data) )) in
  let diff2 =filter_diff_according_to_admissibility fw_data diff1 in 
  let _ = store_changed_files root_dir diff2 in 
  diff2 ;;
          
end ;;

let check = Private.check ;;

let modify_forgotten_files_artificially = Private.modify_forgotten_files_artificially ;; 