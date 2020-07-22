(*

#use"check_ocaml_dircopy.ml";;

Usable on a github clone of the remote master version.

*)


exception Failure_in_clone_directory_creation;;
exception Failure_during_github_cloning;;


let filter_diff_according_to_admissibility data diff=
   let filter_list = List.filter (Fw_configuration.test_for_admissibility data) in 
   {
    Dircopy_diff_t.recently_deleted = filter_list diff.Dircopy_diff_t.recently_deleted;
    Dircopy_diff_t.recently_changed = filter_list diff.Dircopy_diff_t.recently_changed;
    Dircopy_diff_t.recently_created = filter_list diff.Dircopy_diff_t.recently_created;
  };;


let commands_for_confidentiality confidential_files =
   Image.image (
     fun (replacer,replacee) ->
       let s_replacer = Dfn_rootless.to_line  replacer in 
       let s_full_path = Fw_constant.clone_download_location^"/"^(Dfn_rootless.to_line replacee) in 
       Unix_command.prefix_for_reverse_replacing_patterns^s_replacer^" "^s_full_path
   ) confidential_files ;;


let check data =
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
    "git clone "^
    (data.Fw_configuration_t.github_url)^" "^
    name_of_clone_directory in 
  let j=Unix_command.uc full_clone_command in
  if j<>0
  then raise(Failure_during_github_cloning)
  else 
  let root_dir = data.Fw_configuration_t.root in 
  let diff1=Prepare_dircopy_update.compute_restricted_diff
     root_dir remotedir (data.Fw_configuration_t.ignored_subdirectories,
        (Image.image Dfn_rootless.to_line data.Fw_configuration_t.ignored_files) ) in
  filter_diff_according_to_admissibility  data diff1;;
          
