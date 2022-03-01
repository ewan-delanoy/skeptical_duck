(*

#use"Filewatching/fw_poly.ml";;

*)


module Private = struct 
module Crobj = struct 
let salt = "Fw_poly_t." ;;
let label_for_type_name                          = salt ^ "type_name" ;;
let label_for_dir_for_backup                     = salt ^ "dir_for_backup" ;;
let label_for_encoding_protected_files           = salt ^ "encoding_protected_files" ;;
let label_for_github_url                         = salt ^ "github_url" ;;
let label_for_gitpush_after_backup               = salt ^ "gitpush_after_backup" ;;
let label_for_ignored_files                      = salt ^ "ignored_files" ;;
let label_for_ignored_subdirectories             = salt ^ "ignored_subdirectories" ;;
let label_for_last_compilation_result_for_module = salt ^ "last_compilation_result_for_module" ;;
let label_for_root                               = salt ^ "root" ;;
let label_for_small_details_in_files             = salt ^ "small_details_in_files" ;;
let label_for_subdirs_for_archived_mlx_files     = salt ^ "subdirs_for_archived_mlx_files" ;;
let label_for_watched_files                      = salt ^ "watched_files" ;;

let of_concrete_object ccrt_obj = 
 let g=Concrete_object.get_record ccrt_obj in 
 {
   Fw_poly_t.type_name = Crobj_converter.string_of_concrete_object (g label_for_type_name) ;
   dir_for_backup = Dfa_root.of_concrete_object (g label_for_dir_for_backup)  ;
   encoding_protected_files = Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object (g label_for_encoding_protected_files)  ;
   github_url = Crobj_converter.string_of_concrete_object (g label_for_github_url)  ;
   gitpush_after_backup = Crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup)  ;
   ignored_files = Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object (g label_for_ignored_files)  ;
   ignored_subdirectories = Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object (g label_for_ignored_subdirectories)  ;
   index_for_caching = (Fw_instance_index_t.I(0),Fw_state_index_t.I(0)) ;
   last_compilation_result_for_module = Crobj_converter_combinator.to_pair_list Dfa_module.of_concrete_object Crobj_converter.bool_of_concrete_object (g label_for_last_compilation_result_for_module)  ;
   root = Dfa_root.of_concrete_object (g label_for_root)  ;
   small_details_in_files = Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Fw_file_small_details.of_concrete_object (g label_for_small_details_in_files)  ;
   subdirs_for_archived_mlx_files = Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object (g label_for_subdirs_for_archived_mlx_files)  ;
   watched_files = Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Crobj_converter.string_of_concrete_object (g label_for_watched_files)  ;
} ;;

let to_concrete_object fw = 
 let items =  
 [
     label_for_dir_for_backup, Dfa_root.to_concrete_object fw.Fw_poly_t.dir_for_backup ;
     label_for_encoding_protected_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object fw.Fw_poly_t.encoding_protected_files ;
     label_for_github_url, Crobj_converter.string_to_concrete_object fw.Fw_poly_t.github_url ;
     label_for_gitpush_after_backup, Crobj_converter.bool_to_concrete_object fw.Fw_poly_t.gitpush_after_backup ;
     label_for_ignored_files, Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object fw.Fw_poly_t.ignored_files ;
     label_for_ignored_subdirectories, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object fw.Fw_poly_t.ignored_subdirectories ;
     label_for_last_compilation_result_for_module, Crobj_converter_combinator.of_pair_list Dfa_module.to_concrete_object Crobj_converter.bool_to_concrete_object fw.Fw_poly_t.last_compilation_result_for_module ;
     label_for_root, Dfa_root.to_concrete_object fw.Fw_poly_t.root ;
     label_for_small_details_in_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Fw_file_small_details.to_concrete_object fw.Fw_poly_t.small_details_in_files ;
     label_for_subdirs_for_archived_mlx_files, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object fw.Fw_poly_t.subdirs_for_archived_mlx_files ;
     label_for_watched_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Crobj_converter.string_to_concrete_object fw.Fw_poly_t.watched_files ;
 ] in 
 Concrete_object_t.Record items ;;


end;; 


let origin = {
   Fw_poly_t.type_name = "" ;
   dir_for_backup = Dfa_root.of_line "dummy" ;
   encoding_protected_files = [] ;
   github_url = "" ;
   gitpush_after_backup = false ;
   ignored_files = [] ;
   ignored_subdirectories = [] ;
   index_for_caching = (Fw_instance_index_t.I(0),Fw_state_index_t.I(0)) ;
   last_compilation_result_for_module = [] ;
   root = Dfa_root.of_line "dummy" ;
   small_details_in_files = [] ;
   subdirs_for_archived_mlx_files = [] ;
   watched_files = [] ;
} ;;
end;; 


let construct_fw_configuration ~root:v1_r ~ignored_subdirectories:v2_ign_subdirs ~ignored_files:v3_ign_files = {
   Private.origin with 
   Fw_poly_t.type_name = "Fw_configuration" ;
   root = v1_r ;
   ignored_subdirectories = v2_ign_subdirs ;
   ignored_files = v3_ign_files ;
} ;;
let dir_for_backup x = x.Fw_poly_t.dir_for_backup ;;
let encoding_protected_files x = x.Fw_poly_t.encoding_protected_files ;;
let extend_fw_with_batch_compilation_to_fw_with_githubbing fw ~dir_for_backup:v1_backup_dir ~gitpush_after_backup:v2_gab ~github_url:v3_url ~encoding_protected_files:v4_protected_pairs = {
   fw with 
   Fw_poly_t.type_name = "Fw_with_githubbing" ;
   dir_for_backup = v1_backup_dir ;
   gitpush_after_backup = v2_gab ;
   github_url = v3_url ;
   encoding_protected_files = v4_protected_pairs ;
} ;;
let github_url x = x.Fw_poly_t.github_url ;;
let gitpush_after_backup x = x.Fw_poly_t.gitpush_after_backup ;;
let ignored_files x = x.Fw_poly_t.ignored_files ;;
let ignored_subdirectories x = x.Fw_poly_t.ignored_subdirectories ;;
let index_for_caching x = x.Fw_poly_t.index_for_caching ;;
let last_compilation_result_for_module x = x.Fw_poly_t.last_compilation_result_for_module ;;
let of_concrete_object = Private.Crobj.of_concrete_object ;;
let print_out (fmt:Format.formatter) fw  = Format.fprintf fmt "@[%s@]" (fw.Fw_poly_t.type_name) ;;
let restrict_fw_with_githubbing_to_github_configuration fw  = {
   fw with 
   Fw_poly_t.type_name = "Github_configuration" ;
} ;;
let root x = x.Fw_poly_t.root ;;
let set_dir_for_backup x backup_dir = { x with Fw_poly_t.dir_for_backup = backup_dir} ;;
let set_encoding_protected_files x protected_pairs = { x with Fw_poly_t.encoding_protected_files = protected_pairs} ;;
let set_github_url x url = { x with Fw_poly_t.github_url = url} ;;
let set_gitpush_after_backup x gab = { x with Fw_poly_t.gitpush_after_backup = gab} ;;
let set_ignored_files x ign_files = { x with Fw_poly_t.ignored_files = ign_files} ;;
let set_ignored_subdirectories x ign_subdirs = { x with Fw_poly_t.ignored_subdirectories = ign_subdirs} ;;
let set_index_for_caching x cache_idx = { x with Fw_poly_t.index_for_caching = cache_idx} ;;
let set_last_compilation_result_for_module x compilation_results = { x with Fw_poly_t.last_compilation_result_for_module = compilation_results} ;;
let set_root x r = { x with Fw_poly_t.root = r} ;;
let set_small_details_in_files x small_details = { x with Fw_poly_t.small_details_in_files = small_details} ;;
let set_subdirs_for_archived_mlx_files x archives_subdirs = { x with Fw_poly_t.subdirs_for_archived_mlx_files = archives_subdirs} ;;
let set_watched_files x files = { x with Fw_poly_t.watched_files = files} ;;
let small_details_in_files x = x.Fw_poly_t.small_details_in_files ;;
let subdirs_for_archived_mlx_files x = x.Fw_poly_t.subdirs_for_archived_mlx_files ;;
let to_concrete_object = Private.Crobj.to_concrete_object ;;
let watched_files x = x.Fw_poly_t.watched_files ;;