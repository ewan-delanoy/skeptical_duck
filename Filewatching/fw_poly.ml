(*

#use"Filewatching/fw_poly.ml";;

*)



module Private = struct 
let origin = {
   Fw_poly_t.type_name = "" ;
   dir_for_backup = Dfa_root.of_line "" ;
   encoding_protected_files = [] ;
   github_url = "" ;
   gitpush_after_backup = false ;
   ignored_files = [] ;
   ignored_subdirectories = [] ;
   index_for_caching = (Fw_instance_index_t.I(0),Fw_state_index_t.I(0)) ;
   last_compilation_result_for_module = [] ;
   root = Dfa_root.of_line "" ;
   small_details_in_files = [] ;
   subdirs_for_archived_mlx_files = [] ;
   watched_files = [] ;
} ;;
end;; 


let dir_for_backup x = x.Fw_poly_t.dir_for_backup ;;
let encoding_protected_files x = x.Fw_poly_t.encoding_protected_files ;;
let github_url x = x.Fw_poly_t.github_url ;;
let gitpush_after_backup x = x.Fw_poly_t.gitpush_after_backup ;;
let ignored_files x = x.Fw_poly_t.ignored_files ;;
let ignored_subdirectories x = x.Fw_poly_t.ignored_subdirectories ;;
let index_for_caching x = x.Fw_poly_t.index_for_caching ;;
let last_compilation_result_for_module x = x.Fw_poly_t.last_compilation_result_for_module ;;
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
let watched_files x = x.Fw_poly_t.watched_files ;;