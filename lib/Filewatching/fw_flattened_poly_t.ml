(*

#use"lib/Filewatching/fw_flattened_poly_t.ml";;

*)

type t = { 
   type_name : string option;
   root : Dfa_root_t.t option;
   ignored_subdirectories : Dfa_subdirectory_t.t list option;
   ignored_files : Dfn_rootless_t.t list option;
   watched_files : (Dfn_rootless_t.t * string) list option;
   subdirs_for_archived_mlx_files : (Dfa_subdirectory_t.t list) option ;
   small_details_in_files : ((Dfn_rootless_t.t * Fw_file_small_details_t.t) list) option;
   index_for_caching : (Fw_instance_index_t.t * Fw_state_index_t.t) option;
   last_compilation_result_for_module : ((Dfa_module_t.t * bool) list) option ;
   dir_for_backup : Dfa_root_t.t option;
   gitpush_after_backup : bool option;
   github_url : string option;
   encoding_protected_files : ((Dfn_rootless_t.t * Dfn_rootless_t.t) list) option; 
} ;;

let origin = {
   type_name = Some "" ;
   ignored_files = Some [] ;
   ignored_subdirectories = Some [] ;
   index_for_caching = Some (Fw_indexer.make_full_instance ()) ;
   root = Some (Dfa_root.of_line "dummy") ;
   small_details_in_files = Some [] ;
   subdirs_for_archived_mlx_files = Some [] ;
   watched_files = Some [] ;

   last_compilation_result_for_module = None ;
   dir_for_backup = None ;
   encoding_protected_files = None ;
   github_url = None ;
   gitpush_after_backup = None ;  
} ;;