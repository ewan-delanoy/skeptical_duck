(*

#use"lib/Filewatching/gw_poly_t.ml";;

*)


type t = { 
   type_name : string ;
   dir_for_backup : Dfa_root_t.t ;
   encoding_protected_files : (Dfn_rootless_t.t * Dfn_rootless_t.t) list ;
   github_url : string ;
   gitpush_after_backup : bool ;
   ignored_files : Dfn_rootless_t.t list ;
   ignored_subdirectories : Dfa_subdirectory_t.t list ;
   index_for_caching : Fw_instance_index_t.t * Fw_state_index_t.t ;
   last_compilation_result_for_module : (Dfa_module_t.t * bool) list ;
   root : Dfa_root_t.t ;
   small_details_in_files : (Dfn_rootless_t.t * Fw_file_small_details_t.t) list ;
   subdirs_for_archived_mlx_files : Dfa_subdirectory_t.t list ;
   watched_files : (Dfn_rootless_t.t * string) list ;
} ;;