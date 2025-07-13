(*

#use"lib/Filewatching/fw_flattened_poly_t.ml";;

*)

type t = { 
   type_name : string option;
   root : Dfa_root_t.t option;
   ignored_files : Dfn_rootless_t.t list option;
   ignored_subdirectories : Dfa_subdirectory_t.t list option;
   watched_files : (Dfn_rootless_t.t * string) list option;
   subdirs_for_archived_mlx_files : (Dfa_subdirectory_t.t list) option ;
   small_details_in_files : ((Dfn_rootless_t.t * Fw_file_small_details_t.t) list) option;
   dependencies : Fw_dependencies_t.t option;
   last_compilation_result_for_module : ((Dfa_module_t.t * bool) list) option ;
   dir_for_backup : Dfa_root_t.t option;
   gitpush_after_backup : bool option;
   github_url : string option;
   encoding_protected_files : ((Dfn_rootless_t.t * Dfn_rootless_t.t) list) option; 
} ;;


let origin = {
   type_name = None ;
   root = None ;
   ignored_files = None ;
   ignored_subdirectories = None ;
   watched_files = None ;
   subdirs_for_archived_mlx_files = None ;
   small_details_in_files = None ;
   dependencies = None ;
   last_compilation_result_for_module = None ;
   dir_for_backup = None ;
   encoding_protected_files = None ;
   github_url = None ;
   gitpush_after_backup = None ;  
} ;; 