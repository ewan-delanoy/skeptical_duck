
(* 

#use"Compilation_management/coma_state_t.ml";;

*)

type t={
     root : Dfa_root_t.t;
     dir_for_backup : Dfa_root_t.t;
     push_after_backup : bool;
     modules : Dfa_module_t.t Small_array.t ;
     subdir_for_module : (Dfa_module_t.t * Dfa_subdirectory_t.t ) list;
     principal_ending_for_module : (Dfa_module_t.t * Dfa_ending_t.t ) list;
     mli_presence_for_module : (Dfa_module_t.t * bool ) list;
     principal_mt_for_module : (Dfa_module_t.t * string ) list;
     mli_mt_for_module : (Dfa_module_t.t * string ) list;
     needed_libs_for_module : (Dfa_module_t.t * Ocaml_library.t list ) list;
     direct_fathers_for_module : (Dfa_module_t.t * Dfa_module_t.t list ) list;
     ancestors_for_module : (Dfa_module_t.t * Dfa_module_t.t list ) list; 
     needed_dirs_for_module : (Dfa_module_t.t * (Dfa_subdirectory_t.t list)) list;
     product_up_to_date_for_module : (Dfa_module_t.t * bool) list;
     directories : Dfa_subdirectory_t.t list;
     printer_equipped_types : (Dfn_endingless_t.t*bool) list;
};;
