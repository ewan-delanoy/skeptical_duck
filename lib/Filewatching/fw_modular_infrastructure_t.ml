(*

#use"lib/Filewatching/fw_modular_infrastructure_t.ml";;

*)

type t = {
   modularized_details :  (Dfa_module_t.t * Fw_module_details_t.t) list ;
   order: (Dfa_module_t.t * (Dfa_module_t.t list * Dfa_module_t.t list)) list;
   needed_dirs :(Dfa_module_t.t * (Dfa_subdirectory_t.t list)) list;
   needed_libs : (Dfa_module_t.t * (Ocaml_library_t.t list)) list;
   all_subdirectories: Dfa_subdirectory_t.t list;
   all_printables : Dfn_middle_t.t list;
   registered_printers : (int * string) list ;
} ;;
 