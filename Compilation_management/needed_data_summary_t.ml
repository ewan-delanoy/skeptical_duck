
(* 

#use"Compilation_management/needed_data_summary_t.ml";;

*)


type t=
    Everything 
   | Selection of Dfa_module_t.t list * Dfa_subdirectory_t.t list ;;
