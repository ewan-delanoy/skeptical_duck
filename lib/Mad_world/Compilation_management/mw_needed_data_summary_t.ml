(* 

#use"lib/Mad_world/Compilation_management/mw_needed_data_summary_t.ml";;

Used to describe what data you want in a copied structure equipped with compilation management.

*)


type t=
    Everything 
   | Selection of Dfa_module_t.t list * Dfa_subdirectory_t.t list ;;
