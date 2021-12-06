(* 

#use"Compilation_management/fw_with_batch_compilation_t.ml";;

*)


type t={
     frontier_with_unix_world : Fw_with_dependencies_t.t;
     last_compilation_result_for_module : (Dfa_module_t.t * bool) list;
};;
