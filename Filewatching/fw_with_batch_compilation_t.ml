(* 

#use"Filewatching/fw_with_batch_compilation_t.ml";;

*)


type t={
     parent : Fw_poly_t.t;
     last_compilation_result_for_module : (Dfa_module_t.t * bool) list;
};;
