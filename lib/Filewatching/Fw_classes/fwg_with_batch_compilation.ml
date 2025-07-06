(* 

#use"lib/Filewatching/Fw_classes/fwg_with_batch_compilation.ml";;

*)


(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;
let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_dependencies" 
 } ;; 

let last_compilation_result_for_module fw = match fw.Fw_flattened_poly_t.last_compilation_result_for_module with Some x -> x | None -> raise(Get_exn "last_compilation_result_for_module")  ;;


let make fw_with_deps lcr = {
  fw_with_deps with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_batch_compilation" ;
  last_compilation_result_for_module = Some lcr ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fwg_with_dependencies.t;
  last_compilation_result_for_module : ((Dfa_module_t.t * bool) list);
} ;;

let parent fw = fw._parent ;; 
let last_compilation_result_for_module fw = fw.last_compilation_result_for_module ;; 

let make fw_with_deps lcr = {
  _parent = fw_with_deps ;
  last_compilation_result_for_module = lcr ;
} ;;

(* End of tight version of file watching *)


