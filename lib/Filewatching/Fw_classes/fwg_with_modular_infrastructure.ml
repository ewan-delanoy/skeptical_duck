(*

#use"lib/Filewatching/Fw_classes/fwg_with_modular_infrastructure.ml";;

*)

(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;
let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_small_details" 
 } ;; 

let dependencies fw = match fw.Fw_flattened_poly_t.dependencies with Some x -> x | None -> raise(Get_exn "dependencies")  ;;


let make fw_with_small_details deps = {
  fw_with_small_details with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_dependencies" ;
  dependencies = Some deps ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fwc_with_small_details.t;
  dependencies : Fw_modular_infrastructure_t.t ;
} ;;

let parent fw = fw._parent ;; 
let dependencies fw = fw.dependencies ;; 

let make fw_small_details deps = {
  _parent = fw_small_details ;
  dependencies = deps ;
} ;;

(* End of tight version of file watching *)


