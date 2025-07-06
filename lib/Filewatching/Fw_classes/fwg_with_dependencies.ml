(*

#use"lib/Filewatching/Fw_classes/fwg_with_dependencies.ml";;

*)

(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;
let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_archives" 
 } ;; 

let index_for_caching fw = match fw.Fw_flattened_poly_t.index_for_caching with Some x -> x | None -> raise(Get_exn "index_for_caching")  ;;


let make fw_with_archives ifc = {
  fw_with_archives with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_dependencies" ;
  index_for_caching = Some ifc ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fw_flattened_poly_t.t;
  index_for_caching : (Fw_instance_index_t.t * Fw_state_index_t.t);

} ;;

let parent fw = fw._parent ;; 
let index_for_caching fw = fw.index_for_caching ;; 

let make fw_archives ifc = {
  _parent = fw_archives ;
  index_for_caching = ifc ;
} ;;

(* End of tight version of file watching *)


