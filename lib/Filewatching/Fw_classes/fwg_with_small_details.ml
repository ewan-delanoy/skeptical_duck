(*

#use"lib/Filewatching/Fw_classes/fwg_with_small_details.ml";;

*)

(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;
let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_archives" 
 } ;; 

let small_details_in_files fw = match fw.Fw_flattened_poly_t.small_details_in_files with Some x -> x | None -> raise(Get_exn "small_details_in_files")  ;;


let make fw_with_deps sdf = {
  fw_with_deps with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_small_details" ;
  small_details_in_files = Some sdf ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fwg_with_archives.t;
  small_details_in_files : ((Dfn_rootless_t.t * Fw_file_small_details_t.t) list);
} ;;

let parent fw = fw._parent ;; 
let small_details_in_files fw = fw.small_details_in_files ;; 

let make fw_with_deps sdf = {
  _parent = fw_with_deps ;
  small_details_in_files = sdf ;
} ;;

(* End of tight version of file watching *)


