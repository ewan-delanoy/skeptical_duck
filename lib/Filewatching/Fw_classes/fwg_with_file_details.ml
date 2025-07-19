(*

#use"lib/Filewatching/Fw_classes/fwg_with_file_details.ml";;

*)

(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;
let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_archives" 
 } ;; 

let file_details fw = match fw.Fw_flattened_poly_t.file_details with Some x -> x | None -> raise(Get_exn "file_details")  ;;


let make fw_with_deps sdf = {
  fw_with_deps with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_file_details" ;
  file_details = Some sdf ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fwc_with_archives.t;
  file_details : ((Dfn_rootless_t.t * Fw_file_details_t.t) list);
} ;;

let file_details fw = fw.file_details ;; 

let make fw_with_deps sdf = {
  _parent = fw_with_deps ;
  file_details = sdf ;
} ;;

let parent fw = fw._parent ;; 

(* End of tight version of file watching *)


 