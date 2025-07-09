(*

#use"lib/Filewatching/Fw_classes/fwg_file_watcher.ml";;

*)

(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;
let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_configuration" 
 } ;; 

let watched_files fw = match fw.Fw_flattened_poly_t.watched_files with Some x -> x | None -> raise(Get_exn "watched_files")  ;;


let make fw_config w_files = {
  fw_config with 
  Fw_flattened_poly_t.type_name = Some "Fw_file_watcher" ;
  watched_files = Some w_files ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fwc_configuration.t;
  watched_files : (Dfn_rootless_t.t * string) list;
} ;;

let parent fw = fw._parent ;; 
let watched_files fw = fw.watched_files ;; 

let make fw_config w_files = {
  _parent = fw_config ;
  watched_files = w_files ;
} ;;

(* End of tight version of file watching *)


