(*

#use"lib/Filewatching/Fw_classes/fwg_with_archives.ml";;

*)
(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;
let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_file_watcher" 
 } ;; 

let subdirs_for_archived_mlx_files fw = match fw.Fw_flattened_poly_t.subdirs_for_archived_mlx_files with Some x -> x | None -> raise(Get_exn "subdirs_for_archived_mlx_files")  ;;


let make fw_watcher subdirs = {
  fw_watcher with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_archives" ;
  subdirs_for_archived_mlx_files = Some subdirs ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fwg_file_watcher.t;
  subdirs_for_archived_mlx_files : (Dfa_subdirectory_t.t list);
} ;;

let parent fw = fw._parent ;; 
let subdirs_for_archived_mlx_files fw = fw.subdirs_for_archived_mlx_files ;; 

let make fw_watcher subdirs = {
  _parent = fw_watcher ;
  subdirs_for_archived_mlx_files = subdirs ;
} ;;

(* End of tight version of file watching *)


