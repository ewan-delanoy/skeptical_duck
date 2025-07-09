(*

#use"lib/Filewatching/Fw_classes/fwg_configuration.ml";;

*)

(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;

let ignored_files fw = match fw.Fw_flattened_poly_t.ignored_files with Some x -> x | None -> raise(Get_exn "ignored_files") ;;
let ignored_subdirectories fw = match fw.Fw_flattened_poly_t.ignored_subdirectories with Some x -> x | None -> raise(Get_exn "ignored_subdirectories");;

let make v_root v_ignored_files v_ignored_subdirectories =  {
  Fw_flattened_poly_t.origin with
  Fw_flattened_poly_t.type_name = Some "Fwc_configuration" ;
  root = Some(v_root) ;
  ignored_files = v_ignored_files;
  ignored_subdirectories = v_ignored_subdirectories;
} ;;

let root fw = match fw.Fw_flattened_poly_t.root with Some x -> x | None -> raise(Get_exn "root")  ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = { 
  root : Dfa_root_t.t ;
  ignored_files : Dfn_rootless_t.t list;
  ignored_subdirectories : Dfa_subdirectory_t.t list;
}  ;;

let ignored_files fw = fw.ignored_files  ;;
let ignored_subdirectories fw = fw.ignored_subdirectories  ;;

let make v_root v_ignored_files v_ignored_subdirectories = {
  root = v_root;
  ignored_files = v_ignored_files;
  ignored_subdirectories = v_ignored_subdirectories;
} ;;

let root fw = fw.root   ;;


(* End of tight version of file watching *)

