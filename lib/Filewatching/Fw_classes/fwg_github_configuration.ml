(* 

#use"lib/Filewatching/Fw_classes/fwg_github_configuration.ml";;

*)


(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

exception Get_exn of string ;;


let dir_for_backup fw = match fw.Fw_flattened_poly_t.dir_for_backup with Some x -> x | None -> raise(Get_exn "dir_for_backup")  ;;

let encoding_protected_files fw = match fw.Fw_flattened_poly_t.encoding_protected_files with Some x -> x | None -> raise(Get_exn "encoding_protected_files") ;;

let github_url fw = match fw.Fw_flattened_poly_t.github_url with Some x -> x | None -> raise(Get_exn "github_url") ;;
let gitpush_after_backup fw = match fw.Fw_flattened_poly_t.gitpush_after_backup with Some x -> x | None -> raise(Get_exn "gitpush_after_backup");;

let make 
~v_root
~v_dir_for_backup
~v_gitpush_after_backup
~v_github_url
~v_encoding_protected_files = {
  Fw_flattened_poly_t.origin with
  Fw_flattened_poly_t.type_name = Some "Fwc_github_configuration_t" ;
  root = Some(v_root) ;
  dir_for_backup = Some(v_dir_for_backup)  ;
  encoding_protected_files = Some(v_encoding_protected_files)  ;
  github_url = Some(v_github_url)  ;
  gitpush_after_backup = Some(v_gitpush_after_backup)  ;
} ;;

let root fw = match fw.Fw_flattened_poly_t.root with Some x -> x | None -> raise(Get_exn "root")  ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = { 
  root : Dfa_root_t.t ;
  dir_for_backup : Dfa_root_t.t ;
  gitpush_after_backup : bool ;
  github_url : string ;
  encoding_protected_files : ((Dfn_rootless_t.t * Dfn_rootless_t.t) list) ;
}  ;;




let dir_for_backup fw = fw.dir_for_backup  ;;

let encoding_protected_files fw = fw.encoding_protected_files  ;;

let github_url fw = fw.github_url   ;;
let gitpush_after_backup fw = fw.gitpush_after_backup  ;;

let make 
~v_root
~v_dir_for_backup
~v_gitpush_after_backup
~v_github_url
~v_encoding_protected_files = {
  root = v_root;
  dir_for_backup =v_dir_for_backup  ;
  encoding_protected_files = v_encoding_protected_files  ;
  github_url = v_github_url  ;
  gitpush_after_backup = v_gitpush_after_backup  ;
} ;;

let root fw = fw.root ;;
(* End of tight version of file watching *)

