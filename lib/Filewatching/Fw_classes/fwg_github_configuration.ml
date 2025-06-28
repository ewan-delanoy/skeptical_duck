(* 

#use"lib/Filewatching/Fw_classes/fwg_github_configuration.ml";;

*)


(* Beginning of loose version of file watching *)

type t = Fw_flattened_poly_t.t ;;



let make 
~v_dir_for_backup
~v_gitpush_after_backup
~v_github_url
~v_encoding_protected_files = {
  Fw_poly.origin with
  Fw_flattened_poly_t.type_name = Some "Fwc_github_configuration_t" ;
  dir_for_backup = Some(v_dir_for_backup)  ;
  encoding_protected_files = Some(v_encoding_protected_files)  ;
  github_url = Some(v_github_url)  ;
  gitpush_after_backup = Some(v_gitpush_after_backup)  ;
} ;;

let dir_for_backup fw = Fw_poly.dir_for_backup fw  ;;

let encoding_protected_files fw = Fw_poly.encoding_protected_files fw ;;

let github_url fw = Fw_poly.github_url fw  ;;
let gitpush_after_backup fw = Fw_poly.gitpush_after_backup fw  ;;

let set_gitpush_after_backup fw new_gab = {
  fw with
  Fw_flattened_poly_t.gitpush_after_backup = Some new_gab  ;
} ;;



(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

(* ... Put tight version here ... *)

(* End of tight version of file watching *)

