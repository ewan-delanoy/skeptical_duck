(* 

#use"lib/Filewatching/Fw_classes/fwc_github_configuration.ml";;

*)



type t = Fw_poly_t.t ;;

module Private = struct 

let make 
~v_dir_for_backup
~v_gitpush_after_backup
~v_github_url
~v_encoding_protected_files = {
  Fw_poly.origin with
  Fw_poly_t.type_name = Some "Fwc_github_configuration_t" ;
  dir_for_backup = Some(v_dir_for_backup)  ;
  encoding_protected_files = Some(v_encoding_protected_files)  ;
  github_url = Some(v_github_url)  ;
  gitpush_after_backup = Some(v_gitpush_after_backup)  ;
} ;;

let dir_for_backup fw = Fw_poly.dir_for_backup fw  ;;

let encoding_protected_files fw = Fw_poly.encoding_protected_files fw ;;

let gitpush_after_backup fw = Fw_poly.gitpush_after_backup fw  ;;

let github_url fw = Fw_poly.github_url fw  ;;




module Crobj = struct 
let salt = "Fwc_github_configuration_t." ;;
let label_for_dir_for_backup                     = salt ^ "dir_for_backup" ;;
let label_for_encoding_protected_files           = salt ^ "encoding_protected_files" ;;
let label_for_github_url                         = salt ^ "github_url" ;;
let label_for_gitpush_after_backup               = salt ^ "gitpush_after_backup" ;;


let of_concrete_object ccrt_obj = 
  let g=Concrete_object.get_record ccrt_obj in 
  make 
   ~v_dir_for_backup:(Dfa_root.of_concrete_object (g label_for_dir_for_backup))
   ~v_gitpush_after_backup:(Crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup))
   ~v_github_url:(Crobj_converter.string_of_concrete_object (g label_for_github_url))
   ~v_encoding_protected_files:(Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object (g label_for_encoding_protected_files)) ;;
  ;;

let to_concrete_object fw = 
 let items =  
 [
   label_for_dir_for_backup, Dfa_root.to_concrete_object ( dir_for_backup fw ) ;
   label_for_encoding_protected_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object ( encoding_protected_files fw ) ;
   label_for_github_url, Crobj_converter.string_to_concrete_object ( github_url fw ) ;
   label_for_gitpush_after_backup, Crobj_converter.bool_to_concrete_object ( gitpush_after_backup fw ) ;
 ] in 
 Concrete_object_t.Record items ;;


end;; 

end ;; 


let dir_for_backup = Private.dir_for_backup ;;

let encoding_protected_files = Private.encoding_protected_files ;;

let gitpush_after_backup = Private.gitpush_after_backup ;;

let github_url = Private.github_url  ;;

let of_concrete_object = Private.Crobj.of_concrete_object ;;

let make = Private.make ;;
let to_concrete_object = Private.Crobj.to_concrete_object ;;