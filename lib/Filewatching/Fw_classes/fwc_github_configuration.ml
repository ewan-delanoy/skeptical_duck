(* 

#use"lib/Filewatching/Fw_classes/fwc_github_configuration.ml";;

*)

(*
type delta_t = Fw_poly_t.t ;;
type t = Fw_poly_t.t ;;

module Private = struct 




module Crobj = struct 
let salt = "Fwc_github_configuration_t." ;;
let label_for_dir_for_backup                     = salt ^ "dir_for_backup" ;;
let label_for_encoding_protected_files           = salt ^ "encoding_protected_files" ;;
let label_for_github_url                         = salt ^ "github_url" ;;
let label_for_gitpush_after_backup               = salt ^ "gitpush_after_backup" ;;


let of_concrete_object ccrt_obj = 
 let g=Concrete_object.get_record ccrt_obj in 
 {
   Fw_poly_t.type_name = Some "Fwc_github_configuration_t" ;
   dir_for_backup = Some(Dfa_root.of_concrete_object (g label_for_dir_for_backup))  ;
   encoding_protected_files = Some(Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object (g label_for_encoding_protected_files))  ;
   github_url = Some(Crobj_converter.string_of_concrete_object (g label_for_github_url))  ;
   gitpush_after_backup = Some(Crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup))  ;
 } ;;

let to_concrete_object fw = 
 let items =  
 [
   label_for_dir_for_backup, Dfa_root.to_concrete_object ( get_dir_for_backup fw ) ;
   label_for_encoding_protected_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object ( get_encoding_protected_files fw ) ;
   label_for_github_url, Crobj_converter.string_to_concrete_object ( get_github_url fw ) ;
   label_for_gitpush_after_backup, Crobj_converter.bool_to_concrete_object ( get_gitpush_after_backup fw ) ;
 ] in 
 Concrete_object_t.Record items ;;


end;; 

end ;; *)