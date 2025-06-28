(* 

#use"lib/Filewatching/Fw_classes/fwc_github_configuration.ml";;

*)

module Private = struct 


module Crobj = struct 
let salt = "Fwc_github_configuration_t." ;;
let label_for_dir_for_backup                     = salt ^ "dir_for_backup" ;;
let label_for_encoding_protected_files           = salt ^ "encoding_protected_files" ;;
let label_for_github_url                         = salt ^ "github_url" ;;
let label_for_gitpush_after_backup               = salt ^ "gitpush_after_backup" ;;


let of_concrete_object ccrt_obj = 
  let g=Concrete_object.get_record ccrt_obj in 
  Fwg_github_configuration.make 
   ~v_dir_for_backup:(Dfa_root.of_concrete_object (g label_for_dir_for_backup))
   ~v_gitpush_after_backup:(Crobj_converter.bool_of_concrete_object (g label_for_gitpush_after_backup))
   ~v_github_url:(Crobj_converter.string_of_concrete_object (g label_for_github_url))
   ~v_encoding_protected_files:(Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object (g label_for_encoding_protected_files)) ;;
  ;;

let to_concrete_object fw = 
 let items =  
 [
   label_for_dir_for_backup, Dfa_root.to_concrete_object ( Fwg_github_configuration.dir_for_backup fw ) ;
   label_for_encoding_protected_files, Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object ( Fwg_github_configuration.encoding_protected_files fw ) ;
   label_for_github_url, Crobj_converter.string_to_concrete_object ( Fwg_github_configuration.github_url fw ) ;
   label_for_gitpush_after_backup, Crobj_converter.bool_to_concrete_object ( Fwg_github_configuration.gitpush_after_backup fw ) ;
 ] in 
 Concrete_object_t.Record items ;;


end;; 

end ;;


let of_concrete_object = Private.Crobj.of_concrete_object ;;

let to_concrete_object = Private.Crobj.to_concrete_object ;;