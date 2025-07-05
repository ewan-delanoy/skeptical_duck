(* 

#use"lib/Filewatching/Fw_classes/fwg_with_githubbing.ml";;

*)


(* Beginning of loose version of file watching *)
(*


type t = Fw_flattened_poly_t.t ;;

let parent fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_batch_compilation" 
 } ;; 
let github_configuration fw = {
  fw with 
  Fw_flattened_poly_t.type_name = Some "Fw_github_configuration" 
 } ;; 

let make fw_batch github_config = {
  fw_batch with 
  Fw_flattened_poly_t.type_name = Some "Fw_with_githubbing" ;
  dir_for_backup = Some (Fwg_github_configuration.dir_for_backup github_config) ;
  gitpush_after_backup = Some (Fwg_github_configuration.gitpush_after_backup github_config) ;
  github_url = Some (Fwg_github_configuration.github_url github_config)  ;
  encoding_protected_files = Some (Fwg_github_configuration.encoding_protected_files github_config) ;
} ;;



*)
(* End of loose version of file watching *)

(* Beginning of tight version of file watching *)

type t = {
  _parent : Fwg_with_batch_compilation.t;
  github_config : Fwg_github_configuration.t;
} ;;

let parent fw = fw._parent ;; 
let github_configuration fw = fw.github_config ;; 

let make fw_batch github_configuration = {
  _parent = fw_batch ;
  github_config = github_configuration ;
} ;;

(* End of tight version of file watching *)


