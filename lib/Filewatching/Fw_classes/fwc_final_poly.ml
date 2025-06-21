(* 

#use"lib/Filewatching/Fw_classes/fwc_final_poly.ml";;

*)

type t =
   With_githubbing of Fw_poly_t.t;;

module Fwc_with_githubbing = struct 

let forget_modules (With_githubbing fw) mods=
    With_githubbing(Fw_with_githubbing.forget_modules fw mods) ;;
let forget_nonmodular_rootlesses (With_githubbing fw) rls=
    With_githubbing(Fw_with_githubbing.forget_modules fw rls) ;;    
let of_fw_config_and_github_config fw_config github_config=
    With_githubbing(Fw_with_githubbing.of_fw_config_and_github_config 
    fw_config github_config) ;;        
let of_fw_with_batch_compilation fw_batch backup_dir gab git_url enc_files=
    With_githubbing(Fw_with_githubbing.of_fw_with_batch_compilation 
    fw_batch backup_dir gab git_url enc_files) ;;  
let plunge_fw_config_with_github_config fw_config github_config=
    With_githubbing(Fw_with_githubbing.plunge_fw_config_with_github_config 
    fw_config github_config) ;;
let register_rootless_paths (With_githubbing fw) rls=
    With_githubbing(Fw_with_githubbing.register_rootless_paths fw rls) ;;
let relocate_module_to (With_githubbing fw) mod_name new_subdir=
    With_githubbing(Fw_with_githubbing.relocate_module_to 
    fw mod_name new_subdir) ;;
let rename_module (With_githubbing fw) old_middle_name new_nonslashed_name=
    With_githubbing(Fw_with_githubbing.rename_module 
    fw old_middle_name new_nonslashed_name) ;;

let rename_subdirectory_as (With_githubbing fw) (old_subdir,new_subdir)=
    With_githubbing(Fw_with_githubbing.rename_subdirectory_as 
    fw (old_subdir,new_subdir)) ;;

let replace_string (With_githubbing fw) old_s new_s=
    With_githubbing(Fw_with_githubbing.replace_string 
    fw old_s new_s) ;;

let replace_value (With_githubbing fw) ((preceding_files,path),(old_v,new_v))=
    With_githubbing(Fw_with_githubbing.replace_value 
    fw ((preceding_files,path),(old_v,new_v)) ) ;;
let usual_recompile (With_githubbing fw) opt_comment=
    With_githubbing(Fw_with_githubbing.usual_recompile fw opt_comment) ;;

end ;;  

