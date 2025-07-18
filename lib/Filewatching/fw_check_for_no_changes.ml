(* 

#use"lib/Filewatching/fw_check_for_no_changes.ml";;

Follows Fwc_with_githubbing.

*)

let forget_modules fw mod_names=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.forget_modules fw mod_names ;;
   
let forget_nonmodular_rootlesses fw rootless_paths=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.forget_nonmodular_rootlesses fw rootless_paths ;;
    

let register_rootless_paths fw rootless_path=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.register_rootless_paths fw rootless_path ;;  
   
let relocate_module_to fw old_module new_subdir=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.relocate_module_to fw old_module new_subdir ;;   
   
let rename_module fw old_middle_name new_nonslashed_name=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.rename_module fw old_middle_name new_nonslashed_name ;;  

let rename_subdirectory_as fw old_subdir new_subdir=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.rename_subdirectory_as fw (old_subdir,new_subdir) ;;  

let replace_string fw old_s new_s=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.replace_string fw old_s new_s ;;     
    
         
let replace_value fw ((preceding_files,path),(old_v,new_v))=
   let _=Fwc_with_githubbing.Inherited.check_that_no_change_has_occurred fw in 
   Fwc_with_githubbing.replace_value fw ((preceding_files,path),(old_v,new_v)) ;;        
   
   
 