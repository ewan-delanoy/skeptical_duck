(* 

#use"lib/Filewatching/fw_automatic_persisting.ml";;

Follows Fw_check_for_no_changes (usually), 
and Fwc_with_githubbing (sometimes). 

*)

let forget_modules fw mod_names=
   let fw2 = Fw_check_for_no_changes.forget_modules fw mod_names in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;
   
let forget_nonmodular_rootlesses fw rootless_paths=
   let fw2 = Fw_check_for_no_changes.forget_nonmodular_rootlesses fw rootless_paths in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;

         
let save_latest_changes fw opt_comment=
   let fw2= Fwc_with_githubbing.inspect_and_update fw opt_comment in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;
let set_internet_access fw bowl=   
   let fw2=Fwc_with_githubbing.Inherited.set_gitpush_after_backup fw bowl in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;
let refresh fw =
   let fw2= Fwc_with_githubbing.refresh fw  in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;       

let register_rootless_paths fw rootless_path=
   let fw2 = Fw_check_for_no_changes.register_rootless_paths fw rootless_path in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;  
   
let relocate_module_to fw old_module new_subdir=
   let fw2 = Fw_check_for_no_changes.relocate_module_to fw old_module new_subdir in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;   
   
let rename_module fw old_middle_name new_nonslashed_name=
   let fw2=Fw_check_for_no_changes.rename_module fw old_middle_name new_nonslashed_name in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;  

let rename_subdirectory fw old_subdir new_subdir=
   let fw2=Fw_check_for_no_changes.rename_subdirectory_as fw old_subdir new_subdir in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;  

let replace_string fw old_s new_s=
   let fw2=Fw_check_for_no_changes.replace_string fw old_s new_s in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;     
    
         
let replace_value fw ((preceding_files,path),(old_v,new_v))=
   let fw2= Fw_check_for_no_changes.replace_value fw ((preceding_files,path),(old_v,new_v)) in 
   let _=Fw_persisting.persist fw2 in 
   fw2;;        
   
   
 