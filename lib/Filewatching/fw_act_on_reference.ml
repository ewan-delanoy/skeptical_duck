(* 

#use"lib/Filewatching/fw_act_on_reference.ml";;

Follows Fw_automatic_persisting.

*)

   
let forget_modules pfw mod_names=
   let new_fw = Fw_automatic_persisting.forget_modules (!pfw) mod_names in 
   pfw:=new_fw;;
   
let forget_nonmodular_rootlesses pfw rootless_paths=
   let new_fw = Fw_automatic_persisting.forget_nonmodular_rootlesses (!pfw) rootless_paths in 
   pfw:=new_fw;; 
   
let load_persisted_version pfw =
   let new_fw = Fw_persisting.load_persisted_version (!pfw) in 
   pfw:=new_fw;;
   
let refresh pfw =
   let new_fw = Fw_automatic_persisting.refresh (!pfw)  in 
   pfw:=new_fw;;
   
let register_rootless_paths pfw rootless_paths=
   let new_fw = Fw_automatic_persisting.register_rootless_paths (!pfw) rootless_paths in 
   pfw:=new_fw;;
   
let relocate_module_to pfw old_module new_subdir=
   let new_fw = Fw_automatic_persisting.relocate_module_to (!pfw) old_module new_subdir in 
   pfw:=new_fw;;  
   
let rename_subdirectory pfw old_subdir new_subdir=
   let new_fw = Fw_automatic_persisting.rename_subdirectory (!pfw) old_subdir new_subdir in 
   pfw:=new_fw;;
            
let rename_module pfw old_middle_name new_nonslashed_name=
   let new_fw = Fw_automatic_persisting.rename_module (!pfw) old_middle_name new_nonslashed_name in 
   pfw:=new_fw;;
   
let replace_string pfw old_s new_s=
   let new_fw = Fw_automatic_persisting.replace_string (!pfw) old_s new_s in 
   pfw:=new_fw;;   
         
let replace_value pfw ((preceding_files,path),(old_v,new_v))=
   let new_fw = Fw_automatic_persisting.replace_value (!pfw) ((preceding_files,path),(old_v,new_v)) in 
   pfw:=new_fw;;      
     
let save_latest_changes pfw opt_comment=
   let new_fw = Fw_automatic_persisting.save_latest_changes (!pfw) opt_comment in 
   pfw:=new_fw;;
   
let set_internet_access pfw bowl=
   let new_fw = Fw_automatic_persisting.set_internet_access (!pfw) bowl in 
   pfw:=new_fw;;   