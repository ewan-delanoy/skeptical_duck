(* 
#use"Makefile_makers/usual_coma_state.ml";;
*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.This_World.triple in 
  ref(Coma_state_field.empty_one root backup_dir githubbing);;
end;;

let above modname=Coma_state.Almost_concrete.local_above (!(Private.main_ref)) modname;; 


let backup diff opt=Coma_state.backup (!(Private.main_ref)) diff opt;;

let below modname=Coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;

let decipher_path pathname= Coma_state.decipher_path (!(Private.main_ref)) pathname;;
let decipher_module modname= Coma_state.decipher_module (!(Private.main_ref)) modname;;

let directly_below modname=Coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;

let duplicate_module old_t1 old_t2=
  Coma_state.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_half_dressed_module modname= 
  Coma_state.Almost_concrete.find_half_dressed_module (!(Private.main_ref)) modname;;

let forget_with_backup modname=Modify_persistent_coma_state.forget_with_backup Private.main_ref modname;;

let forget_without_backup modname=Modify_persistent_coma_state.forget_without_backup Private.main_ref modname;;      



let initialize ()=Modify_persistent_coma_state.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_persistent_coma_state.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Coma_state.Values_in_modules.list_values_from_module_in_modulesystem 
   (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

(* let polished_short_paths ()=
  Coma_state.Almost_concrete.polished_short_paths  (!(Private.main_ref));; *)

let recompile_without_githubbing ()=
   Modify_persistent_coma_state.recompile_without_githubbing Private.main_ref;;

let recompile opt=Modify_persistent_coma_state.recompile Private.main_ref opt;;
   

let refresh ()=Modify_persistent_coma_state.refresh Private.main_ref;;



let register_short_path x=Modify_persistent_coma_state.register_short_path Private.main_ref x;;
  
let relocate_module old_name new_name=
   Modify_persistent_coma_state.relocate_module Private.main_ref old_name new_name;;
   

let rename_module old_hm_name new_subdir=
   Modify_persistent_coma_state.rename_module Private.main_ref old_hm_name new_subdir;;


let rename_string_or_value old_name new_name=
   Coma_state.Values_in_modules.rename_string_or_value
   (!(Private.main_ref)) old_name new_name;;


let replace_string old_string new_string=
   Coma_state.Values_in_modules.replace_string 
   (!(Private.main_ref)) old_string new_string ;;



let save_all ()=Coma_state.Almost_concrete.save_all (!(Private.main_ref));;

let show_value_occurrences_in_modulesystem module_name=
   Coma_state.Values_in_modules.show_value_occurrences_in_modulesystem
   (!(Private.main_ref)) module_name;;


