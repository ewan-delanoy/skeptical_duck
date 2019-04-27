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


let below modname=Coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;

let clean_debug_dir ()=Coma_state.clean_debug_dir (!(Private.main_ref));;
let clean_exec_dir ()=Coma_state.clean_exec_dir (!(Private.main_ref));;

let decipher_path pathname= Coma_state.decipher_path (!(Private.main_ref)) pathname;;
let decipher_module modname= Coma_state.decipher_module (!(Private.main_ref)) modname;;

let directly_below modname=Coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;

let duplicate_module old_t1 old_t2=
  Coma_state.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_half_dressed_module modname= 
  Coma_state.Almost_concrete.find_half_dressed_module (!(Private.main_ref)) modname;;

let forget modname=Modify_coma_state_reference.forget Private.main_ref modname;;

let initialize ()=Modify_coma_state_reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_coma_state_reference.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Coma_state.Values_in_modules.list_values_from_module_in_modulesystem 
   (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

let modules_using_value x=Coma_state.modules_using_value (!(Private.main_ref)) x;;

let recompile opt=Modify_coma_state_reference.recompile Private.main_ref opt;;
   

let refresh ()=Modify_coma_state_reference.refresh Private.main_ref;;



let register_short_path x=Modify_coma_state_reference.register_short_path Private.main_ref x;;
  
let relocate_module old_hm_name new_subdir=
   Modify_coma_state_reference.relocate_module Private.main_ref old_hm_name new_subdir;;

let rename_directory old_subdir new_subdirname=
    Modify_coma_state_reference.rename_directory Private.main_ref old_subdir new_subdirname;;


let rename_module old_name new_name=
   Modify_coma_state_reference.rename_module Private.main_ref old_name new_name;;


let rename_string_or_value old_name new_name=
   Coma_state.Values_in_modules.rename_string_or_value
   (!(Private.main_ref)) old_name new_name;;

let show_value_occurrences_in_modulesystem module_name=
   Coma_state.Values_in_modules.show_value_occurrences_in_modulesystem
   (!(Private.main_ref)) module_name;;

let start_debugging ()=Coma_state.start_debugging (!(Private.main_ref));;
let start_executing short_path= Coma_state.start_executing (!(Private.main_ref)) short_path;;
