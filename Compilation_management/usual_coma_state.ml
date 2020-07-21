(* 

#use"Compilation_management/usual_coma_state.ml";;

*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.This_World.triple 
  and url=Coma_big_constant.github_url in 
  ref(Coma_state_field.empty_one root backup_dir githubbing url []);;
end;;

let above modname=Coma_state.Almost_concrete.local_above (!(Private.main_ref)) modname;; 

let add_printer_equipped_type mn=
   let old_cs=(!(Private.main_ref)) in 
   let new_cs=Coma_state.add_printer_equipped_type old_cs (mn,true) in 
   (Private.main_ref := new_cs ;
    Save_coma_state.save new_cs);;

let all_modules ()=Coma_state.all_modules (!(Private.main_ref)) ;; 

let below modname=Coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;

let census_of_foreigners ()= Coma_state.census_of_foreigners (!(Private.main_ref));;

let clean_debug_dir ()=Coma_state.clean_debug_dir (!(Private.main_ref));;
let clean_exec_dir ()=Coma_state.clean_exec_dir (!(Private.main_ref));;

let decipher_path pathname= Coma_state.decipher_path (!(Private.main_ref)) pathname;;
let decipher_module modname= Coma_state.decipher_module (!(Private.main_ref)) modname;;

let directly_above modname=Coma_state.Almost_concrete.local_directly_above (!(Private.main_ref)) modname;;
let directly_below modname=Coma_state.Almost_concrete.local_directly_below (!(Private.main_ref)) modname;;

let duplicate_module old_t1 old_t2=
  Coma_state.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_endingless modname= 
  Coma_state.endingless_from_mildly_capitalized_module_name (!(Private.main_ref)) modname;;

let forget_one modname=Modify_coma_state.Syntactic_sugar.forget Private.main_ref [modname];;

let forget_several modnames=Modify_coma_state.Syntactic_sugar.forget Private.main_ref modnames;;

let initialize ()=Modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let initialize ()=Modify_coma_state.Reference.initialize Private.main_ref ;; 

let internet_access bowl=Modify_coma_state.Reference.internet_access Private.main_ref bowl;;

let latest_changes ()=Coma_state.latest_changes (!(Private.main_ref));;

let list_values_from_module_in_modulesystem module_name=
   Coma_state.Values_in_modules.list_values_from_module_in_modulesystem 
   (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

let modules_using_value x=Coma_state.modules_using_value (!(Private.main_ref)) x;;

let recompile opt=Modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=Modify_coma_state.Reference.refresh Private.main_ref;;

let register_rootless_line x=Modify_coma_state.Syntactic_sugar.register_one Private.main_ref x;;
  
let register_rootless_lines x=Modify_coma_state.Syntactic_sugar.register_several Private.main_ref x;;

let relocate_module_to old_module_name new_subdir=
   Modify_coma_state.Syntactic_sugar.relocate_module_to Private.main_ref old_module_name new_subdir;;

let rename_subdirectory old_subdirname new_subdirname=
    Modify_coma_state.Syntactic_sugar.rename_subdirectory Private.main_ref old_subdirname new_subdirname;;

let rename_module old_name new_name=
   Modify_coma_state.Syntactic_sugar.rename_module Private.main_ref old_name new_name;;


let rename_string_or_value old_name new_name=
   Modify_coma_state.Reference.rename_string_or_value
   (Private.main_ref) old_name new_name;;

let show_value_occurrences_in_modulesystem module_name=
   Coma_state.Values_in_modules.show_value_occurrences_in_modulesystem
   (!(Private.main_ref)) module_name;;

let start_debugging ()=Coma_state.start_debugging (!(Private.main_ref));;
let start_executing short_path= Coma_state.start_executing (!(Private.main_ref)) short_path;;
