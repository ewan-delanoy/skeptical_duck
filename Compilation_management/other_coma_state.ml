(* 
#use"Makefile_makers/unusual_coma_state.ml";;
*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.Next_World.triple in 
  ref(Coma_state_field.empty_one root backup_dir githubbing);;

end;;

let above =Coma_state.Almost_concrete.local_above (!(Private.main_ref));;


let below =Coma_state.Almost_concrete.local_below (!(Private.main_ref));;

let decipher_path = Coma_state.decipher_path (!(Private.main_ref));;
let decipher_module = Coma_state.decipher_module (!(Private.main_ref));;

let directly_below =Coma_state.Almost_concrete.local_below (!(Private.main_ref));;

let duplicate_module old_t1 old_t2=
  Coma_state.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_half_dressed_module = 
  Coma_state.Almost_concrete.find_half_dressed_module (!(Private.main_ref));;

let forget x=Modify_coma_state_reference.forget Private.main_ref;;     

let initialize ()=Modify_coma_state_reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_coma_state_reference.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Coma_state.Values_in_modules.list_values_from_module_in_modulesystem 
   (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;


let recompile opt=Modify_coma_state_reference.recompile Private.main_ref opt;;
   

let refresh ()=Modify_coma_state_reference.refresh Private.main_ref;;



let register_short_path x=Modify_coma_state_reference.register_short_path Private.main_ref x;;
  
let rename_string_or_value old_name new_name=
   Coma_state.Values_in_modules.rename_string_or_value
   (!(Private.main_ref)) old_name new_name;;


let replace_string old_string new_string=
   Coma_state.Values_in_modules.replace_string 
   (!(Private.main_ref)) old_string new_string ;;

let repopulate ()=
  let _=Update_compiler_copy.ucc
  (!Usual_coma_state.main_ref)  in 
  initialize();; 

let show_value_occurrences_in_modulesystem module_name=
   Coma_state.Values_in_modules.show_value_occurrences_in_modulesystem
   (!(Private.main_ref)) module_name;;


