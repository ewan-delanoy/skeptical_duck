
(* 

#use"Makefile_makers/unusual_coma_state.ml";;

*)

   

exception No_module_with_name of string;;

module Private = struct 

let main_ref=Coma_state_field.empty_one
                Coma_big_constant.next_world
                Coma_big_constant.dummy_backup_dir;;

let find_module_index x=
  let uncapitalized_x=
    Naked_module.of_string(String.uncapitalize_ascii x) in
  Coma_state.seek_module_index main_ref uncapitalized_x;;

let find_half_dressed_module x=
   match find_module_index x
   with 
   Some(idx)->Coma_state.hm_at_idx main_ref idx
   |None->raise(No_module_with_name(x));;  

let whole ()=Coma_state.uple_form main_ref;;  

let save_all ()=Coma_state.Save_all.write_all 
  (Coma_big_constant.this_world, 
    Coma_constant.name_for_makefile,
    Coma_constant.name_for_targetfile,
    Coma_constant.name_for_loadingsfile,
    Coma_constant.name_for_printersfile
  )
  (
	whole()
  );;



end;;

let above =Coma_state.Almost_concrete.local_above Private.main_ref;;


let backup diff opt=Coma_state.backup Private.main_ref diff opt;;

let below =Coma_state.Almost_concrete.local_below Private.main_ref;;

let decipher_path = Coma_state.decipher_path Private.main_ref;;
let decipher_module = Coma_state.decipher_module Private.main_ref;;

let directly_below =Coma_state.Almost_concrete.local_below Private.main_ref;;

let duplicate_module old_t1 old_t2=
  Coma_state.duplicate_module Private.main_ref old_t1 old_t2;;

let find_half_dressed_module = 
  Coma_state.Almost_concrete.find_half_dressed_module Private.main_ref;;



 
let forget_with_backup x=Coma_state.Almost_concrete.forget_with_backup Private.main_ref;;

let forget_without_backup x=Coma_state.Almost_concrete.forget_with_backup Private.main_ref;;      

 
let from_outside = Coma_state.Almost_concrete.local_from_outside  Private.main_ref;; 

let initialize ()=Coma_state.initialize Private.main_ref ;; 

let initialize_if_empty ()=Coma_state.Almost_concrete.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Coma_state.Values_in_modules.list_values_from_module_in_modulesystem 
   Private.main_ref module_name;;

let main_ref=Private.main_ref;;

let polished_short_paths ()=
  Coma_state.Almost_concrete.polished_short_paths   main_ref ;;

let recompile_without_githubbing ()=
   Coma_state.Almost_concrete.recompile_without_githubbing main_ref;;

let recompile opt=Coma_state.Almost_concrete.recompile main_ref opt;;
   

let refresh ()=Coma_state.Almost_concrete.local_refresh main_ref;;

let refresh_with_backup ()=Coma_state.Almost_concrete.refresh_with_backup main_ref;;


let register_short_path x=Coma_state.Almost_concrete.register_short_path main_ref x;;
  
let rename_string_or_value old_name new_name=
   Coma_state.Values_in_modules.rename_string_or_value
   Private.main_ref old_name new_name;;


let replace_string old_string new_string=
   Coma_state.Values_in_modules.replace_string 
   Private.main_ref old_string new_string ;;

let repopulate ()=
Coma_state.Create_or_update_copied_compiler.ucc
  main_ref 
 (Coma_big_constant.next_world,
  Coma_big_constant.dummy_backup_dir);; 

let save_all ()=Coma_state.Almost_concrete.save_all main_ref;;

let show_value_occurrences_in_modulesystem module_name=
   Coma_state.Values_in_modules.show_value_occurrences_in_modulesystem
   Private.main_ref module_name;;


let to_outside ()= Coma_state.Almost_concrete.local_to_outside  main_ref;;  
  
