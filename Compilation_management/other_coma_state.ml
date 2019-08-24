(* 
#use"Compilation_management/other_coma_state.ml";;
*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=
  let (root,backup_dir,githubbing)=Coma_big_constant.Next_World.triple in 
  ref(Coma_state_field.empty_one root backup_dir githubbing);;

end;;

let above modname=Coma_state.Almost_concrete.local_above (!(Private.main_ref)) modname;;


let below modname=Coma_state.Almost_concrete.local_below (!(Private.main_ref)) modname;;

let decipher_path pathname= Coma_state.decipher_path (!(Private.main_ref)) pathname;;
let decipher_module modname= Coma_state.decipher_module (!(Private.main_ref)) modname;;

let directly_below modname=Coma_state.Almost_concrete.local_directly_below (!(Private.main_ref)) modname;;

let duplicate_module old_t1 old_t2=
  Coma_state.duplicate_module (!(Private.main_ref)) old_t1 old_t2;;

let find_half_dressed_module modname= 
  Coma_state.Almost_concrete.find_half_dressed_module (!(Private.main_ref)) modname;;

let forget x=Modify_coma_state.Reference.forget Private.main_ref;;     

let initialize ()=Modify_coma_state.Reference.initialize Private.main_ref ;; 

let initialize_if_empty ()=Modify_coma_state.Reference.initialize_if_empty Private.main_ref ;;                       

let list_values_from_module_in_modulesystem module_name=
   Coma_state.Values_in_modules.list_values_from_module_in_modulesystem 
   (!(Private.main_ref)) module_name;;

let main_ref=Private.main_ref;;

let officialize_confidential_changes l=
   let this_root = Dfa_root.connectable_to_subpath (Coma_big_constant.This_World.root) 
   and next_root = Dfa_root.connectable_to_subpath (Coma_big_constant.Next_World.root) in 
   Image.image (
      fun path->
         "cp "^next_root^path^" "^this_root^path 
   ) l;;    


let recompile opt=Modify_coma_state.Reference.recompile Private.main_ref opt;;
   

let refresh ()=Modify_coma_state.Reference.refresh Private.main_ref;;



let register_short_path x=Modify_coma_state.Reference.register_short_path Private.main_ref x;;
  
let rename_module old_name new_name=
   Modify_coma_state.Reference.rename_module Private.main_ref old_name new_name;;

let rename_string_or_value old_name new_name=
   Modify_coma_state.Reference.rename_string_or_value
   (Private.main_ref) old_name new_name;;


let replace_string old_string new_string=
   Coma_state.Values_in_modules.replace_string 
   (!(Private.main_ref)) old_string new_string ;;

let repopulate ()=
  let _=Update_compiler_copy.ucc
  (!Usual_coma_state.main_ref)  in 
  initialize();; 

let see_confidential_changes ()=
   let temp1=Coma_state.all_rootless_paths (!(Usual_coma_state.main_ref)) in 
   let this_root = Dfa_root.connectable_to_subpath (Coma_big_constant.This_World.root) 
   and next_root = Dfa_root.connectable_to_subpath (Coma_big_constant.Next_World.root) in 
   Explicit.filter (
      fun path->
         let ap1=Absolute_path.of_string(this_root^path) 
         and ap2=Absolute_path.of_string(next_root^path) in 
         Io.read_whole_file(ap1)<>Io.read_whole_file(ap2)
   ) temp1;;    

let show_value_occurrences_in_modulesystem module_name=
   Coma_state.Values_in_modules.show_value_occurrences_in_modulesystem
   (!(Private.main_ref)) module_name;;


