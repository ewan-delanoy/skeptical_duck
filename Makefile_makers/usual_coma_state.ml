
(* 

#use"Makefile_makers/usual_coma_state.ml";;

*)

let main_ref=Coma_state_field.empty_one
                Coma_big_constant.this_world
                Coma_big_constant.backup_dir_for_this_world;;

let backup diff opt=Coma_state.backup main_ref diff opt;;

let from_outside ()= Coma_state.from_outside  main_ref Coma_big_constant.next_world;; 


let initialize ()=Coma_state.initialize main_ref ;; 

let initialize_if_empty ()=
      if (Coma_state.size main_ref=0) 
      then initialize();;                           

let polished_short_paths ()=
  Coma_state.all_polished_short_paths   
      main_ref Coma_big_constant.next_world;;

let to_outside ()= Coma_state.to_outside  main_ref Coma_big_constant.next_world;;  

let whole ()=Coma_state.uple_form main_ref;;
  

let ucc ()=
Coma_state.Create_or_update_copied_compiler.ucc
  main_ref 
 (Coma_big_constant.next_world,
  Coma_big_constant.dummy_backup_dir);;    
