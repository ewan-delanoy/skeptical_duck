
(* 

#use"Makefile_makers/usual_coma_state.ml";;

*)

module Private = struct 

let main_ref=Coma_state_field.empty_one
                Coma_big_constant.this_world
                Coma_big_constant.backup_dir_for_this_world;;

let whole ()=Coma_state.uple_form main_ref;;                

end;;



let backup diff opt=Coma_state.backup Private.main_ref diff opt;;

let from_outside ()= Coma_state.from_outside  Private.main_ref Coma_big_constant.next_world;; 


let initialize ()=Coma_state.initialize Private.main_ref ;; 

let initialize_if_empty ()=
      if (Coma_state.size Private.main_ref=0) 
      then initialize();;                           

let main_ref=Private.main_ref;;

let polished_short_paths ()=
  Coma_state.all_polished_short_paths   
      main_ref Coma_big_constant.next_world;;

let save_all ()=Coma_state.Save_all.write_all 
  (Coma_big_constant.this_world, 
    Coma_constant.name_for_makefile,
    Coma_constant.name_for_targetfile,
    Coma_constant.name_for_loadingsfile,
    Coma_constant.name_for_printersfile
  )
  (
	Private.whole()
  );;


let to_outside ()= Coma_state.to_outside  main_ref Coma_big_constant.next_world;;  

let whole =Private.whole;;
  

let ucc ()=
Coma_state.Create_or_update_copied_compiler.ucc
  main_ref 
 (Coma_big_constant.next_world,
  Coma_big_constant.dummy_backup_dir);;    
