(*

#use"Country/Germany/german_wrapper.ml";;


*)

 
let end_debugging ()=
  let sbuild=(Root_directory.connectable_to_subpath Coma_big_constant.This_World.root)
     ^"_build/" in
  let _=Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^
     sbuild^"*.ocaml_debuggable")  in
  ();;     

 



let rename_directory (old_subdir,new_subdirname)=
        let _=Usual_coma_state.recompile_without_githubbing() in 
        let _=Coma_state.rename_directory (!(Usual_coma_state.main_ref)) (old_subdir,new_subdirname) in 
          Usual_coma_state.save_all();;   
        
            
  
let start_debugging ()=
   let _=Coma_state.recompile (!(Usual_coma_state.main_ref)) in 
   let _=Coma_state.start_debugging (!(Usual_coma_state.main_ref)) in 
    Usual_coma_state.save_all();
 ;;    
  

 
            