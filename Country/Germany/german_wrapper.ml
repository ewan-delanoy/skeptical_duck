(*

#use"Country/Germany/german_wrapper.ml";;


*)





let rename_directory (old_subdir,new_subdirname)=
        let _=Usual_coma_state.recompile_without_githubbing() in 
        let _=Coma_state.rename_directory (!(Usual_coma_state.main_ref)) (old_subdir,new_subdirname) in 
          Usual_coma_state.save_all();;   
        
