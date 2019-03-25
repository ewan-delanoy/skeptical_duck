(*

#use"Country/Germany/german_wrapper.ml";;


*)

 
let end_debugging ()=
  let sbuild=(Root_directory.connectable_to_subpath Coma_big_constant.this_world)
     ^"_build/" in
  let _=Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^
     sbuild^"*.ocaml_debuggable")  in
  ();;     

 



let rename_directory (old_subdir,new_subdirname)=
        let _=Coma_state.recompile Usual_coma_state.main_ref in 
  (
          Coma_state.rename_directory Usual_coma_state.main_ref (old_subdir,new_subdirname);
          Usual_coma_state.save_all();
  );;   
        
            
  
let start_debugging ()=
   let _=Coma_state.recompile Usual_coma_state.main_ref in 
   let _=Coma_state.start_debugging Usual_coma_state.main_ref in 
    Usual_coma_state.save_all();
 ;;    
  
  
let unregister_module hm=
     let _=Coma_state.recompile Usual_coma_state.main_ref in 
    (
     Coma_state.unregister_module Usual_coma_state.main_ref hm;
     Usual_coma_state.save_all();
    );;    


let unregister_mlx_file mlx=
   let _=Coma_state.recompile Usual_coma_state.main_ref in 
   (
    Coma_state.unregister_mlx_file Usual_coma_state.main_ref mlx;
    Usual_coma_state.save_all();
   );;        
   
  
   

let view_definition s=
  let opt=Coma_state.find_value_definition (Usual_coma_state.main_ref) s in
  if opt=None then () else
  let itm=Option.unpack opt in
  let text="\n\n"^(Ocaml_gsyntax_item.whole itm)^"\n\n" in
  (print_string text;flush stdout);;   

   

 
            