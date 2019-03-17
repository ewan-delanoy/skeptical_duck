(*

#use"Country/Germany/german_wrapper.ml";;


*)

let usual_root=Coma_big_constant.this_world;;
let usual_dir_for_backup=Coma_big_constant.backup_dir_for_this_world;;



let data ()= (Usual_coma_state.main_ref);;
let directories ()=Coma_state.directories(Usual_coma_state.main_ref);;

let declare_printer_equipped_type hm=
  (
    Coma_state.add_printer_equipped_type Usual_coma_state.main_ref hm; 
    Usual_coma_state.save_all ()
  );;
 
 
let end_debugging ()=
  let sbuild=(Root_directory.connectable_to_subpath usual_root)
     ^"_build/" in
  let _=Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^
     sbuild^"*.ocaml_debuggable")  in
  ();;   
   
let forget_unregistered_file ap= 
    let _=Coma_state.recompile Usual_coma_state.main_ref in
    let _=German_forget_unregistered_file.forget ap in
    Usual_coma_state.save_all();;       
       
let forget_file ap=
  let _=Coma_state.recompile Usual_coma_state.main_ref in 
  (
    Coma_state.forget_file Usual_coma_state.main_ref ap;
    Usual_coma_state.save_all();
  );;   

       
let forget_module hm=
        let _=Coma_state.recompile Usual_coma_state.main_ref in
        let short_paths=
          Coma_state.forget_module Usual_coma_state.main_ref hm in    
        let _=Usual_coma_state.save_all() in
        short_paths;;     

let initialize ()=
    Coma_state.initialize Usual_coma_state.main_ref ;; 

let initialize_if_empty ()=
      if (Coma_state.size(data())=0) 
      then initialize();;    

let printer_equipped_types ()=
    Coma_state.preq_types Usual_coma_state.main_ref;;

let recompile ()=
   let data=Coma_state.recompile Usual_coma_state.main_ref in
   let _=Usual_coma_state.save_all() in
   data;;  


let refresh ()=
   let new_diff=Coma_state.refresh Usual_coma_state.main_ref in
   let _=Usual_coma_state.save_all() in
   new_diff;;

let register_mlx_file mlx=
    let _=Coma_state.recompile Usual_coma_state.main_ref in 
    (
     Coma_state.register_mlx_file Usual_coma_state.main_ref mlx;
     Usual_coma_state.save_all();
    );;  
 
let relocate_module old_name new_subdir=
       let _=Coma_state.recompile Usual_coma_state.main_ref in 
      (
       Coma_state.relocate_module Usual_coma_state.main_ref old_name new_subdir;
       Usual_coma_state.save_all();
      );;          
    


let rename_directory (old_subdir,new_subdirname)=
        let _=Coma_state.recompile Usual_coma_state.main_ref in 
  (
          Coma_state.rename_directory Usual_coma_state.main_ref (old_subdir,new_subdirname);
          Usual_coma_state.save_all();
  );;   
        

let rename_module old_name new_name=
    let _=Coma_state.recompile Usual_coma_state.main_ref in 
    (
            let _=Coma_state.rename_module Usual_coma_state.main_ref old_name new_name in 
            Usual_coma_state.save_all();
    );;   
                   


let reposition_module hm (l_before,l_after)=
  let _=Coma_state.recompile Usual_coma_state.main_ref in
  let _=Coma_state.reposition_module Usual_coma_state.main_ref hm (l_before,l_after) in
  
    Usual_coma_state.save_all();
 ;;   
  
let start_debugging ()=
   let _=Coma_state.recompile Usual_coma_state.main_ref in 
   let _=Coma_state.start_debugging Usual_coma_state.main_ref in 
    Usual_coma_state.save_all();
 ;;    
    
    
let undeclare_printer_equipped_type hm=
    (
      Coma_state.remove_printer_equipped_type Usual_coma_state.main_ref hm; 
      Usual_coma_state.save_all ()
    );;
   

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

   
let whole=Usual_coma_state.whole;;
 
            