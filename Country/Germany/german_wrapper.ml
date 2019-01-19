(*

#use"Country/Germany/german_wrapper.ml";;


*)

let usual_root=Coma_big_constant.this_world;;
let usual_dir_for_backup=Coma_big_constant.backup_dir_for_this_world;;


module Private=struct



let main_ref=Usual_coma_state.main_ref;;

let whole ()=Coma_state.uple_form main_ref;;

let save_all ()=Coma_state.Save_all.write_all 
  (usual_root, 
    Coma_constant.name_for_makefile,
    Coma_constant.name_for_targetfile,
    Coma_constant.name_for_loadingsfile,
    Coma_constant.name_for_printersfile
  )
  (
	  whole()
  );;


  

let recompile ()=Coma_state.recompile main_ref;;   

let recompile_softly ()= let _=recompile() in ();;

end;;


let backup diff opt=
     Coma_state.backup (Private.main_ref) diff opt;;
let data ()= (Private.main_ref);;
let directories ()=Coma_state.directories (Private.main_ref);;

let declare_printer_equipped_type hm=
  (
    Coma_state.add_printer_equipped_type Private.main_ref hm; 
    Private.save_all ()
  );;
 
 
let end_debugging ()=
  let sbuild=(Root_directory.connectable_to_subpath usual_root)
     ^"_build/" in
  let _=Unix_command.uc("rm -f "^sbuild^"*.d.cm*"^" "^
     sbuild^"*.ocaml_debuggable")  in
  ();;   
   
let forget_unregistered_file ap= 
    let _=Private.recompile() in
    let _=German_forget_unregistered_file.forget ap in
    Private.save_all();;       
       
let forget_file ap=
  (
    Private.recompile_softly();
    Coma_state.forget_file Private.main_ref ap;
    Private.save_all();
  );;   

       
let forget_module hm=
        let _=Private.recompile() in
        let short_paths=
          Coma_state.forget_module Private.main_ref hm in    
        let _=Private.save_all() in
        short_paths;;     

let initialize ()=
    Coma_state.initialize Private.main_ref ;; 

let initialize_if_empty ()=
      if (Coma_state.size(data())=0) 
      then initialize();;    

let printer_equipped_types ()=
    Coma_state.preq_types Private.main_ref;;

let recompile ()=
   let data=Private.recompile () in
   let _=Private.save_all() in
   data;;  


let refresh ()=
   let new_diff=Coma_state.refresh Private.main_ref in
   let _=Private.save_all() in
   new_diff;;

let register_mlx_file mlx=
    (Private.recompile_softly();
     Coma_state.register_mlx_file Private.main_ref mlx;
     Private.save_all();
    );;  
 
let relocate_module old_name new_subdir=
      (Private.recompile_softly();
       Coma_state.relocate_module Private.main_ref old_name new_subdir;
       Private.save_all();
      );;          
    


let rename_directory (old_subdir,new_subdirname)=
  (
          Private.recompile_softly();
          Coma_state.rename_directory Private.main_ref (old_subdir,new_subdirname);
          Private.save_all();
  );;   
        

let rename_module old_name new_name=
    (
            Private.recompile_softly();
            let _=Coma_state.rename_module Private.main_ref old_name new_name in 
            Private.save_all();
    );;   
                   


let reposition_module hm (l_before,l_after)=
  let _=Private.recompile_softly() in
  let _=Coma_state.reposition_module Private.main_ref hm (l_before,l_after) in
  
    Private.save_all();
 ;;   
  
let start_debugging ()=
   let _=Private.recompile_softly() in 
   let _=Coma_state.start_debugging Private.main_ref in 
    Private.save_all();
 ;;   
    

    
let save_all=Private.save_all;;   
    
    
let undeclare_printer_equipped_type hm=
    (
      Coma_state.remove_printer_equipped_type Private.main_ref hm; 
      Private.save_all ()
    );;
   

let unregister_module hm=
    (Private.recompile_softly();
     Coma_state.unregister_module Private.main_ref hm;
     Private.save_all();
    );;    


let unregister_mlx_file mlx=
   (Private.recompile_softly();
    Coma_state.unregister_mlx_file Private.main_ref mlx;
    Private.save_all();
   );;        
   
  
   

let view_definition s=
  let opt=Coma_state.find_value_definition (Private.main_ref) s in
  if opt=None then () else
  let itm=Option.unpack opt in
  let text="\n\n"^(Ocaml_gsyntax_item.whole itm)^"\n\n" in
  (print_string text;flush stdout);;   

   
let whole=Private.whole;;
 
            