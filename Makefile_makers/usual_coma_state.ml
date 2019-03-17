
(* 

#use"Makefile_makers/usual_coma_state.ml";;

*)

module Private = struct 

let main_ref=Coma_state_field.empty_one
                Coma_big_constant.this_world
                Coma_big_constant.backup_dir_for_this_world;;

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

let recompile opt=
  let (bowl,short_paths)=Coma_state.recompile main_ref  in
   (if bowl 
   then 
   let ordered_paths=Ordered_string.forget_order(Ordered_string.safe_set(short_paths)) in
   let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list ordered_paths)
    (Recently_created.of_string_list []) in
   (
      backup diff opt;
      Private.save_all() 
   ));;

let register_mlx_file mlx=
    let _=recompile None in 
    (
     Coma_state.register_mlx_file Private.main_ref mlx;
     Private.save_all();
    );;  
 
let register_short_path_without_backup x= 
  let path=Absolute_path.of_string(Root_directory.join Coma_big_constant.this_world x) in
  let mlx=Mlx_ended_absolute_path.of_path_and_root path Coma_big_constant.this_world in
  register_mlx_file mlx;;

let register_short_path x=
  let path=Absolute_path.of_string(Root_directory.join Coma_big_constant.this_world x) in
  let mlx=Mlx_ended_absolute_path.of_path_and_root path Coma_big_constant.this_world in
  let short_path=Mlx_ended_absolute_path.short_path mlx in
  let diff=
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list [])
    (Recently_created.of_string_list [short_path]) in
  (
    register_mlx_file mlx;
    backup diff None;
    Private.save_all() 
   );;


let save_all =Private.save_all;;


let to_outside ()= Coma_state.to_outside  main_ref Coma_big_constant.next_world;;  

let whole =Private.whole;;
  

let ucc ()=
Coma_state.Create_or_update_copied_compiler.ucc
  main_ref 
 (Coma_big_constant.next_world,
  Coma_big_constant.dummy_backup_dir);;    
