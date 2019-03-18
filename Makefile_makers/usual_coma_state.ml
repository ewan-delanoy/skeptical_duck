
(* 

#use"Makefile_makers/usual_coma_state.ml";;

*)

exception No_module_with_name of string;;

module Private = struct 

let main_ref=Coma_state_field.empty_one
                Coma_big_constant.this_world
                Coma_big_constant.backup_dir_for_this_world;;

let find_module_index x=
  let uncapitalized_x=
    Naked_module.of_string(String.uncapitalize_ascii x) in
  Coma_state.seek_module_index main_ref uncapitalized_x;;


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


let find_half_dressed_module x=
   match Private.find_module_index x
   with 
   Some(idx)->Coma_state.hm_at_idx Private.main_ref idx
   |None->raise(No_module_with_name(x));;  

let from_outside ()= Coma_state.from_outside  Private.main_ref Coma_big_constant.next_world;; 


let initialize ()=Coma_state.initialize Private.main_ref ;; 

let initialize_if_empty ()=
      if (Coma_state.size Private.main_ref=0) 
      then initialize();;                           

let main_ref=Private.main_ref;;

let polished_short_paths ()=
  Coma_state.all_polished_short_paths   
      main_ref Coma_big_constant.next_world;;

let recompile_without_githubbing opt=
  let (change_exists,short_paths)=Coma_state.recompile main_ref  in
  let changed_paths=
   (if not change_exists
   then []
   else let _=Private.save_all () in  
       Ordered_string.forget_order(Ordered_string.safe_set(short_paths))) in
    Dircopy_diff.veil
    (Recently_deleted.of_string_list [])
    (Recently_changed.of_string_list changed_paths)
    (Recently_created.of_string_list []) ;;

let recompile opt=
   let diff=recompile_without_githubbing opt in 
   if not(Dircopy_diff.is_empty diff)
   then backup diff opt;;

let refresh ()=
   let new_diff=Coma_state.refresh main_ref in
   let _=Private.save_all() in
   new_diff;;

let refresh_with_backup ()=
  let diff=refresh () in
  (
    backup diff None;
    Private.save_all() 
   );;

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
