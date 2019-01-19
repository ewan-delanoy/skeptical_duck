
(* 

#use"Country/Germany/german_rename_directory.ml";;

Renaming directories inside the Ocaml main directory.


*)

let on_short_path=Rename_endsubdirectory.re;;

let on_subdirectory=Subdirectory.rename_endsubdirectory;;

let on_half_dressed_module=Half_dressed_module.rename_endsubdirectory;;


let on_mlx_ended_absolute_path=
  Mlx_ended_absolute_path.rename_endsubdirectory;;

let on_ocaml_target=Ocaml_target.rename_endsubdirectory;;

let on_printer_equipped_type pair (hm,is_compiled_correctly)=
    (Half_dressed_module.rename_endsubdirectory pair hm,is_compiled_correctly);;


let on_half_dressed_modules (old_subdir,new_subdirname) l=
    Image.image (on_half_dressed_module (old_subdir,new_subdirname)) l ;; 

let on_printer_equipped_types (old_subdir,new_subdirname) l=
        Image.image (on_printer_equipped_type (old_subdir,new_subdirname)) l ;; 

 
let on_subdirectories (old_subdir,new_subdirname) l_subdir=
   Image.image (on_subdirectory (old_subdir,new_subdirname)) l_subdir;; 
   
let on_up_to_date_targets (old_subdir,new_subdirname) l_tgts=
   Image.image (on_ocaml_target (old_subdir,new_subdirname)) l_tgts;;    


let on_deleted_files (old_subdir,new_subdirname) rl=
    let l=Recently_deleted.to_string_list rl in
    Recently_deleted.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );; 
   
let on_changed_files (old_subdir,new_subdirname) rl=
    let l=Recently_changed.to_string_list rl in
    Recently_changed.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );;    
   
let on_created_files (old_subdir,new_subdirname) rl=
    let l=Recently_created.to_string_list rl in
    Recently_created.of_string_list(
     Image.image (on_short_path (old_subdir,new_subdirname)) l 
    );;       
   
   
let on_delchacre_files (old_subdir,new_subdirname) l=
    Image.image (on_short_path (old_subdir,new_subdirname)) l ;; 
   

 
 
           