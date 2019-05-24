
(* 

#use"Makefile_makers/coma_constant.ml";;

*)

let automatically_generated_subdir=
  Subdirectory.of_string "Automatically_generated";;

let left_out_of_updating=
    Subdirectory.of_string "Left_out_of_updating";;  

let abandoned_ideas=
    Subdirectory.of_string "Abandoned_ideas";;  

let temporary_subdir=
  Subdirectory.of_string "Temporary";;

let githubbed_archive=
  Subdirectory.of_string "Githubbed_archive";;

let persistent_compilation_data_subdir = 
   Subdirectory.of_string "Persistent_compilation_data";;

let build_subdir=      Subdirectory.of_string "_build";;
let debug_build_subdir=Subdirectory.of_string "_debug_build";;  
let exec_build_subdir= Subdirectory.of_string "_exec_build";;  
let parameters_subdir= Subdirectory.of_string "Compilation_management";;


let name_for_makefile="makefile";;
let name_for_targetfile="targetfile.ocaml_made";;
let name_for_merlinfile=".merlin";;

let name_for_loadingsfile="my_loadings.ml";;
let name_for_printersfile="my_printers.ml";;
let name_for_parametersfile="coma_big_constant.ml";;


let path_for_loadingsfile=
  (Subdirectory.connectable_to_subpath automatically_generated_subdir)^name_for_loadingsfile;;
let path_for_parametersfile=
   (Subdirectory.connectable_to_subpath parameters_subdir)^name_for_parametersfile;;
let path_for_printersfile=
  (Subdirectory.connectable_to_subpath automatically_generated_subdir)^name_for_printersfile;;
let path_for_targetfile=
  (Subdirectory.connectable_to_subpath persistent_compilation_data_subdir)^name_for_targetfile;;     

let up_to_date_but_not_registered_files=
    [
       path_for_loadingsfile;
       path_for_printersfile;
    ];;

let name_for_debugged_module="debugged";;  
let path_for_debugged_file=name_for_debugged_module^".ml";; 


           