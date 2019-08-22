
(* 

#use"Makefile_makers/coma_constant.ml";;

*)


let automatically_generated_subdir=
  Dfa_subdirectory.of_line "Automatically_generated";;

let abandoned_ideas_subdir=
    Dfa_subdirectory.of_line "Abandoned_ideas";;  

let temporary_subdir=
  Dfa_subdirectory.of_line "Temporary";;

let githubbed_archive_subdir=
  Dfa_subdirectory.of_line "Githubbed_archive";;

let persistent_compilation_data_subdir = 
   Dfa_subdirectory.of_line "Persistent_compilation_data";;

let build_subdir=      Dfa_subdirectory.of_line "_build";;
let debug_build_subdir=Dfa_subdirectory.of_line "_debug_build";;  
let exec_build_subdir= Dfa_subdirectory.of_line "_exec_build";;  
let parameters_subdir= Dfa_subdirectory.of_line "Compilation_management";;


let bare_name_for_makefile="makefile";;
let bare_name_for_merlinfile=".merlin";;
let name_for_debugged_module="debugged";;  

let short_path_for_debugged_file= Dfn_short_path.of_line(name_for_debugged_module^".ml");; 
let short_path_for_loadingsfile= Dfn_short_path.of_line"my_loadings.ml";;
let short_path_for_parametersfile= Dfn_short_path.of_line "coma_big_constant.ml";;
let short_path_for_printersfile= Dfn_short_path.of_line "my_printers.ml";;
let short_path_for_targetfile= Dfn_short_path.of_line "targetfile.ocaml_made";;

let rootless_path_for_loadingsfile=
  Dfn_join.subdirectory_to  automatically_generated_subdir short_path_for_loadingsfile;;
let rootless_path_for_parametersfile=
  Dfn_join.subdirectory_to  parameters_subdir short_path_for_parametersfile;;
let rootless_path_for_printersfile=
  Dfn_join.subdirectory_to  automatically_generated_subdir short_path_for_printersfile;;
let rootless_path_for_targetfile=
  Dfn_join.subdirectory_to  persistent_compilation_data_subdir short_path_for_targetfile;;     

let up_to_date_but_not_registered_files=
    [
       rootless_path_for_loadingsfile;
       rootless_path_for_printersfile;
    ];;



           