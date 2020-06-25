
(* 

#use"Compilation_management/coma_constant.ml";;

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

let short_path_for_debugged_file= Dfn_short.of_line(name_for_debugged_module^".ml");; 
let short_path_for_loadingsfile= Dfn_short.of_line"my_loadings.ml";;
let short_path_for_ocamldebug_printersfile= Dfn_short.of_line"cmos_for_ocamldebug.txt";;
let short_path_for_painful_debugging_file=Dfn_short.of_line"painful_debugging.ml";;
let short_path_for_parametersfile= Dfn_short.of_line "coma_big_constant.ml";;
let short_path_for_printersfile= Dfn_short.of_line "my_printers.ml";;
let short_path_for_targetfile= Dfn_short.of_line "targetfile.ocaml_made";;

let rootless_path_for_debugged_file= Dfn_join.subdirectory_to (Dfa_subdirectory.of_line "") short_path_for_debugged_file;; 
let rootless_path_for_loadingsfile=
  Dfn_join.subdirectory_to  automatically_generated_subdir short_path_for_loadingsfile;;
let rootless_path_for_ocamldebug_printersfile=
  Dfn_join.subdirectory_to  automatically_generated_subdir short_path_for_ocamldebug_printersfile;;  
let rootless_path_for_painful_debugging_file=
  Dfn_join.subdirectory_to  temporary_subdir short_path_for_painful_debugging_file;;
let rootless_path_for_parametersfile=
  Dfn_join.subdirectory_to  parameters_subdir short_path_for_parametersfile;;
let rootless_path_for_printersfile=
  Dfn_join.subdirectory_to  automatically_generated_subdir short_path_for_printersfile;;
let rootless_path_for_targetfile=
  Dfn_join.subdirectory_to  persistent_compilation_data_subdir short_path_for_targetfile;;     

let rootless_path_for_ocamlinit = Dfn_rootless.of_line ".ocamlinit";;

let updated_not_compiled_files=
    [
       rootless_path_for_loadingsfile;
       rootless_path_for_printersfile;
    ];;

let rootless_paths_needed_for_compiler_copy=
    updated_not_compiled_files@
    [
      rootless_path_for_targetfile;
      rootless_path_for_ocamlinit
    ];;

let git_ignored_subdirectories =
  [
     automatically_generated_subdir;
     abandoned_ideas_subdir;
     temporary_subdir;
     githubbed_archive_subdir;
     persistent_compilation_data_subdir;
     build_subdir;
     debug_build_subdir;
     exec_build_subdir
  ];;

let conventional_subdirs =
   [
     build_subdir;
     debug_build_subdir;
     exec_build_subdir
   ];;

let minimalist_text_for_ocamlinit =
   "\n#use\""^(Dfn_rootless.to_line rootless_path_for_loadingsfile)^"\""^Double_semicolon.ds^
  "\n#use\""^(Dfn_rootless.to_line rootless_path_for_printersfile)^"\""^Double_semicolon.ds;;

 let usual_text_for_ocamlinit = (
      minimalist_text_for_ocamlinit^
      "\nopen Needed_values"^Double_semicolon.ds^
      "\ninitialize_toplevel()"^Double_semicolon.ds
       ) ;; 

let conventional_files_other_than_ocamlinit =
  let text_for_printersfile = "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n" 
  and text_for_painful_debugging_file  = "\n\n(*\n\n#use\"Temporary/painful_debugging.ml\";;\n\n*)\n\n" in    
   [
    rootless_path_for_printersfile, text_for_printersfile ; 
     rootless_path_for_loadingsfile, "" ;
     rootless_path_for_targetfile, "";
     rootless_path_for_painful_debugging_file, text_for_painful_debugging_file;
   ] ;;     


let conventional_files_with_usual_content =  
   [
     rootless_path_for_ocamlinit, usual_text_for_ocamlinit ;
   ] @ conventional_files_other_than_ocamlinit ;;      

let conventional_files_with_minimal_content =    
   [
     rootless_path_for_ocamlinit, minimalist_text_for_ocamlinit ;
   ] @ conventional_files_other_than_ocamlinit ;;      


let endings_for_cleaning  = [
    Dfa_ending.mll, Dfa_ending.ml;
    Dfa_ending.mly, Dfa_ending.ml
];;

let reserved_terminations = ["executable"];;

let git_ignored_files = [rootless_path_for_debugged_file];;

    