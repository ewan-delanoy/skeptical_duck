(* 

#use"Compilation_management/coma_constant.ml";;

*)

module Private = struct


let fads_subdir=
  Dfa_subdirectory.of_line "Fads";;

let directives_subdir=
  Dfa_subdirectory.of_line "directives";;

let debugging_subdir=
  Dfa_subdirectory.of_line "debugging";;

let githubbed_archive_subdir=
  Dfa_subdirectory.of_line "Githubbed_archive";;

let watched_subdir=
  Dfa_subdirectory.of_line "watched";;

let nonml_files_subdir=
  Dfa_subdirectory.of_line "nonml_files";;

let watched_and_githubbed_subdir=
  Dfa_subdirectory.extend watched_subdir "watched_and_githubbed";;

let watched_not_githubbed_subdir=
  Dfa_subdirectory.extend watched_subdir "watched_not_githubbed";;

let githubbed_nonml_files_subdir=
  Dfa_subdirectory.extend nonml_files_subdir "githubbed_nonml_files";;

let nongithubbed_nonml_files_subdir=
  Dfa_subdirectory.extend nonml_files_subdir "nongithubbed_nonml_files";;  

let build_subdir =   Dfa_subdirectory.of_line "_build";;
let usual_build_subdir= Dfa_subdirectory.extend build_subdir "_usual_build";;
let debug_build_subdir= Dfa_subdirectory.extend build_subdir "_debug_build";;  
let exec_build_subdir=  Dfa_subdirectory.extend build_subdir "_exec_build";;  
let parameters_subdir= Dfa_subdirectory.of_line "Compilation_management";;



let short_path_for_diary_file= Dfn_short.of_line"diary_archive.ml";;
let short_path_for_loadingsfile= Dfn_short.of_line"my_loadings.ml";;
let short_path_for_painful_debugging_file=Dfn_short.of_line"painful_debugging.ml";;
let short_path_for_parametersfile= Dfn_short.of_line "coma_big_constant.ml";;
let short_path_for_printersfile= Dfn_short.of_line "my_printers.ml";;
let short_path_for_targetfile= Dfn_short.of_line "targetfile.ocaml_made";;
 
 
let rootless_path_for_diary_file=
  Dfn_join.subdirectory_to_short  githubbed_archive_subdir short_path_for_diary_file;;
let rootless_path_for_loadingsfile=
  Dfn_join.subdirectory_to_short  directives_subdir short_path_for_loadingsfile;;
let rootless_path_for_painful_debugging_file=
  Dfn_join.subdirectory_to_short  fads_subdir short_path_for_painful_debugging_file;;
let rootless_path_for_parametersfile=
  Dfn_join.subdirectory_to_short  parameters_subdir short_path_for_parametersfile;;
let rootless_path_for_printersfile=
  Dfn_join.subdirectory_to_short  directives_subdir short_path_for_printersfile;;
let rootless_path_for_targetfile=
  Dfn_join.subdirectory_to_short  nongithubbed_nonml_files_subdir short_path_for_targetfile;;     

let rootless_path_for_ocamlinit = Dfn_rootless.of_line ".ocamlinit";;


let git_ignored_subdirectories =
  [
     build_subdir;
     fads_subdir;
     watched_not_githubbed_subdir;
     nongithubbed_nonml_files_subdir;
  ];;


let minimalist_text_for_ocamlinit =
   "\n#use\""^(Dfn_rootless.to_line rootless_path_for_loadingsfile)^"\""^Particular_string.double_semicolon^
  "\n#use\""^(Dfn_rootless.to_line rootless_path_for_printersfile)^"\""^Particular_string.double_semicolon;;

 let full_text_for_ocamlinit = (
      minimalist_text_for_ocamlinit^
      "\nopen Needed_values"^Particular_string.double_semicolon^
      "\ninitialize_toplevel()"^Particular_string.double_semicolon
       ) ;; 

let text_for_printersfile = "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n" ;;
let text_for_painful_debugging_file  = "\n\n(*\n\n#use\"Fads/painful_debugging.ml\""^Particular_string.double_semicolon^"\n\n*)\n\n" ;;

let common_part_in_conventional_files = 
   [
     rootless_path_for_printersfile, text_for_printersfile ; 
     rootless_path_for_loadingsfile, "" ;
     rootless_path_for_targetfile, "";
     rootless_path_for_diary_file, "";
   ] ;;     


let conventional_files_with_full_content =  
   [
     rootless_path_for_ocamlinit, full_text_for_ocamlinit ;
     rootless_path_for_painful_debugging_file, text_for_painful_debugging_file;
   ] @ common_part_in_conventional_files ;;      

let conventional_files_with_minimal_content =    
   [
     rootless_path_for_ocamlinit, minimalist_text_for_ocamlinit ;
   ] @ common_part_in_conventional_files ;;      


let minimal_set_of_needed_dirs = 
  [
    usual_build_subdir ;
    githubbed_archive_subdir;
    watched_not_githubbed_subdir;
    watched_and_githubbed_subdir;
    githubbed_nonml_files_subdir;
    nongithubbed_nonml_files_subdir;
  ] ;;  

let full_set_of_needed_dirs = 
  minimal_set_of_needed_dirs @
    [
      fads_subdir 
    ] ;;  

end ;;

 let conventional_files_with_full_content = Private.conventional_files_with_full_content ;;
 let conventional_files_with_minimal_content = Private.conventional_files_with_minimal_content ;;
 let debug_build_subdir = Private.debug_build_subdir ;;
 let debugging_subdir = Private.debugging_subdir ;;
 let exec_build_subdir = Private.exec_build_subdir ;;
 let full_set_of_needed_dirs = Private.full_set_of_needed_dirs ;;
 let git_ignored_subdirectories = Private.git_ignored_subdirectories ;;
 let githubbed_archive_subdir = Private.githubbed_archive_subdir ;;
 let minimal_set_of_needed_dirs = Private.minimal_set_of_needed_dirs ;;
 let nongithubbed_nonml_files_subdir = Private.nongithubbed_nonml_files_subdir ;;
 let rootless_path_for_diary_file = Private.rootless_path_for_diary_file ;;
 let rootless_path_for_loadingsfile = Private.rootless_path_for_loadingsfile ;;
 let rootless_path_for_parametersfile = Private.rootless_path_for_parametersfile ;;
 let rootless_path_for_printersfile = Private.rootless_path_for_printersfile ;;
 let rootless_path_for_targetfile = Private.rootless_path_for_targetfile ;;
 let usual_build_subdir = Private.usual_build_subdir ;;


