(* 

#use"Compilation_management/coma_constant.ml";;

*)

module Private = struct

let utility_files_subdir=
  Dfa_subdirectory.of_line "Utility_files";;

let fads_subdir=
  Dfa_subdirectory.of_line "Fads";;

let githubbed_archive_subdir=
  Dfa_subdirectory.of_line "Githubbed_archive";;

let persistent_data_subdir = 
  Dfa_subdirectory.extend utility_files_subdir "Persistent_data";;
let confidential_data_subdir = 
  Dfa_subdirectory.extend utility_files_subdir "Confidential_data";;

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
  Dfn_join.subdirectory_to_short  utility_files_subdir short_path_for_loadingsfile;;
let rootless_path_for_painful_debugging_file=
  Dfn_join.subdirectory_to_short  fads_subdir short_path_for_painful_debugging_file;;
let rootless_path_for_parametersfile=
  Dfn_join.subdirectory_to_short  parameters_subdir short_path_for_parametersfile;;
let rootless_path_for_printersfile=
  Dfn_join.subdirectory_to_short  utility_files_subdir short_path_for_printersfile;;
let rootless_path_for_targetfile=
  Dfn_join.subdirectory_to_short  persistent_data_subdir short_path_for_targetfile;;     

let rootless_path_for_ocamlinit = Dfn_rootless.of_line ".ocamlinit";;


let git_ignored_subdirectories =
  [
     utility_files_subdir;
     build_subdir;
     fads_subdir;
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
    persistent_data_subdir ; 
    usual_build_subdir ;
    utility_files_subdir
  ] ;;  

let full_set_of_needed_dirs = 
  minimal_set_of_needed_dirs @
    [
      fads_subdir 
    ] ;;  

end ;;

 let confidential_data_subdir = Private.confidential_data_subdir ;;
 let conventional_files_with_full_content = Private.conventional_files_with_full_content ;;
 let conventional_files_with_minimal_content = Private.conventional_files_with_minimal_content ;;
 let debug_build_subdir = Private.debug_build_subdir ;;
 let exec_build_subdir = Private.exec_build_subdir ;;
 let full_set_of_needed_dirs = Private.full_set_of_needed_dirs ;;
 let git_ignored_subdirectories = Private.git_ignored_subdirectories ;;
 let githubbed_archive_subdir = Private.githubbed_archive_subdir ;;
 let minimal_set_of_needed_dirs = Private.minimal_set_of_needed_dirs ;;
 let rootless_path_for_diary_file = Private.rootless_path_for_diary_file ;;
 let rootless_path_for_loadingsfile = Private.rootless_path_for_loadingsfile ;;
 let rootless_path_for_parametersfile = Private.rootless_path_for_parametersfile ;;
 let rootless_path_for_printersfile = Private.rootless_path_for_printersfile ;;
 let rootless_path_for_targetfile = Private.rootless_path_for_targetfile ;;
 let usual_build_subdir = Private.usual_build_subdir ;;
 let utility_files_subdir = Private.utility_files_subdir ;;


