(* 

#use"lib/Compilation_management/coma_constant.ml";;

*)

module Private = struct

let directives_subdir=
  Dfa_subdirectory.of_line "directives";;

let debugging_subdir=
  Dfa_subdirectory.of_line "debugging";;

let watched_subdir=
  Dfa_subdirectory.of_line "watched";;

let outer_subdir=
  Dfa_subdirectory.of_line "qfff";;

let dune_bin_subdir= Dfa_subdirectory.of_line "bin";;  
let dune_build_subdir= Dfa_subdirectory.of_line "_build";;
let dune_lib_subdir= Dfa_subdirectory.of_line "lib";;
let dune_test_subdir= Dfa_subdirectory.of_line "test";;
let dune_watched_subdir= Dfa_subdirectory.of_line "watched";; 

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

let build_subdir =   Dfa_subdirectory.extend nongithubbed_nonml_files_subdir "_build";;
let usual_build_subdir= Dfa_subdirectory.extend build_subdir "_usual_build";;
let debug_build_subdir= Dfa_subdirectory.extend build_subdir "_debug_build";;  
let exec_build_subdir=  Dfa_subdirectory.extend build_subdir "_exec_build";;  
let parameters_subdir= Dfa_subdirectory.extend dune_lib_subdir "Compilation_management";;

let short_path_for_diary_file= Dfn_short.of_line"diary_archive.ml";;
let short_path_for_dune_file= Dfn_short.of_line"dune.txt";;
let short_path_for_loadingsfile= Dfn_short.of_line"my_loadings.ml";;
let short_path_for_painful_debugging_file=Dfn_short.of_line"painful_debugging.ml";;
let short_path_for_parametersfile= Dfn_short.of_line "coma_big_constant.ml";;
let short_path_for_printersfile= Dfn_short.of_line "my_printers.ml";;
let short_path_for_targetfile= Dfn_short.of_line "targetfile.ocaml_made";;
 
let rootless_path_for_bindune_file=
  Dfn_join.subdirectory_to_short  dune_bin_subdir short_path_for_dune_file;;  

let rootless_path_for_diary_file=
  Dfn_join.subdirectory_to_short  watched_and_githubbed_subdir short_path_for_diary_file;;

let rootless_path_for_libdune_file=
  Dfn_join.subdirectory_to_short  dune_lib_subdir short_path_for_dune_file;;  
let rootless_path_for_loadingsfile=
  Dfn_join.subdirectory_to_short  directives_subdir short_path_for_loadingsfile;;
let rootless_path_for_ocamlformat_file =
    Dfn_rootless.of_line ".ocamlformat" ;;   
let rootless_path_for_ocamlinit_file =
  Dfn_rootless.of_line ".ocamlinit" ;; 
  
let rootless_path_for_opam_file proj_name=
    Dfn_rootless.of_line ((String.uncapitalize_ascii proj_name)^".opam") ;;   
  
let rootless_path_for_painful_debugging_file=
  Dfn_join.subdirectory_to_short  watched_not_githubbed_subdir short_path_for_painful_debugging_file;;
let rootless_path_for_parametersfile=
  Dfn_join.subdirectory_to_short  parameters_subdir short_path_for_parametersfile;;
let rootless_path_for_printersfile=
  Dfn_join.subdirectory_to_short  directives_subdir short_path_for_printersfile;;
let rootless_path_for_targetfile=
  Dfn_join.subdirectory_to_short  nongithubbed_nonml_files_subdir short_path_for_targetfile;;     
let rootless_path_for_watcheddune_file=
  Dfn_join.subdirectory_to_short  watched_subdir short_path_for_dune_file;;

let rootless_path_for_ocamlinit = Dfn_rootless.of_line ".ocamlinit";;



let git_ignored_subdirectories =
  [
     build_subdir;
     dune_bin_subdir;
     dune_build_subdir;
     dune_test_subdir;
     watched_not_githubbed_subdir;
     nongithubbed_nonml_files_subdir;
     directives_subdir;
     debugging_subdir;
     outer_subdir
  ];;


let minimalist_text_for_ocamlinit proj_name=
  "open "^(String.capitalize_ascii proj_name)^"_lib ;;"^
  "\n#use\""^(Dfn_rootless.to_line rootless_path_for_loadingsfile)^"\""^Particular_string.double_semicolon^
  "\n#use\""^(Dfn_rootless.to_line rootless_path_for_printersfile)^"\""^Particular_string.double_semicolon;;

 let full_text_for_ocamlinit proj_name= (
      (minimalist_text_for_ocamlinit proj_name)^
      "\nopen Needed_values"^Particular_string.double_semicolon^
      "\ninitialize_toplevel()"^Particular_string.double_semicolon
       ) ;; 

let text_for_bindune_file proj_name =
  let p = String.uncapitalize_ascii proj_name in 
  "(executable \n"^
  " (public_name "^p^")\n"^
  " (name main)\n"^
  " (libraries "^p^"_lib))\n";;

let text_for_libdune_file proj_name =
   let p = String.uncapitalize_ascii proj_name in 
   "(include_subdirs unqualified)\n"^
   "(library\n"^
   "(flags -w +1..31+33..69+71+72)\n"^
   "(name "^p^"_lib)\n"^
   "(libraries str unix zarith))\n" ;;

let text_for_watcheddune_file proj_name =
    let p = String.uncapitalize_ascii proj_name in 
    "(include_subdirs unqualified)\n"^
    "(library\n"^
    "(flags -w -27-32)\n"^
    "(name watched_lib)\n"^
    "(libraries "^p^"_lib))\n" ;;

let text_for_printersfile = "\n\n (*Registered printers start here *) \n\n (*Registered printers end here *) \n\n" ;;
let text_for_painful_debugging_file  = "\n\n(*\n\n#use\"Fads/painful_debugging.ml\""^Particular_string.double_semicolon^"\n\n*)\n\n" ;;

let common_part_in_conventional_files proj_name= 
   [
     rootless_path_for_printersfile, text_for_printersfile ; 
     rootless_path_for_loadingsfile, "" ;
     rootless_path_for_targetfile, "";
     rootless_path_for_diary_file, "";
     rootless_path_for_bindune_file, text_for_bindune_file proj_name;
     rootless_path_for_libdune_file, text_for_libdune_file proj_name;
     rootless_path_for_watcheddune_file, text_for_watcheddune_file proj_name;
   ] ;;     


let conventional_files_with_full_content proj_name=  
   [
     rootless_path_for_ocamlinit, full_text_for_ocamlinit proj_name ;
     rootless_path_for_painful_debugging_file, text_for_painful_debugging_file;
   ] @ (common_part_in_conventional_files proj_name) ;;      

let conventional_files_with_minimal_content proj_name=    
   [
     rootless_path_for_ocamlinit, minimalist_text_for_ocamlinit proj_name ;
   ] @ (common_part_in_conventional_files proj_name) ;;      


let minimal_set_of_needed_dirs = 
  [
    usual_build_subdir ;
    watched_not_githubbed_subdir;
    watched_and_githubbed_subdir;
    githubbed_nonml_files_subdir;
    nongithubbed_nonml_files_subdir;
    directives_subdir
  ] ;;  

let full_set_of_needed_dirs = 
  minimal_set_of_needed_dirs @
    [
    ] ;;  

let nongithubbed_high_subdirs = [
   directives_subdir;
   debugging_subdir;
   dune_bin_subdir;
   dune_build_subdir;
   dune_test_subdir;
   nonml_files_subdir;
   watched_not_githubbed_subdir;
] ;;

let nongithubbed_high_files proj_name = [
  rootless_path_for_ocamlformat_file ;   
  rootless_path_for_ocamlinit_file ; 
  rootless_path_for_opam_file proj_name;   
] ;;


end ;;

 let conventional_files_with_full_content = Private.conventional_files_with_full_content ;;
 let conventional_files_with_minimal_content = Private.conventional_files_with_minimal_content ;;
 let debug_build_subdir = Private.debug_build_subdir ;;
 let debugging_subdir = Private.debugging_subdir ;;
 let dune_lib_subdir = Private.dune_lib_subdir ;;
 let dune_watched_subdir = Private.dune_watched_subdir ;;
 let exec_build_subdir = Private.exec_build_subdir ;;
 let full_set_of_needed_dirs = Private.full_set_of_needed_dirs ;;
 let git_ignored_subdirectories = Private.git_ignored_subdirectories ;;
 let watched_and_githubbed_subdir = Private.watched_and_githubbed_subdir ;;
 let minimal_set_of_needed_dirs = Private.minimal_set_of_needed_dirs ;;
 let nongithubbed_high_files = Private.nongithubbed_high_files ;;
 let nongithubbed_high_subdirs = Private.nongithubbed_high_subdirs ;;
 let nongithubbed_nonml_files_subdir = Private.nongithubbed_nonml_files_subdir ;;
 let rootless_path_for_diary_file = Private.rootless_path_for_diary_file ;;
 let rootless_path_for_loadingsfile = Private.rootless_path_for_loadingsfile ;;
 let rootless_path_for_parametersfile = Private.rootless_path_for_parametersfile ;;
 let rootless_path_for_printersfile = Private.rootless_path_for_printersfile ;;
 let rootless_path_for_targetfile = Private.rootless_path_for_targetfile ;;
 let usual_build_subdir = Private.usual_build_subdir ;;


