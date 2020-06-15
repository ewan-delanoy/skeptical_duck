(*

#use"check_ocaml_dircopy.ml";;

Usable on a github clone of the remote master version.

*)

let instance ={
    Dircopy_checker_t.ignored_endings = 
     ["depend";"ocamlinit";"cmi";"cmo";"DS_Store";"txt";"php";"js";
      "ocaml_made";"ocaml_debuggable"] ;
    ignored_subdirs =  [
       Coma_constant.abandoned_ideas_subdir;
       Coma_constant.automatically_generated_subdir;
       Coma_constant.build_subdir;
       Coma_constant.githubbed_archive_subdir;
       Coma_constant.persistent_compilation_data_subdir;
       Coma_constant.temporary_subdir;
       Dfa_subdirectory.of_line ".vscode/";
       Dfa_subdirectory.of_line ".merlin/";
     ] ;
    ignored_special_files =  ["README";"makefile";"debugged.ml"];
    name_of_clone_directroy = "/Users/ewandelanoy/Downloads/Clone" ; 
    clone_command = "git clone https://github.com/ewan-delanoy/skeptical_duck " ;

};;
  
let check = Dircopy_checker.check instance ;;

