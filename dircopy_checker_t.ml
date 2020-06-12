(*

#use"dircopy_checker_t.ml";;

Usable on a github clone of the remote master version.

*)

type t ={
    ignored_endings : string list ;
    ignored_subdirs : Dfa_subdirectory_t.t list ;
    ignored_special_files : string list ;
    name_of_clone_directroy : string ; 
    clone_command : string ;

};;
  
