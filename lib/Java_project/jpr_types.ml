(*

#use"lib/Java_project/jpr_types.ml";;

*) 

type project_root = Pr of string ;;

type java_source = Jsrc of string ;;

type java_file = Jf of string ;; 

type java_classname = Jcn of string ;; 

type java_package = Jpkg of string ;; 

type java_import = Jiprt of string * string ;; 

type java_subdir = Jsbd of string ;; 

