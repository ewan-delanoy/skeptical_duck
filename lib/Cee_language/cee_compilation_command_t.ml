(*

#use"lib/Cee_language/cee_compilation_command_t.ml";;

*)

type separate_t = {
  root : Directory_name_t.t;
  included_header_dirs : Directory_name_t.t list;
  included_source_dirs : string list;
  deffers : (string * (string option)) list;
  undeffers : string list;
  dep_file : string option;
  libobj_file : string option;
  short_path : string;
  ending : string;
  core_of_command : string;
  } ;;

type batch_t = {
  beginning_of_command : string ;
  litany: string list ;
  end_of_command : string
} ;; 

type t = 
    Separate of separate_t 
   |Batch of batch_t ;;