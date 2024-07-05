(*

#use"lib/local_dircopy_t.ml";;

*)

type t = {
   config: Local_dircopy_config_t.t;
   remote_files: string list;
   number_of_remote_files: int;
} ;;











