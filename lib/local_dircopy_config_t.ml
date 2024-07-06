(*

#use"lib/local_dircopy_config_t.ml";;

*)

type t = {
   allowed_number_of_digits: int;
   remote_dir: Directory_name_t.t ;
   frontier_dir: Directory_name_t.t; 
   file_for_persistence : string;
} ;;











