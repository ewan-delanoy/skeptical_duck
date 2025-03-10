(*

#use"lib/local_dircopy_config_t.ml";;

*)

type t = {
   allowed_number_of_digits: int;
   remote_dir: string ;
   frontier_dir: string ; 
   persisted_file_listing_dir_content : string;
} ;;











