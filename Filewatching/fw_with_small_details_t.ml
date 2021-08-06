(*

#use"Filewatching/fw_with_small_details_t.ml";;


*)

type t ={
  parent : File_watcher_t.t ;
  small_details_in_files : (Dfn_rootless_t.t * Fw_file_small_details_t.t) list;
};;
