(*

#use"Filewatching/fw_with_small_details_t.ml";;

Stores the small details for archived or usual files (not the noncompilables).

*)

type t ={
  parent : File_watcher_t.t ;
  small_details_in_files : (Dfn_rootless_t.t * Fw_file_small_details_t.t) list;
};;
