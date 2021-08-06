(*

#use"Filewatching/fw_with_module_linking_t.ml";;


*)

type t ={
  parent : File_watcher_t.t ;
  small_details_in_files : (Dfn_rootless_t.t * Fw_file_simple_details_t.t) list;
};;
