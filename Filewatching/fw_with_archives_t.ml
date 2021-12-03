(*

#use"Filewatching/fw_with_archives_t.ml";;

*)

type t ={
  parent : File_watcher_t.t ;
  subdirs_for_archived_mlx_files : Dfa_subdirectory_t.t list ; 
};;
