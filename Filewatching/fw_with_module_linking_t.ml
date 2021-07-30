(*

#use"Filewatching/fw_with_module_linking_t.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

type t ={
  parent : File_watcher_t.t ;
  modules_in_watched_files : (Dfn_rootless_t.t * (Dfa_module_t.t list)) list;
  printers_in_watched_files : Dfn_rootless_t.t list; 
};;
