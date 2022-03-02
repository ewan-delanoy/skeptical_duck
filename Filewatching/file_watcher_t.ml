(*

#use"Filewatching/file_watcher_t.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

type t ={
  configuration         : Fw_poly_t.t ;
  watched_files         : (Dfn_rootless_t.t * string ) list;
};;