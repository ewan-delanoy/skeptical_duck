(*

#use"Filewatching/fw_wrapper_t.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

type t ={
  parent : File_watcher_t.t ;
};;

