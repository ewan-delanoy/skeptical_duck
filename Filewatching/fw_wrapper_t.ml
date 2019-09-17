(*

#use"Filewatching/fw_wrapper_t.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

type t ={
  configuration         : Fw_configuration_t.t ;
  watched_files         : (Dfn_rootless_t.t * string * string) list;
  special_watched_files : (Dfn_rootless_t.t * string * string) list;
};;