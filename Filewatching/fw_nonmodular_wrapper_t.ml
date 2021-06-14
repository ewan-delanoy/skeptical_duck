(*

#use"Filewatching/fw_nonmodular_wrapper_t.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

type t ={
  configuration         : Fw_configuration_t.t ;
  compilable_files      : (Dfn_rootless_t.t * string ) list;
  noncompilable_files   : (Dfn_rootless_t.t * string ) list;
  last_noticed_changes  : Dircopy_diff_t.t ;
};;