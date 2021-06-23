(*

#use"Filewatching/fw_modular_wrapper_t.ml";;

Acts on the physical Unix world around, within the limits
defined in  the configuration parameter

*)

type t ={
  parent : Fw_nonmodular_wrapper_t.t ;
  archived_compilable_files : (Dfn_rootless_t.t * string ) list;
  usual_compilable_files    : (Dfn_rootless_t.t * string ) list;
  noncompilable_files       : (Dfn_rootless_t.t * string ) list;
};;

