(*

#use"Filewatching/fw_with_dependencies_t.ml";;

Thin wrapper on Fw_with_module_linking_t.t, only adds
the dependencies layer with its associated caching.

*)

type t ={
  parent : Fw_with_module_linking_t.t ;
  index_for_caching : Fw_instance_index_t.t * Fw_state_index_t.t ; 
};;
