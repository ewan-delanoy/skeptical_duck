(*

#use"Filewatching/fw_configuration_t.ml";;

*)

type t ={
  root : Dfa_root_t.t ;
  ignored_subdirectories : Dfa_subdirectory_t.t list;
  additional_files : Dfn_rootless_t.t list;
};;