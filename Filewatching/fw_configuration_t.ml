(*

#use"Filewatching/fw_configuration_t.ml";;

*)

type t ={
  root : Dfa_root_t.t ;
  allowed_endings : Dfa_ending_t.t list;
  ignored_subdirectories : Dfa_subdirectory_t.t list;
  special_files : Dfn_rootless_t.t list;
};;