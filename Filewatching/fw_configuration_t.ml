(*

#use"Filewatching/fw_configuration_t.ml";;

*)

type t ={
  root : Dfa_root_t.t ;
  dir_for_backup : Dfa_root_t.t;
  gitpush_after_backup : bool;
  ignored_subdirectories : Dfa_subdirectory_t.t list;
  ignored_files : Dfn_rootless_t.t list;
  github_url : string;
  confidential_files : ( Dfn_rootless_t.t * Dfn_rootless_t.t) list;
};;