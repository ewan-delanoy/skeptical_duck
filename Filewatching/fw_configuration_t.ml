(*

#use"Filewatching/fw_configuration_t.ml";;


In the encoding_protected_files field, the list elements are pair of files ;
the first elt of the pair contains the encoding, and the second elt is the
encoded file. This allows you to vary the encoding depending on the file.

*)

type t ={
  root : Dfa_root_t.t ;
  dir_for_backup : Dfa_root_t.t;
  gitpush_after_backup : bool;
  ignored_subdirectories : Dfa_subdirectory_t.t list;
  ignored_files : Dfn_rootless_t.t list;
  github_url : string;
  encoding_protected_files : ( Dfn_rootless_t.t * Dfn_rootless_t.t) list;
};;