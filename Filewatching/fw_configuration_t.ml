(*

#use"Filewatching/fw_configuration_t.ml";;

*)

type t ={
  root : Dfa_root_t.t ;
  allowed_endings : Dfa_ending_t.t list;
  git_ignored_subdirectories : Dfa_subdirectory_t.t list;
  special_git_saved_files : Dfn_rootless_t.t list;
  final_cleaner : Fw_final_cleaner_t.t ;
  ignored_files : Dfn_rootless_t.t list;
};;