(* 

#use"Filewatching/fw_with_githubbing_t.ml";;

*)


type t={
     parent : Fw_poly_t.t;
     dir_for_backup : Dfa_root_t.t;
     gitpush_after_backup : bool;
     github_url : string;
     encoding_protected_files : (Dfn_rootless_t.t * Dfn_rootless_t.t) list;
};;