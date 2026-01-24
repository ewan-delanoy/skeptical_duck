(*

#use"lib/Cee_language/cee_project_t.ml";;

*)

type  t = {
  root : Directory_name_t.t ;
  global_index : int ; 
  suffix_for_snapshots : string ;
} ;; 