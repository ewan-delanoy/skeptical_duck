(*

#use"lib/Cee_language/cee_snapshot_parameters_t.ml";;

*)

type  t = {
  root : Directory_name_t.t ;
  suffix_for_snapshots : string ;
  index : int ;
} ;; 