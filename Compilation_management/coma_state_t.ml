(* 

#use"Compilation_management/coma_state_t.ml";;

*)


type t={
     parent : Fw_with_batch_compilation_t.t;
     dir_for_backup : Dfa_root_t.t;
     gitpush_after_backup : bool;
     github_url : string;
};;
