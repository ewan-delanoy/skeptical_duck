(*

#use"Van_der_Waerden/vdw_repeatedly_partitionable_t.ml";;

*)

type t =
   {
     parts : (int * (int list list)) list;
     main : int list;
     history : ( int * Vdw_criterion_t.t * int * int) list; 
     gains : ( int * Vdw_criterion_t.t * int * int) list; 
   } ;;

  