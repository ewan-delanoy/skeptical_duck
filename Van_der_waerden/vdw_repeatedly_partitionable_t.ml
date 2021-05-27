(*

#use"Van_der_Waerden/vdw_repeatedly_partitionable_t.ml";;

*)

type t =
   {
     parts : ( Vdw_part_t.t * (int list list)) list;
     history : ( Vdw_part_t.t * Vdw_criterion_t.t * Vdw_part_t.t * Vdw_part_t.t) list; 
     gains : ( Vdw_part_t.t * Vdw_criterion_t.t * (Vdw_part_t.t option) * (Vdw_part_t.t option)) list; 
   } ;;

  