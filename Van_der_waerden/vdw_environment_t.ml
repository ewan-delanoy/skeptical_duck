(*

#use"Van_der_Waerden/vdw_environment_t.ml";;


*)

type t = {
   headquarters : Vdw_repeatedly_partitionable_t.t ;
   variables : (int * Vdw_fan_t.t) list ;
};;

