(*

#use"Van_der_Waerden/First_try/vdw_environment_t.ml";;


*)

type t = {
   headquarters : Vdw_repeatedly_partitionable_t.t ;
   variables : (Vdw_variable_t.t * Vdw_fan_t.t) list ;
   partition_history : ( Vdw_variable_t.t * Vdw_criterion_t.t * Vdw_variable_t.t * Vdw_variable_t.t) list; 
   partition_gains : ( Vdw_variable_t.t * Vdw_criterion_t.t * (Vdw_variable_t.t option) * (Vdw_variable_t.t option)) list; 
   translation_history : ( (Vdw_variable_t.t * (int list)) * Vdw_variable_t.t) list;
   mergeing_history :  ( (Vdw_variable_t.t * Vdw_variable_t.t) * Vdw_variable_t.t) list;  
};;

