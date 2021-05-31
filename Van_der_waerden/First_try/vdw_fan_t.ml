(*

#use"Van_der_Waerden/First_try/vdw_fan_t.ml";;

A fan is a finite union of translates of parts from
an initial partition (i.e. an object of type 
Vdw_repeatedly_partitionable_t.t)


*)

type t = F of (Vdw_part_t.t * (int list list)) list;;

