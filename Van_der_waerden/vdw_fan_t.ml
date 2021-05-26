(*

#use"Van_der_Waerden/vdw_fan_t.ml";;

A fan is a finite union of translates of parts from
an initial partition (i.e. an object of type )

*)

type t = F of (int * (int list)) list;;
