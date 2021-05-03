(*

#use"Van_der_waerden/vdw_advance_t.ml";;

*)

type t =
   Extend        of int * (int list) * (int list)
  |Decomposition of int * (int list) * (int list) * (int list) 
  |Fork          of int * ( int * int *int ) * (int list) ;;






