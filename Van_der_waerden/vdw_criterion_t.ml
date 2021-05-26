(*

#use"Van_der_Waerden/vdw_criterion_t.ml";;

*)

type t =
   Cardinality_equals of int
  |Compatible_with of (int list) ;;
