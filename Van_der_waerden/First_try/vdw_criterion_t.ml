(*

#use"Van_der_Waerden/First_try/vdw_criterion_t.ml";;

*)

type t =
   Cardinality_lower_than_or_equal_to of int
  |Compatible_with of (int list) ;;

  