(*

#use"Van_der_Waerden/First_try/vdw_list_of_constraints_t.ml";;

*)

type t =
   Defined_by_max_width of int 
  |General_case of Set_of_integers_t.t  list ;;
