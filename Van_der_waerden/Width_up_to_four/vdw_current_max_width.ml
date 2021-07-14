(*

#use"Van_der_Waerden/Width_up_to_four/vdw_current_max_width.ml";;

*)

let mw = Vdw_max_width_t.MW 4 ;;

let lower_measure = Vdw_common.lower_measure mw ;;
let measure = Vdw_common.measure mw ;;

let homogeneous_translation = 
     Vdw_common.homogeneous_translation (Vdw_list_of_constraints_t.Defined_by_max_width 4) ;;