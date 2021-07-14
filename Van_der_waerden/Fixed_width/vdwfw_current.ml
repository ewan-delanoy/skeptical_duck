(*

#use"Van_der_Waerden/Fixed_width/vdwfw_current.ml";;

*)

let mw = Vdw_max_width_t.MW 4 ;;

let base_for_15 = Vdw_common.generic_computer mw 15 ;;
let decompose = Vdw_common.decompose mw ;;
let lower_measure = Vdw_common.lower_measure mw ;;
let measure = Vdw_common.measure mw ;;


let homogeneous_translation = 
     Vdw_common.homogeneous_translation (Vdw_list_of_constraints_t.Defined_by_max_width 4) ;;