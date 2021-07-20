(*

#use"Old_Van_der_Waerden/udwfw_current.ml";;

*)

let mw = Udw_max_width_t.MW 4 ;;
let threshhold = 15 ;; 

let base_for_threshhold = Udw_common.generic_computer mw threshhold ;;
let decompose = Udw_common.decompose mw ;;
let homogeneous_translation = 
     Udw_common.homogeneous_translation 
     (Udw_list_of_constraints_t.Defined_by_max_width (
      (fun (Udw_max_width_t.MW m)->m) mw) ) ;;
let lower_measure = Udw_common.lower_measure mw ;;
let measure = Udw_common.measure mw ;;
let minimal_obstructions_corresponding_to_above = Udw_common.minimal_obstructions_corresponding_to_above mw ;;
let rightmost_blowup = Udw_common.rightmost_blowup mw ;;


