(*

#use"Van_der_Waerden/Fixed_width/vdw_chosen.ml";;

*)

let max_width = Vdw_max_width_t.MW 4 ;;

let extender = Vdw_max_width.extender max_width ;;

let lower_measure = Vdw_precomputed.lower_measure max_width ;;

let measure = Vdw_precomputed.measure max_width ;;

let naive_restricted_power_set = 
  Vdw_max_width.naive_restricted_power_set max_width;;

let test_for_admissibility = 
    Vdw_max_width.test_for_admissibility max_width;;
         
let test_joinability = 
    Vdw_max_width.test_joinability max_width;;