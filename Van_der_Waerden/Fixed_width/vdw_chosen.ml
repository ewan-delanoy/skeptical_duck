(*

#use"Van_der_Waerden/Fixed_width/vdw_chosen.ml";;

*)

let mw = Vdw_max_width_t.MW 4 ;;

let lower_measure n = Vdw_precomputed.lower_measure mw ;;

let measure n = Vdw_precomputed.measure mw ;;

let naive_restricted_power_set = 
  Vdw_max_width.naive_restricted_power_set mw;;

let test_for_admissibility = 
    Vdw_max_width.test_for_admissibility mw;;
         
let test_joinability = 
    Vdw_max_width.test_joinability mw;;