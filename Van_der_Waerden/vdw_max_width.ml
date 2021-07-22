(*

#use"Van_der_Waerden/vdw_max_width.ml";;

*)


let look_for_arithmetic_progressions_in_with_width_up_to 
  (Vdw_max_width_t.MW width) soi=
  let max_width = (if width<1 then ((Vdw_preliminaries.diameter soi)-1)/2 else width) in 
  List.rev(List.flatten(Ennig.doyle 
  (Vdw_preliminaries.look_for_arithmetic_progressions_in_with_width_equal_to soi) 1 max_width));;
  
let test_for_admissibility max_width soi = 
    ((look_for_arithmetic_progressions_in_with_width_up_to max_width soi) = [])
         
let test_joinability max_width l1 l2 =
    test_for_admissibility max_width 
    (Ordered.safe_set Vdw_preliminaries.oint (l1@l2)) ;;

let naive_restricted_power_set max_width soi =
        let temp1 = Vdw_preliminaries.naive_power_set soi in 
        List.filter (fun l-> test_for_admissibility max_width 
        l) temp1 ;;
  
let extender max_width ll x=
  let temp1 = Image.image 
     (fun y->Ordered.safe_set Vdw_preliminaries.oint (y@[x])) ll in 
  let temp2 = List.filter (test_for_admissibility max_width) temp1 in
  Ordered.merge  Vdw_preliminaries.ointlist ll temp2 ;;

