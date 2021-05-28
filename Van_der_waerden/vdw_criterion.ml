(*

#use"Van_der_Waerden/vdw_criterion.ml";;

*)

module Private = struct

let test (Vdw_criterion_t.C(nt_criterion,translation)) l = match nt_criterion with 
   (Vdw_nontranslated_criterion_t.Cardinality_lower_than_or_equal_to k) ->
        (List.length l) + (List.length translation) <= k
  |(Compatible_with l2) ->
        let z= Set_of_integers.safe_set 
         (l@l2@translation) in 
       Vdw_common.test_for_admissibility
      (Vdw_list_of_constraints_t.Defined_by_max_width Vdw_current_bound.bound)
      z ;;

end ;;     
  
let partition criterion ll =
     List.partition (Private.test criterion) ll;;