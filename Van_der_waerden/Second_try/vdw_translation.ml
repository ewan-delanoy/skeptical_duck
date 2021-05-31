(*

#use"Van_der_Waerden/Second_try/vdw_translation.ml";;

*)

let merge 
  (Vdw_translation_t.T translation1)
    (Vdw_translation_t.T translation2) =
    let oint = Total_ordering.for_integers in 
    Vdw_translation_t.T(
      Ordered.merge oint translation1 translation2
    );;

let partition (Vdw_translation_t.T translation) (ll,common)=
    let oint = Total_ordering.for_integers in 
    let translation2 = Ordered.setminus oint translation common in 
    let (temp1,temp2) = List.partition (
        Ordered.is_included_in oint translation2
    ) ll in 
    (Vdw_comm_on.extract_core temp1,
     Vdw_comm_on.extract_core temp2);;

       