(*

#use"Szemeredi/sz_spray.ml";;

*)

let constructor l =
  let temp1 = Ordered.safe_set Total_ordering.silex_for_intlists l in 
  let temp2 = Ordered_misc.minimal_elts_wrt_inclusion temp1 in 
  Sz_spray_t.Sp temp2 ;;

let join_carefully max_width (Sz_spray_t.Sp l1) added =
    let temp = Option.filter_and_unpack (
      fun z ->
        let z2 = Ordered.merge Total_ordering.for_integers z added in 
        if Sz_preliminaries.test_for_admissibility max_width z2 
        then Some z2 
        else None   
    ) l1 in 
    Sz_spray_t.Sp temp ;; 


let merge (Sz_spray_t.Sp l1) (Sz_spray_t.Sp l2) =
   let temp1 = Ordered.merge Total_ordering.silex_for_intlists l1 l2 in 
   let temp2 = Ordered_misc.minimal_elts_wrt_inclusion temp1 in 
   Sz_spray_t.Sp temp2 ;;

