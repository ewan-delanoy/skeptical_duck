(*

#use"Van_der_Waerden/Fixed_width/vdw_computer.ml";;

*)

let computation1 () = Ennig.doyle (fun n->
  List.length(Vdw_precomputed.restricted_power_set 
  (Vdw_max_width_t.MW 4,Ennig.ennig 1 n)) 
) 1 24;;

let timed_computation1 () = Chronometer.it computation1 ();;

let order_for_s_obstructions =
  Total_ordering.product 
     Vdw_preliminaries.oint Vdw_preliminaries.ointlist ;;
let ref_for_s_admissibility = ref [] ;;
let is_s_admissible (d,l) =
  List.for_all (
    fun (d1,l1)->
      (d1<>d)||(not(Ordered.is_included_in Vdw_preliminaries.oint l1 l))   
  ) (!ref_for_s_admissibility) ;;
 
let add_new_s_obstructions l =
 ref_for_s_admissibility := 
  (Ordered.sort order_for_s_obstructions (l@(!ref_for_s_admissibility)));;

  exception Naive_main_on_large_number_exn of ((int * int list) list);;

let compute_all_without_remembering threshhold =
  let _=(ref_for_s_admissibility := []) in 
  let m_for_threshhold = Vdw_chosen.measure threshhold in 
  let base = Vdw_precomputed.restricted_power_set (Vdw_chosen.max_width,Ennig.ennig 1 threshhold) in 
  let horizontal = Memoized.make (fun d->
   List.filter(fun x->List.length(x) = m_for_threshhold-d) base) in 
  let translated_horizontal =(fun d l ->
   let temp1 = 
    Vdw_preliminaries.level_two_translate l (horizontal d) in 
   List.filter Vdw_chosen.test_for_admissibility temp1) in 
  let main_on_small_number =(fun n ->
   (Vdw_chosen.naive_restricted_power_set (Ennig.ennig 1 n),[])) in 
  let naive_main_on_large_number n =
    let m = Vdw_chosen.measure n in 
    let temp1 =  Vdw_chosen.naive_restricted_power_set (Ennig.ennig (threshhold+1) n) in 
    let temp2 = Image.image (fun l->(m_for_threshhold-m+List.length(l),l)) temp1 in 
    let temp3 = List.filter (fun (d,l)->(d>=0) && (is_s_admissible (d,l)) ) temp2 in 
    let temp4 = Image.image (fun (d,l)->
     ((d,l),translated_horizontal d l)
    ) temp3 in 
    let (temp5,temp6) = List.partition (fun ((d,l),res)->res=[]) temp4 in 
    if temp5 <> []
    then raise(Naive_main_on_large_number_exn(Image.image fst temp5))
    else 
    let temp7 = Image.image fst temp6    
    and temp8 = Image.image snd temp6 in 
    (Ordered.fold_merge Vdw_preliminaries.ointlist temp8,temp7) in 
  let main_on_large_number n =
      let m = Vdw_chosen.measure n in 
      let temp1 =  Vdw_chosen.naive_restricted_power_set (Ennig.ennig (threshhold+1) n) in 
      let temp2 = Image.image (fun l->(m_for_threshhold-m+List.length(l),l)) temp1 in 
      let temp3 = List.filter (fun (d,l)->(d>=0) && (is_s_admissible (d,l)) ) temp2 in 
      let temp4 = Image.image (fun (d,l)->
       ((d,l),translated_horizontal d l)
      ) temp3 in 
      let (temp5,temp6) = List.partition (fun ((d,l),res)->res=[]) temp4 in 
      if temp5 <> []
      then let _ = add_new_s_obstructions (Image.image fst temp5) in 
           naive_main_on_large_number n
      else 
      let temp7 = Image.image fst temp6    
      and temp8 = Image.image snd temp6 in 
      (Ordered.fold_merge Vdw_preliminaries.ointlist temp8,temp7) in     
  let main = Memoized.make (fun n->
    if n<=threshhold 
    then  main_on_small_number n 
    else  main_on_large_number n
  ) in 
  let _ =Explicit.image main (Ennig.ennig (threshhold+1) (threshhold+9)) in 
   (!ref_for_s_admissibility);;


let compute_all = Memoized.make compute_all_without_remembering ;;

let computation2 () = Explicit.image compute_all (Ennig.ennig 15 24);;

let timed_computation2 () = Chronometer.it computation2 ();;