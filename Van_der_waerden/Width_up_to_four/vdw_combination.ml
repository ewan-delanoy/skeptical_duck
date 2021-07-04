(*

#use"Van_der_Waerden/Width_up_to_four/vdw_combination.ml";;

*)

let replace_with_in (x,combination_for_x) combination_for_y =
  let (Vdw_combination_t.C partition_for_y) = combination_for_y in
  let (before,opt,after) = 
     Three_parts.select_center_element_and_reverse_left 
     (fun (translation,core)->core = x) partition_for_y in 
  match opt with 
   None -> combination_for_y  
  |Some(translation,_) -> 
     let (Vdw_combination_t.C partition_for_x) = combination_for_x in 
    let new_center = Image.image (
     fun (translation1,core1)->
           (Ordered.merge Vdw_common.oint translation translation1,core1)
    ) partition_for_x in 
    Vdw_combination_t.C(List.rev_append before (new_center@after)) ;;

  