(*

#use"Van_der_Waerden/Width_up_to_four/vdw_combination.ml";;

*)

exception Homogeneous_translation_exn of string * (int list) * ( (int list) * (int list) );;

let homogeneous_translation 
 (Vdw_combination_t.C l) translation =
 let tempf1 =  (fun (translation1,core1) ->
     let full_translation = 
      Ordered.merge Vdw_common.oint
      translation1 translation in 
      match Vdw_variable.homogeneous_translation 
         core1 (full_translation) with 
      None -> None 
      |Some ll1 -> Some(full_translation,core1)   
)  in 
let tempf2 = (fun (translation1,core1)->
   try tempf1 (translation1,core1) with 
   Vdw_common.Homogeneous_translation_exn(tr,(l1,l2)) ->
      raise(Homogeneous_translation_exn(core1,tr,(l1,l2)) )
) in 
let temp1 = Option.filter_and_unpack tempf2 l in 
let temp2 = Image.image (
  fun (tr,core) -> 
   Option.unpack(Vdw_variable.homogeneous_translation core tr)
) temp1 in 
 Vdw_combination_t.C(Option.filter_and_unpack tempf2 l,
 Ordered.fold_merge Vdw_common.oord temp2);;

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

  