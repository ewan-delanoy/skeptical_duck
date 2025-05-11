(*

#use"lib/Szemeredi/sz5_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz5 is short for "fifth stab at Szemeredi problem".

*)

type width = Sz5_types.width = W of int ;; 

type constraint_t = Sz5_types.constraint_t = C of int list list ;;

let i_order = Total_ordering.for_integers ;;

let i_is_included_in = Ordered.is_included_in i_order ;;
let i_max = Ordered.max i_order ;;

let i_setminus = Ordered.setminus i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;

let il_fold_merge = Ordered.fold_merge il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

module Constraint = struct 

exception Shift_exn of int list list ;; 

module Private = struct
let constructor ll = 
   let temp1 = il_sort ll in 
   C(Ordered_misc.minimal_elts_wrt_inclusion temp1) ;;



let max (C ll) = 
  if ll = [] then (-1) else
  if ll = [[]] then 0 else  
  Max.list (Image.image i_max ll) ;;

let order = (( fun cstr1 cstr2 ->
  let trial1 = Total_ordering.for_integers (max cstr1) (max cstr2) in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else
  let (C l1)=cstr1 and (C l2)=cstr2 in  
  Total_ordering.silex_compare il_order l1 l2
) : constraint_t Total_ordering_t.t) ;;

let left_complements (W w) a= 
  Int_range.scale (fun t->[a-2*t;a-t]) 1 (min w (a/2)) ;;

let middle_complements (W w) a= 
  Int_range.scale (fun t->[a-t;a+t]) 1 (min w (a-1)) ;;  

let right_complements (W w) a= 
  Int_range.scale (fun t->[a+t;a+2*t]) 1 w ;;    

let complements_for_individual w a =
   il_sort (
    (left_complements w a) 
    @ (middle_complements w a) 
    @ (right_complements w a)
   ) ;; 

let complements w l = 
  Ordered.fold_merge 
  il_order
  (Image.image (complements_for_individual w) l) ;;

let force w forced_subset (C ll) =
   let base = il_merge (complements w forced_subset) ll in 
   let cleaned_base = List.filter_map (
     fun z ->
      let reduced_z = i_setminus z forced_subset in 
      if reduced_z = [] then None else Some reduced_z
   ) base in 
   constructor cleaned_base ;;

let remove removed_subset (C ll) =
   C(List.filter (
    fun z -> not(i_is_included_in removed_subset z)
   ) ll) ;;

let shift (C ll) =
   if List.exists (fun z->List.hd(z)=1) ll   
   then raise(Shift_exn(ll))
   else  C(Image.image (Image.image(fun t->t-1)) ll);;
   
let son cstr = shift(remove [1] cstr);;

let daughter_opt w cstr = 
  try Some(shift(force w [1] cstr)) with Shift_exn _ -> None ;;


let immediate_descendants_for_individual w cstr =
  let l = List.filter_map (fun o->o) [Some(son cstr);daughter_opt w cstr] in 
  Ordered.sort order l ;;

let immediate_descendants w l_cstr =
   Ordered.fold_merge order (Image.image 
   (immediate_descendants_for_individual w) l_cstr ) ;;

let rec helper_for_generating_tribe (w,whole_so_far,to_be_treated) =
   if to_be_treated = []
   then whole_so_far 
   else 
   let candidates = immediate_descendants w to_be_treated in 
   let new_descendants = 
      Ordered.setminus order candidates whole_so_far in 
   helper_for_generating_tribe (w,
    Ordered.merge order whole_so_far new_descendants,new_descendants) ;;

let generated_tribe w l_cstr = 
   helper_for_generating_tribe (w,l_cstr,l_cstr) ;;



end ;;     

let constructor = Private.constructor ;;

let daughter_opt = Private.daughter_opt ;;

let force = Private.force ;;
let generated_tribe = Private.generated_tribe ;;
let order = Private.order ;;

let shift = Private.shift ;;
let son = Private.son ;;


end ;;  

module Compute = struct 

module Private = struct 
(*
let ref_for_unregistered_constraints = ref [] ;;

let ref_for_precomputed = ref [] ;;
*)

end ;;  


end ;;  
