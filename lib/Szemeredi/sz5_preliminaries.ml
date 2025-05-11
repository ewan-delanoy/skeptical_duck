(*

#use"lib/Szemeredi/sz5_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz5 is short for "fifth stab at Szemeredi problem".

*)

type width = Sz5_types.width = W of int ;; 

type constraint_t = Sz5_types.constraint_t = C of int list list ;;

let i_order = Total_ordering.for_integers ;;

let i_max = Ordered.max i_order ;;

let il_order = Total_ordering.silex_for_intlists ;;

let il_sort = Ordered.sort il_order ;;

module Constraint = struct 

module Private = struct
let constructor ll = 
   let temp1 = il_sort ll in 
   C(Ordered_misc.minimal_elts_wrt_inclusion temp1) ;;

let son (C ll) =
   let translated_forms = List.filter_map (
     fun l ->
      if List.mem 1 l then None else 
      Some(Image.image (fun t->t-1) l)  
   ) ll in 
   constructor translated_forms ;;

let daughter (W w) (C ll) =
   let translated_forms = Image.image (
     fun l ->
      let (a,b) =List_again.head_with_tail l in 
      let reduced_l = (if a=1 then b else l) in
      Image.image (fun t->t-1) reduced_l 
   ) ll 
   and extra_obstructions = 
    Int_range.scale (fun j->[1+j;1+2*j]) 1 w
  in 
  constructor (extra_obstructions @ translated_forms) ;;

let max (C ll) = Max.list (Image.image i_max ll) ;;

let order = (( fun cstr1 cstr2 ->
  let trial1 = Total_ordering.for_integers (max cstr1) (max cstr2) in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else
  let (C l1)=cstr1 and (C l2)=cstr2 in  
  Total_ordering.silex_compare il_order l1 l2
) : constraint_t Total_ordering_t.t) ;;

let immediate_descendants_for_individual w cstr =
  Ordered.sort order [son cstr;daughter w cstr] ;;

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

let daughter = Private.daughter ;;

let generated_tribe = Private.generated_tribe ;;
let order = Private.order ;;
let son = Private.son ;;


end ;;  

Ordered_misc.minimal_elts_wrt_inclusion ;;