(*

#use"Szemeredi/sz_preliminaries.ml";;


*)

module Private = struct

let i_sort = Ordered.safe_set Total_ordering.for_integers  ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;

let diameter soi =
  if List.length(soi)<2 then 0 else 
  (List.hd(List.rev soi)) - (List.hd soi) + 1  ;; 

let look_for_arithmetic_progressions_in_with_width_equal_to
         soi width=
  if List.length(soi)<3 then [] else 
  let temp1 = Image.image (fun x->[x;x+width;x+2*width]) soi in 
  List.filter (fun obstruction -> i_is_included_in obstruction soi) temp1 ;;  

let look_for_arithmetic_progressions_in_with_width_up_to 
  (Sz_max_width_t.MW width) soi=
  let max_width = (if width<1 then ((diameter soi)-1)/2 else width) in 
  List.rev(List.flatten(Ennig.doyle 
  (look_for_arithmetic_progressions_in_with_width_equal_to soi) 1 max_width));;
  
let test_for_admissibility max_width soi = 
    ((look_for_arithmetic_progressions_in_with_width_up_to max_width soi) = [])

let extender max_width ll x=
  let temp1 = Image.image (fun y->i_sort (y@[x])) ll in 
  let temp2 = List.filter (test_for_admissibility max_width) temp1 in
  il_merge ll temp2 ;;

let restricted_power_set =Memoized.recursive (fun old_f (max_width,soi) ->
  if soi = [] 
  then [[]]  
  else 
  let temp1 = List.rev soi in 
  let (last_elt,temp2) = Listennou.ht temp1 in 
  let soi2 = List.rev temp2 in 
  extender max_width (old_f (max_width,soi2)) last_elt  
);;

end ;;

let restricted_power_set = Private.restricted_power_set ;;