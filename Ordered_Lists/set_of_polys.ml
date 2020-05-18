(* 

#use"Ordered_Lists/set_of_polys.ml";;

*)


let tr = ((fun x->Set_of_polys_t.S(x)),(fun (Set_of_polys_t.S(x))->x),Total_ordering.standard);;

let does_not_intersect x y= Functor_for_sets.does_not_intersect tr x y;;
let empty_set = Functor_for_sets.empty_set tr;;
let fold_merge l= Functor_for_sets.fold_merge tr l;;
let forget_order x= Functor_for_sets.forget_order tr x;;
let hd x = Functor_for_sets.hd tr x;;
let image f x= Functor_for_sets.image tr f x;;
let insert a x= Functor_for_sets.insert tr a x;;
let is_included_in x y= Functor_for_sets.is_included_in tr x y;;
let length x= Functor_for_sets.length tr x;;
let mem a x= Functor_for_sets.mem tr a x;;
let merge l= Functor_for_sets.merge tr l;;
let nmem a x= Functor_for_sets.nmem tr a x;;
let outsert a x= Functor_for_sets.outsert tr a x;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let setminus x y= Functor_for_sets.setminus tr x y;;
let singleton a= Functor_for_sets.singleton tr a;;
let sort l= Functor_for_sets.sort tr l;;
let unsafe_set l= Functor_for_sets.unsafe_set tr l;;


