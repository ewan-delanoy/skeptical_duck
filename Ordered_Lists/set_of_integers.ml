(* 

#use"Ordered_Lists/set_of_integers.ml";;

*)

let tr = ((fun x->Set_of_integers_t.S(x)),(fun (Set_of_integers_t.S(x))->x),Total_ordering.standard);;

let fold_merge x = Functor_for_sets.fold_merge tr x;;
let forget_order x= Functor_for_sets.forget_order tr x;;
let image f x= Functor_for_sets.image tr f x;;
let is_included_in x y= Functor_for_sets.is_included_in tr x y;;
let length x= Functor_for_sets.length tr x;;
let max x= Functor_for_sets.max tr x;;
let mem a x= Functor_for_sets.mem tr a x;;
let merge l= Functor_for_sets.merge tr l;;
let min x= Functor_for_sets.min tr x;;
let outsert x oy= Functor_for_sets.outsert tr x oy;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let setminus x y= Functor_for_sets.setminus tr x y;;
let singleton a= Functor_for_sets.singleton tr a;;
let size_of_intersection x y= Functor_for_sets.size_of_intersection tr x y;;
let sort l= Functor_for_sets.sort tr l;;
let unsafe_set l= Functor_for_sets.unsafe_set tr l;;



