(* 

#use"Ordered_Lists/set_of_integers.ml";;s

*)

let tr = ((fun x->Set_of_integers_t.S(x)),(fun (Set_of_integers_t.S(x))->x),Total_ordering.standard);;

let fold_merge x = Functor_for_sets.fold_merge tr x;;
let forget_order x= Functor_for_sets.forget_order tr x;;
let image f x= Functor_for_sets.image tr f x;;
let is_included_in x y= Functor_for_sets.is_included_in tr x y;;
let mem a x= Functor_for_sets.mem tr a x;;
let merge l= Functor_for_sets.merge tr l;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let setminus x y= Functor_for_sets.setminus tr x y;;
let singleton a= Functor_for_sets.singleton tr a;;
let sort l= Functor_for_sets.sort tr l;;
let unsafe_set l= Functor_for_sets.unsafe_set tr l;;



