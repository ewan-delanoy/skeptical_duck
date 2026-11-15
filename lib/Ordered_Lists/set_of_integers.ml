(* 

#use"lib/Ordered_Lists/set_of_integers.ml";;

*)

let tr = 
  let i_order = Total_ordering.for_integers in 
  ((fun x->Set_of_integers_t.S(Ordered.to_list x)),(fun (Set_of_integers_t.S(x))->Ordered.safe_set i_order x),i_order);;

let empty_set = Set_of_integers_t.S [] ;; 
let fold_merge x = Functor_for_sets.fold_merge tr x;;
let forget_order x= Functor_for_sets.forget_order tr x;;
let image f x= Functor_for_sets.image tr f x;;
let insert x oy= Functor_for_sets.insert tr x oy;;
let intersect x y= Functor_for_sets.intersect tr x y;;
let is_included_in x y= Functor_for_sets.is_included_in tr x y;;
let length x= Functor_for_sets.length tr x;;
let max x= Functor_for_sets.max tr x;;
let mem a x= Functor_for_sets.mem tr a x;;
let merge l= Functor_for_sets.merge tr l;;
let min x= Functor_for_sets.min tr x;;
let outsert x oy= Functor_for_sets.outsert tr x oy;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let select_minimal_elements_for_inclusion l= Functor_for_sets.select_minimal_elements_for_inclusion tr l;;
let setminus x y= Functor_for_sets.setminus tr x y;;
let singleton a= Functor_for_sets.singleton tr a;;
let size_of_intersection x y= Functor_for_sets.size_of_intersection tr x y;;
let sort l= Functor_for_sets.sort tr l;;
let unsafe_set l= Set_of_integers_t.S l;;



