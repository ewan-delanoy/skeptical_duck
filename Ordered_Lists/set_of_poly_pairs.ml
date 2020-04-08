(* 

#use"Ordered_Lists/set_of_poly_pairs.ml";;s

*)

let tr = ((fun x->Set_of_poly_pairs_t.S(x)),(fun (Set_of_poly_pairs_t.S(x))->x),Total_ordering.standard2);;

let empty_set = Functor_for_sets.empty_set tr ;;
let fold_merge l= Functor_for_sets.fold_merge tr l;;
let forget_order z= Functor_for_sets.forget_order tr z;;
let hd z= Functor_for_sets.hd tr z;;
let image f l= Functor_for_sets.image tr f l;;
let insert x y=Functor_for_sets.insert tr x y;;
let intersect x y=Functor_for_sets.intersect tr x y;;
let is_included_in x y= Functor_for_sets.is_included_in tr x y;;
let length x= Functor_for_sets.length tr x;;
let mem x y= Functor_for_sets.mem tr x y;;
let merge x y= Functor_for_sets.merge tr x y;;
let outsert x y=Functor_for_sets.outsert tr x y;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let setminus x y= Functor_for_sets.setminus tr x y;;
let sort l= Functor_for_sets.sort tr l;;
let tl z= Functor_for_sets.tl tr z;;