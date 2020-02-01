(* 

#use"Ordered_Lists/set_of_poly_pairs.ml";;s

*)

let tr = ((fun x->Set_of_poly_pairs_t.S(x)),(fun (Set_of_poly_pairs_t.S(x))->x),Total_ordering.standard2);;

let fold_merge l= Functor_for_sets.fold_merge tr l;;
let forget_order z= Functor_for_sets.forget_order tr z;;
let image f l= Functor_for_sets.image tr f l;;
let length x= Functor_for_sets.length tr x;;
let merge x y= Functor_for_sets.merge tr x y;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let setminus x y= Functor_for_sets.setminus tr x y;;
let sort l= Functor_for_sets.sort tr l;;
