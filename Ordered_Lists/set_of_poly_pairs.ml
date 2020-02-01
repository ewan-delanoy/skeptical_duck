(* 

#use"Ordered_Lists/set_of_poly_pairs.ml";;s

*)

let tr = ((fun x->Set_of_poly_pairs_t.S(x)),(fun (Set_of_poly_pairs_t.S(x))->x),Total_ordering.standard2);;

let safe_set l= Functor_for_sets.safe_set tr l;;
let sort l= Functor_for_sets.sort tr l;;
