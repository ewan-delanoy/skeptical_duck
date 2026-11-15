(* 

#use"lib/Ordered_Lists/set_of_strings.ml";;

*)

let tr = 
  let order = Total_ordering.silex_for_strings in 
  ((fun x->Set_of_strings_t.S(Ordered.to_list x)),(fun (Set_of_strings_t.S(x))->Ordered.safe_set order x),order);;


let forget_order x= Functor_for_sets.forget_order tr x;;
let image f x= Functor_for_sets.image tr f x;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let sort l= Functor_for_sets.sort tr l;;



