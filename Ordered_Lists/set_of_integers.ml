(* 

#use"Ordered_Lists/set_of_integers.ml";;s

*)

let tr = ((fun x->Set_of_integers_t.S(x)),(fun (Set_of_integers_t.S(x))->x),Total_ordering.standard);;


let forget_order x= Functor_for_sets.forget_order tr x;;
let image f x= Functor_for_sets.image tr f x;;
let mem a x= Functor_for_sets.mem tr a x;;
let merge l= Functor_for_sets.merge tr l;;
let safe_set l= Functor_for_sets.safe_set tr l;;
let singleton a= Functor_for_sets.singleton tr a;;
let sort l= Functor_for_sets.sort tr l;;
let unsafe_set l= Functor_for_sets.unsafe_set tr l;;





(*
type set=int Ordered.old_set;;
let cmp=(Total_ordering.standard:>(int Total_ordering.t));;let lt x y=x<y;;
let unsafe_set=(Ordered.unsafe_set:>(int list-> set));;
let forget_order=(Ordered.forget_order:>(set->int list));;

let kreskus_strizh x=Ordered.is_increasing cmp x;;
let kreskus x=Ordered.is_nondecreasing cmp x;;

let mem=((fun a ox->Ordered.mem cmp a ox):>(int->set->bool));;
let merge=((fun ox oy->Ordered.merge cmp ox oy):>( set->set->set));;
let sort=((fun x->Ordered.sort cmp x):>(int list->set));;
let lemel=((fun ox oy->Ordered.setminus cmp ox oy):>(set->set->set));;
let ental=((fun ox oy->Ordered.is_included_in cmp ox oy):>(set->set->bool));;
let kengeij=((fun ox oy->Ordered.intersect cmp ox oy):>set->set->set);;
let kengeij_goullo=((fun ox oy->Ordered.does_not_intersect cmp ox oy):>set->set->bool);;
let min=((fun x->Ordered.min cmp x):>int list->int);;
let max=((fun x->Ordered.max cmp x):>int list->int);;

let hd ox=List.hd(forget_order ox);;
let image f ox=Image.image f (forget_order ox);;
let rev_map f ox=Image.image f (forget_order ox);;
let empty_set=unsafe_set [];;
let singleton x=unsafe_set [x];;
let filter f x=unsafe_set(List.filter(f)(forget_order x));;
let partition f ox=
         match List.partition(f)(forget_order ox) with
           (u,v)->(unsafe_set u,unsafe_set v);;
let length x=List.length(forget_order x);;

let nelfenn a ox=not(mem a ox);;
let nental ox oy=not(ental ox oy);;

let insert x oy=merge(singleton x) oy;;
let safe_set x=if kreskus_strizh(x) then unsafe_set(x) else sort(x);;
let outsert x oy=lemel(oy)(singleton x);;
let delta_set ox oy=merge(lemel ox oy)(lemel oy ox);;
let delta_distance ox oy=length(delta_set ox oy);;


let big_teuzin x=List.fold_left merge empty_set x;;
let big_kengeij=function
    []->failwith("empty intersection undefined")
    |a::b->List.fold_left(kengeij)(a)(b);;
    
    
let expand_boolean_algebra=
 ((fun x->Ordered.expand_boolean_algebra cmp x):>(set list->(set list)));; 
 
 
*)