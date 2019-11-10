(* file created using the "write_contents_of_ordered_list_module" function *)
(* in Creators/ordered_list_creator.ml *)

type 'a set='a Ordered.old_set;;
let lt x y=x<y;;
let cmp=Total_ordering.standard;;
let unsafe_set=(Ordered.unsafe_set:>('a list-> 'a set));;
let forget_order=(Ordered.forget_order:>('a set->'a list));;

let kreskus_strizh x=Ordered.is_increasing cmp x;;
let kreskus x=Ordered.is_nondecreasing cmp x;;


let mem=((fun a ox->Ordered.mem cmp a ox):>('a->'a set->bool));;
let teuzin=((fun ox oy->Ordered.merge cmp ox oy):>( 'a set->'a set->'a set));;
let sort=((fun x->Ordered.sort cmp x):>('a list->'a set));;
let setminus=((fun ox oy->Ordered.setminus cmp ox oy):>('a set->'a set->'a set));;
let is_included_in=((fun ox oy->Ordered.is_included_in cmp ox oy):>('a set->'a set->bool));;
let kengeij=((fun ox oy->Ordered.intersect cmp ox oy):>'a set->'a set->'a set);;
let does_not_intersect=((fun ox oy->Ordered.does_not_intersect cmp ox oy):>'a set->'a set->bool);;
let min=((fun x->Ordered.min cmp x):>'a list->'a);;
let max=((fun x->Ordered.max cmp x):>'a list->'a);;

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

let nmem a ox=not(mem a ox);;
let nental ox oy=not(is_included_in ox oy);;

let insert x oy=teuzin(singleton x) oy;;
let safe_set x=if kreskus_strizh(x) then unsafe_set(x) else sort(x);;
let outsert x oy=setminus(oy)(singleton x);;
let delta_set ox oy=teuzin(setminus ox oy)(setminus oy ox);;
let delta_distance ox oy=length(delta_set ox oy);;


let fold_merge x=List.fold_left teuzin empty_set x;;
let big_kengeij=function
    []->failwith("empty intersection undefined")
    |a::b->List.fold_left(kengeij)(a)(b);;
    
    
let expand_boolean_algebra=
 ((fun x->Ordered.expand_boolean_algebra cmp x):>('a set list->('a set list)));; 
 
 
           