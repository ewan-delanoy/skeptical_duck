(*
 
#use"lib/Ordered_Lists/functor_for_sets.ml";;

Here all the possible dependencies are defined. Each particular
instance defines only the value it needs.

*)


type ('a,'b) parameter = (('a list) -> 'b) * ('b -> ('a list)) * ('a Total_ordering_t.t);;


let does_not_intersect ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= Ordered.does_not_intersect cmpr (deco ox) (deco oy);;
    
let empty_set ((co,_deco,_cmpr):('a,'b) parameter) = co [];;

let fold_merge ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Ordered.fold_merge cmpr (Image.image deco l));;

let fold_intersect ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Ordered.fold_intersect cmpr (Image.image deco l));;
    
let forget_order ((_co,deco,_cmpr):('a,'b) parameter) =deco;;

let hd ((_co,deco,_cmpr):('a,'b) parameter) ox= List.hd(deco ox);;

let image ((_co,deco,_cmpr):('a,'b) parameter) f ox= Image.image f (deco ox);;

let insert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Ordered.insert cmpr x (deco oy));;

let intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Ordered.intersect cmpr (deco ox) (deco oy));;

let intersects ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= Ordered.intersects cmpr (deco ox) (deco oy);;

let is_included_in ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= Ordered.is_included_in cmpr (deco ox) (deco oy);;

let length ((_co,deco,_cmpr):('a,'b) parameter) ox= List.length(deco ox);;

let max ((_co,deco,_cmpr):('a,'b) parameter) ox= List.hd(List.rev (deco ox));;

let mem ((_co,deco,cmpr):('a,'b) parameter) 
     x oy= Ordered.mem cmpr x (deco oy);;

let merge ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Ordered.merge cmpr (deco ox) (deco oy));;

let min ((_co,deco,_cmpr):('a,'b) parameter) ox= List.hd(deco ox);;

let nmem ((_co,deco,cmpr):('a,'b) parameter) 
     x oy= not(Ordered.mem cmpr x (deco oy));;

let outsert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Ordered.outsert cmpr x (deco oy));;

let safe_set ((co,_deco,cmpr):('a,'b) parameter) 
     l= co(Ordered.safe_set cmpr l);;

let select_minimal_elements_for_inclusion 
  ((co,deco,cmpr):('a,'b) parameter) ll  
   = 
     let old_form = Image.image deco ll in
     let new_form = Ordered.select_minimal_elements_for_inclusion cmpr old_form in 
     Image.image co new_form;;

let setminus ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Ordered.setminus cmpr (deco ox) (deco oy));;

let singleton ((co,_deco,_cmpr):('a,'b) parameter)  x=co[x];;

let size_of_intersection ((_co,deco,cmpr):('a,'b) parameter) 
     ox oy= List.length(Ordered.intersect cmpr (deco ox) (deco oy));;

let sort ((co,_deco,cmpr):('a,'b) parameter) 
     l= co(Ordered.sort cmpr l);;

let tl ((co,deco,_cmpr):('a,'b) parameter) ox= co(List.tl(deco ox));;

let unsafe_set ((co,_deco,_cmpr):('a,'b) parameter) 
     l= co l;;

