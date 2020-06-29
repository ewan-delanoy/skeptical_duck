(*
 
#use"functor_for_sets.ml";;

Here all the possible dependencies are defined. Each particular
instance defines only the value it needs.

*)


type ('a,'b) parameter = (('a list) -> 'b) * ('b -> ('a list)) * ('a Total_ordering.t);;

let does_not_intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Ordered.does_not_intersect cmpr (deco ox) (deco oy);;
    
let empty_set ((co,deco,cmpr):('a,'b) parameter) = co [];;

let fold_merge ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Ordered.fold_merge cmpr (Image.imagination deco l));;

let fold_intersect ((co,deco,cmpr):('a,'b) parameter) 
     l=co (Ordered.fold_intersect cmpr (Image.imagination deco l));;
    
let forget_order ((co,deco,cmpr):('a,'b) parameter) =deco;;

let hd ((co,deco,cmpr):('a,'b) parameter) ox= List.hd(deco ox);;

let image ((co,deco,cmpr):('a,'b) parameter) f ox= Image.imagination f (deco ox);;

let insert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Ordered.insert cmpr x (deco oy));;

let intersect ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Ordered.intersect cmpr (deco ox) (deco oy));;

let is_included_in ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= Ordered.is_included_in cmpr (deco ox) (deco oy);;

let length ((co,deco,cmpr):('a,'b) parameter) ox= List.length(deco ox);;

let mem ((co,deco,cmpr):('a,'b) parameter) 
     x oy= Ordered.mem cmpr x (deco oy);;

let merge ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Ordered.merge cmpr (deco ox) (deco oy));;

let nmem ((co,deco,cmpr):('a,'b) parameter) 
     x oy= not(Ordered.mem cmpr x (deco oy));;

let outsert ((co,deco,cmpr):('a,'b) parameter) 
     x oy= co(Ordered.outsert cmpr x (deco oy));;

let safe_set ((co,deco,cmpr):('a,'b) parameter) 
     l= co(Ordered.safe_set cmpr l);;

let setminus ((co,deco,cmpr):('a,'b) parameter) 
     ox oy= co(Ordered.setminus cmpr (deco ox) (deco oy));;

let singleton ((co,deco,cmpr):('a,'b) parameter)  x=co[x];;

let sort ((co,deco,cmpr):('a,'b) parameter) 
     l= co(Ordered.sort cmpr l);;

let tl ((co,deco,cmpr):('a,'b) parameter) ox= co(List.tl(deco ox));;

let unsafe_set ((co,deco,cmpr):('a,'b) parameter) 
     l= co l;;

