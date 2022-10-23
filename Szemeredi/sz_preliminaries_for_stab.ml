(*

#use"Szemeredi/sz_preliminaries_for_stab.ml";;

*)

let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_setminus = Ordered.setminus i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let t_order = Total_ordering.triple_product 
   i_order i_order (Total_ordering.silex_for_intlists) ;;


module Parameter_pair_for_obstruction = struct 

  let predecessor max_in_set (width,breadth) = 
    if breadth < 1 
    then (if width < 2 then None else Some(width-1,max_in_set-2*(width-1)) )  
    else (Some(width,breadth-1)) ;;
    
  let check_for_meaningful_obstruction (width,breadth) domain =
     if breadth < 1 
     then false 
     else Ordered.is_included_in 
           Total_ordering.for_integers 
         [breadth;breadth+width;breadth+2*width] domain ;;  
  
end ;;  

module Finite_int_set = struct 

  let of_pair (n,scrappers) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let to_pair domain =
       if domain = [] then (0,[]) else 
       let n = List.hd(List.rev domain) in 
       (n,i_setminus (Int_range.range 1 n) domain) ;;   
  
end ;;    

module Point = struct 
  type t = Sz_types.point = P of int * int * int * int list ;;  
  let width (P(w,b,n,s)) = w ;;
  let breadth (P(w,b,n,s)) = b ;;
  let size (P(w,b,n,s)) = n ;;
  let scrappers (P(w,b,n,s)) = s ;;
  let unveil (P(w,b,n,s)) = (w,b,n,s) ;;
  let enumerate_supporting_set (P(w,b,n,s)) = Finite_int_set.of_pair (n,s) ;; 

end ;;  