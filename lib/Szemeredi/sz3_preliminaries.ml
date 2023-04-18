(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type finite_set = Sz3_types.finite_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t ;; 

type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type mold = Sz3_types.mold = M of (solution list) * extension_data ;;


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

module Find_constraint = struct 

module Private = struct 

let rec helper_for_exact_width (W w,domain,to_be_treated) =
   match to_be_treated with 
   [] -> None 
   |p::others ->
      if p<=2*w then None else 
      if i_is_included_in [p-2*w;p-w] domain 
      then Some [p-2*w;p-w;p]
      else helper_for_exact_width (W w,domain,others) ;;     

let rec helper_with_maximal_width (W w,domain) =
  match helper_for_exact_width (W w,domain,List.rev domain) with 
  Some answer -> Some(W w,answer)
  |None ->
     if w<2 then None else 
    helper_with_maximal_width (W (w-1),domain) ;;  

end ;;  

let with_exact_width (W w) domain = 
  Private.helper_for_exact_width (W w,domain,List.rev domain) ;; 
   

end ;;   

module Finite_int_set = struct 

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  let remove_one_element (FIS(n,scrappers)) k=
       let new_scrappers = i_insert k scrappers in 
       if k <> n then FIS(n,new_scrappers) else 
       let new_z =  to_usual_int_list (FIS(n,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(n,List.filter (fun t->t<new_max) new_scrappers) ;;     
  
  
  (*
  
  remove_one_element (FIS(10,[3;7;8;9])) 10 ;;
  
  *)
  
  
  


end ;;    


 



