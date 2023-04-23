(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type finite_int_set = Sz3_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

type upper_bound_for_constraints = 
    Sz3_types.upper_bound_for_constraints = 
    UBC of int * width ;; 


type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type mold = Sz3_types.mold = M of (solution list) * extension_data ;;

type peek_result = Sz3_types.peek_result =
    P_Success of mold 
   |P_Failure
   |P_Unfinished_computation of (finite_int_set * upper_bound_for_constraints) list ;;


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


module Constraint = struct 

let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  


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
      then Some (C[p-2*w;p-w;p])
      else helper_for_exact_width (W w,domain,others) ;;     

let rec helper_for_maximal_width (W w,domain) =
  match helper_for_exact_width (W w,domain,List.rev domain) with 
  Some answer -> Some(W w,answer)
  |None ->
     if w<2 then None else 
    helper_for_maximal_width (W (w-1),domain) ;;  

 
let natural_upper_bound domain (W w) = 
    match helper_for_maximal_width (W w,domain) with 
     None -> None 
    |Some (W wmax,C l)->
         Some(UBC(List.hd l,W wmax)) ;; 


let with_upper_bound domain (UBC(b,W w)) =
    let u = b + 2*w in 
    let temp1 = List.rev domain in 
    let temp2 = List.filter (fun t->t<=u) temp1 in 
    helper_for_exact_width (W w,domain,temp2) ;;     

end ;;  

(*
let with_exact_width (W w) domain = 
  Private.helper_for_exact_width (W w,domain,List.rev domain) ;; 
*)   

let natural_upper_bound = Private.natural_upper_bound ;; 

let with_maximal_width (W w) domain = 
  Private.helper_for_maximal_width (W w,domain) ;;   

let with_upper_bound = Private.with_upper_bound ;; 
    

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
       let new_z =  to_usual_int_list (FIS(n-1,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;     
  
  
  (*
  
  remove_one_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_one_element (FIS(3,[])) 3 ;;
  
  *)
  
  let head_constraint max_width fis_domain =
    Find_constraint.with_maximal_width max_width (to_usual_int_list fis_domain) ;; 

  let tail_and_head fis_domain =
     let (FIS(n,_scrappers)) = fis_domain in 
     (remove_one_element fis_domain n,n);;

  let decompose_wrt_translation fis_domain = 
    let domain = to_usual_int_list fis_domain in 
    let (d,core_domain) = (match domain with 
      [] -> (0,[])
      | h :: _ -> (h-1, if h=1 then domain else 
                    Image.image (fun x->x-(h-1)) domain 
                   )
    ) in 
    (d,of_usual_int_list core_domain) ;; 

   let natural_upper_bound fis_domain w =
      Find_constraint.natural_upper_bound (to_usual_int_list fis_domain) w;;
   
   let relative_head_constraint fis_domain upper_bound =
    Find_constraint.with_upper_bound (to_usual_int_list fis_domain) upper_bound ;;    

end ;;    


module With_upper_bound = struct 

let remove_one_element (old_fis,upper_bound) k=
   let (UBC(_,W w)) = upper_bound 
   and new_fis = Finite_int_set.remove_one_element old_fis k in 
   let new_bound =(match Finite_int_set.natural_upper_bound new_fis (W w) with 
   None -> upper_bound 
   | Some better_bound -> better_bound
   ) in  
   (new_fis,new_bound) ;; 

let tail_and_head (fis,upper_bound) =
    let n = List.hd(List.rev(Finite_int_set.to_usual_int_list fis)) in 
    remove_one_element (fis,upper_bound) n ;;   

end ;;   


