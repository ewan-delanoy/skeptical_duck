(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type finite_set = Sz3_types.finite_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

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
      then Some (C[p-2*w;p-w;p])
      else helper_for_exact_width (W w,domain,others) ;;     

let rec helper_for_maximal_width (W w,domain) =
  match helper_for_exact_width (W w,domain,List.rev domain) with 
  Some answer -> Some(W w,answer)
  |None ->
     if w<2 then None else 
    helper_for_maximal_width (W (w-1),domain) ;;  

end ;;  

(*
let with_exact_width (W w) domain = 
  Private.helper_for_exact_width (W w,domain,List.rev domain) ;; 
*)   

let with_maximal_width (W w) domain = 
  Private.helper_for_maximal_width (W w,domain) ;;   

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

end ;;    

exception Get_below_exn of int * finite_set ;;

module Level1 = struct 

let current_width = 1 ;; 

let no_constraint_opt fis_domain = 
   match Finite_int_set.head_constraint (W current_width) fis_domain with 
   Some cstr -> (None,Some cstr)
  |None -> let domain = Finite_int_set.to_usual_int_list fis_domain in 
           (Some(M([domain],domain)),None);; 


let force_get_below fis_domain = raise(Get_below_exn(0,fis_domain));;

let main_hashtbl = Hashtbl.create 50 ;; 

let access_opt fis_domain =
   match fst(no_constraint_opt fis_domain) with 
   Some answer -> Some answer 
   | None -> Hashtbl.find_opt main_hashtbl fis_domain ;; 

let access_with_helper_opt fis_domain helper =
   match List.assoc_opt fis_domain helper with 
   Some answer -> Some answer 
   | None -> access_opt fis_domain ;; 

let partial_pusher_in_computation helper fis_domain =
    let domain = Finite_int_set.to_usual_int_list fis_domain in 
    let opt = Finite_int_set.head_constraint (W current_width) fis_domain in 
    match opt with 
     None -> 
            (Some(M([domain],domain)),None)
    |Some (W w,C _cstr) -> 
        (match  access_with_helper_opt fis_domain helper with 
          Some answer -> (Some answer,opt) 
          | None -> 
            if w<current_width 
            then (Some(force_get_below fis_domain),opt)   
            else (None,opt)
        );;

let analysis_on_tail sols2 ext2 n = 
  let interval = Int_range.range 1 current_width in 
    let is_admissible = (fun x->List.for_all(fun d->
           not(i_is_included_in [n-(2*d);n-d] x) 
    ) interval) in 
    if not(is_admissible ext2)
    then Some(Some(M(sols2,[])),true,[])  
    else 
    let sols3 = List.filter_map (fun sol->
             if is_admissible sol 
             then Some(sol@[n]) 
             else None    
    ) sols2 in 
    if sols3 <> [] 
    then Some(Some(M(sols3,ext2@[n])),true,[])  
    else None
  ;;


let fork_analysis helper fis_domain  cstr = 
  let candidates = Image.image (
     fun i-> Finite_int_set.remove_one_element fis_domain i
  ) cstr in 
  let candidates2 = Image.image (
    fun cand ->(cand,fst(partial_pusher_in_computation helper fis_domain))
  ) candidates in 
  let bad_ones = List.filter (
    fun (_cand,opt) -> opt<>None
  ) candidates2 in 
  if bad_ones <> []
  then (None,false,Image.image fst bad_ones)
  else 
  let candidates3 = Image.image (
   fun (cand,opt) ->(cand,Option.get opt)
  ) candidates2 in     
  let lengths = Image.image (fun (_cand,M(sols,_ext))->
      List.length(List.hd sols)) candidates3 in 
  let indexed_lengths = Int_range.index_everything lengths in 
  let (min1,min_indices) = Min.minimize_it_with_care snd indexed_lengths 
  and (max1,max_indices) = Max.maximize_it_with_care snd indexed_lengths in 
if min1 = max1 
then let (M(sols4,_)) = snd(List.hd(List.rev candidates3)) in 
    (Some (M(sols4,[])),true,[])
else let (max_idx,_) = List.hd(List.rev max_indices) in 
    let (M(sols5,_)) = snd(List.nth candidates3 (max_idx-1) ) in  
    let ext5 = Image.image (fun (k,_)->List.nth cstr (k-1)) min_indices in 
    (Some (M(sols5,ext5)),true,[]);;    


let end_of_full_pusher 
       helper fis_domain fis_tail n 
       opt_easy_case2 opt_cstr_data = 
  match opt_easy_case2 with 
   None -> (None,false,[fis_tail])
  |Some(M(sols2,ext2)) ->
    let opt1 = analysis_on_tail sols2 ext2 n in 
    if opt1<>None then Option.get opt1 else
    let (_,C cstr) = Option.get opt_cstr_data in 
    fork_analysis helper fis_domain  cstr;;    

let full_pusher_in_computation helper fis_domain =
  let (opt_easy_case,opt_cstr_data) =  partial_pusher_in_computation helper fis_domain in 
  match opt_easy_case with 
   Some(answer) -> (Some answer,false,[])
  |None -> 
     let (fis_tail,n) = Finite_int_set.tail_and_head fis_domain in 
     let (opt_easy_case2,_opt_cstr_data2) =  partial_pusher_in_computation helper fis_tail in 
     end_of_full_pusher 
       helper fis_domain fis_tail n 
       opt_easy_case2 opt_cstr_data ;;

       
let rec helper_for_needed_computations (helper,to_be_treated)= match to_be_treated with 
    [] -> List.rev(helper)
   | fis_domain :: others -> 
      let (opt_answer,should_remember,to_be_treated_later) = 
         full_pusher_in_computation helper fis_domain in 
      match opt_answer with 
      None ->
        helper_for_needed_computations (helper,to_be_treated_later@others)
     |Some answer -> 
       let new_helper = 
          (if should_remember then (fis_domain,answer)::helper else helper) in 
       helper_for_needed_computations (new_helper,others) 
       ;;      
   
 let needed_computations fis_domain =
  helper_for_needed_computations ([],[fis_domain]) ;;

end ;;  


 



