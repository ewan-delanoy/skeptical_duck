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

type key = finite_int_set * upper_bound_for_constraints ;;  
  
type peek_result = Sz3_types.peek_result =
    P_Success of mold 
   |P_Failure
   |P_Unfinished_computation of key list ;;


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

module Mold = struct 

let translate d (M(sols,ext)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    M(Image.image tr sols,tr ext) ;; 

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
    match helper_for_exact_width (W w,domain,temp2) with 
    Some (C l) -> Some(C l,UBC(List.hd l,W w)) 
   |None ->
     (
        match helper_for_maximal_width (W (w-1),domain) with 
        Some(W w2,C l2) -> Some(C l2,UBC(List.hd l2,W w2)) 
        | None -> None
     );;     

 
let is_admissible upper_bound candidate = 
    ((with_upper_bound candidate upper_bound)=None);;


end ;;  


(*
let with_exact_width (W w) domain = 
  Private.helper_for_exact_width (W w,domain,List.rev domain) ;; 
*)   

let is_admissible = Private.is_admissible ;; 

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

let decompose_wrt_translation (old_fis,UBC(b,W w)) = 
   let (d,new_fis) = Finite_int_set.decompose_wrt_translation old_fis in 
   (d,(new_fis,UBC(b-d,W w))) ;;


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
    (n,remove_one_element (fis,upper_bound) n) ;;   

let usual_pair (n,scrappers,W w) =
    let fis = FIS(n,scrappers) in
    let upper_bound =(match Finite_int_set.natural_upper_bound fis (W w) with 
     None -> UBC(max(1)(n-2*w),W w)
    |Some answer -> answer
    ) in 
    (fis,upper_bound) ;;
end ;;   

exception Get_below_exn of int * (finite_int_set * upper_bound_for_constraints) ;;
exception Compute_fast_exn of int * (finite_int_set * upper_bound_for_constraints) ;;
exception Using_translation_exn of int ;;
exception Peek_for_cumulative_case_should_never_happen_1_exn of int ;; 
exception Peek_for_fork_case_should_never_happen_1_exn of int ;;
exception Multiple_peek_exn of int ;; 
exception Pusher_for_needed_subcomputations_exn_1 of int ;; 
exception Pusher_for_needed_subcomputations_exn_2 of int ;;  
exception Add_exn of  int * (finite_int_set * upper_bound_for_constraints) ;;
exception Bad_remainder_by_three of int ;; 
exception Import_exn of int * (int * (int list)) ;;

module Level1 = struct 

  let current_width = 1 ;; 
  
  let main_hashtbl = ((Hashtbl.create 50) : (finite_int_set * upper_bound_for_constraints, mold) Hashtbl.t) ;;  
  
  let simpler_without_upper_bound fis =
    let domain = Finite_int_set.to_usual_int_list fis in 
    let intervals = Arithmetic_list.decompose_into_connected_components domain in 
    let sol_components = Image.image (
      fun (a,b) ->
        List.filter(fun k->((k-a+1) mod 3)<>0)(Int_range.range a b)
    ) intervals 
    and forced_elements = Image.image (
      fun (a,b) ->
        match ((b-a+1) mod 3) with 
         0 -> []
        |1 -> List.filter(fun k->List.mem ((k-a+1) mod 3) [1])(Int_range.range a b)
        |2 -> List.filter(fun k->List.mem ((k-a+1) mod 3) [1;2])(Int_range.range a b)
        |_ -> raise(Bad_remainder_by_three(current_width)) 
    ) intervals in 
    M([List.flatten sol_components],List.flatten forced_elements);;

  let simpler (fis1,UBC(b,W w0)) =
    if w0>1 then simpler_without_upper_bound fis1 else  
    let domain1 = Finite_int_set.to_usual_int_list fis1 in 
    let (domain2,extra) = List.partition (fun t->t<=(b+2)) domain1 in 
    let fis2 = Finite_int_set.of_usual_int_list domain2 in 
    let (M(sols2,ext2))  = simpler_without_upper_bound fis2 in 
    M(Image.image (fun sol->sol@extra) sols2,ext2@extra);; 

  let compute_fast_opt fis_with_ub =
    match Hashtbl.find_opt main_hashtbl fis_with_ub with 
    (Some answer) -> Some answer 
    | None -> Some(simpler fis_with_ub);;

   let compute_reasonably_fast_opt fis_with_ub = 
    compute_fast_opt fis_with_ub ;;     

   

end ;;  

module Level2 = struct 

  let current_width = 2 ;; 
  
  let force_get_below fis_with_ub = 
    match Level1.compute_reasonably_fast_opt fis_with_ub with 
    Some answer -> answer  
    |None -> raise(Get_below_exn(current_width-1,fis_with_ub));;
  
  let main_hashtbl = ((Hashtbl.create 50) : (finite_int_set * upper_bound_for_constraints, mold) Hashtbl.t) ;; 
  
  let peek_for_obvious_accesses helper fis_with_ub = 
    match List.assoc_opt fis_with_ub helper with 
      Some answer1 -> (P_Success(answer1),false) 
    | None ->
       (
          match  Hashtbl.find_opt main_hashtbl fis_with_ub with 
          Some answer2 -> (P_Success(answer2),false)
        | None -> 
          let (fis,upper_bound) = fis_with_ub in 
         (match Finite_int_set.relative_head_constraint fis upper_bound with 
          None -> let domain = Finite_int_set.to_usual_int_list fis in 
                   (P_Success(M([domain],domain)),false)
         |Some (cstr,_) ->   
            let (W w) = Constraint.width cstr in
            if w<current_width 
            then (P_Success(force_get_below (fis,upper_bound)),true)   
            else (P_Failure,false)          
         )
       ) ;; 
  
  (*
   
  We use translations as little as possible. Most of the functions
  of this module are supposed to work on arguments where translation
  does not apply. The function below is an exception.
  
  *)

  

  let seek_obvious_accesses_using_translation 
      helper original_fis_with_ub = 
    let (d,translated_fis_with_ub) = 
        With_upper_bound.decompose_wrt_translation original_fis_with_ub in 
    let (peek_res,_) = peek_for_obvious_accesses helper translated_fis_with_ub in 
        match peek_res with 
       P_Unfinished_computation(_)  -> raise(Using_translation_exn(current_width))
      |P_Failure -> (None,Some translated_fis_with_ub)
      |P_Success(translated_answer) ->
         let answer_to_original = Mold.translate d translated_answer in 
         (Some(answer_to_original),None);; 

  
  let peek_for_cumulative_case helper old_fis_with_ub = 
      let (n,new_fis_ub) = With_upper_bound.tail_and_head old_fis_with_ub in 
      let (peek_res,_) = peek_for_obvious_accesses helper new_fis_ub in 
        match peek_res with 
       P_Unfinished_computation(_)  -> raise(Peek_for_cumulative_case_should_never_happen_1_exn(current_width))
      |P_Failure -> P_Unfinished_computation([new_fis_ub]) 
      |P_Success(M(sols2,ext2)) ->
        let (_,old_ub) = old_fis_with_ub in 
        if not(Find_constraint.is_admissible old_ub (ext2@[n]))
        then P_Success(M(sols2,[]))
        else
        let sols3 = List.filter_map (fun sol->
                    if Find_constraint.is_admissible old_ub (sol@[n]) 
                    then Some(sol@[n]) 
                    else None    
        ) sols2 in 
        if sols3 <> [] 
        then P_Success(M(sols3,ext2@[n]))  
        else P_Failure
    ;;

let partition_leaves_in_fork_case helper leaves =
  let leaves2 = Image.image (
    fun cand ->
      let (peek_res,_) = peek_for_obvious_accesses helper cand in 
      let res_opt = (
          match peek_res with 
            P_Success (mold) -> Some mold
          | P_Failure
          | P_Unfinished_computation (_) -> None
      ) in 
      (cand,res_opt)
  ) leaves in 
  let (bad_leaves,good_leaves) = 
    List.partition (fun (_,opt) -> opt = None ) leaves2 in 
  (Image.image (fun (cand, opt)->(cand,Option.get opt)) good_leaves,
   Image.image (fun (cand,_opt)-> cand                ) bad_leaves) ;; 

let partition_leaves_in_fork_case helper leaves =
  let leaves2 = Image.image (
      fun cand ->
        (cand,seek_obvious_accesses_using_translation helper cand)
  ) leaves in 
  let (bad_leaves,good_leaves) = 
      List.partition (fun (_,(_,opt_bad)) -> opt_bad = None ) leaves2 in 
  (Image.image (fun ( cand,( opt_good,_opt_bad)) -> (cand,Option.get opt_good)) good_leaves,
   Image.image (fun (_cand,(_opt_good, opt_bad)) -> Option.get opt_bad        ) bad_leaves) ;; 

let peek_for_fork_case helper old_fis_with_ub = 
  let (fis,upper_bound) = old_fis_with_ub in 
  let opt1 = Finite_int_set.relative_head_constraint fis upper_bound in 
  if opt1=None  
  then raise(Peek_for_fork_case_should_never_happen_1_exn(current_width))
  else    
  let (C cstr_l,_) = Option.get opt1 in    
  let candidates = Image.image (
         fun i-> With_upper_bound.remove_one_element old_fis_with_ub i
  ) cstr_l in 
  let (candidates2,bad_ones) = partition_leaves_in_fork_case helper candidates in 
  if bad_ones <> []
  then P_Unfinished_computation(bad_ones)
  else   
  let lengths = Image.image (fun (_cand,M(sols,_ext))->
          List.length(List.hd sols)) candidates2 in 
  let indexed_lengths = Int_range.index_everything lengths in 
  let (min1,min_indices) = Min.minimize_it_with_care snd indexed_lengths 
  and (max1,max_indices) = Max.maximize_it_with_care snd indexed_lengths in 
  if min1 = max1 
  then let (M(sols4,_)) = snd(List.hd(List.rev candidates2)) in 
         P_Success(M(sols4,[]))
  else let (max_idx,_) = List.hd(List.rev max_indices) in 
        let (M(sols5,_)) = snd(List.nth candidates2 (max_idx-1) ) in  
        let ext5 = Image.image (fun (k,_)->List.nth cstr_l (k-1)) min_indices in 
        P_Success(M(sols5,ext5));;    


  let multiple_peek helper old_fis_with_ub = 
    let (peek_res1,to_be_remembered) = peek_for_obvious_accesses helper old_fis_with_ub in 
      match peek_res1 with 
      P_Success (_) -> (peek_res1,to_be_remembered)
    | P_Unfinished_computation (_) -> (peek_res1,false)  
    | P_Failure ->
    let peek_res2= peek_for_cumulative_case helper old_fis_with_ub in 
      match peek_res2 with 
      P_Success (_) -> (peek_res2,true)
    | P_Unfinished_computation (_) -> (peek_res2,false)  
    | P_Failure -> 
    let peek_res3= peek_for_fork_case helper old_fis_with_ub in 
      match peek_res3 with 
      P_Success (_) -> (peek_res3,true)
    | P_Unfinished_computation (_) -> (peek_res3,false)  
    | P_Failure -> raise(Multiple_peek_exn(current_width)) ;; 
        

  let pusher_for_needed_subcomputations (helper,to_be_treated) =
      match to_be_treated with 
       [] -> raise (Pusher_for_needed_subcomputations_exn_1(current_width)) 
      |fis_with_ub :: others ->
        let (peek_res,to_be_remembered)= multiple_peek helper fis_with_ub in
        (
          match peek_res with 
          P_Failure -> raise (Pusher_for_needed_subcomputations_exn_2(current_width))
        | P_Unfinished_computation (new_to_be_treated) -> 
             (helper,new_to_be_treated@to_be_treated)
        | P_Success (answer) -> 
            let new_helper =(
               if to_be_remembered 
               then (fis_with_ub,answer) :: helper 
               else helper 
            ) in 
            (new_helper,others)
        )  ;;     

   let rec iterator_for_needed_subcomputations walker = 
      if snd walker = [] then List.rev(fst walker) else 
      let new_walker = pusher_for_needed_subcomputations walker in      
      iterator_for_needed_subcomputations new_walker ;;

   let needed_subcomputations items = 
    iterator_for_needed_subcomputations ([],items) ;;  
    
   let compute_fast_opt fis_with_ub =
    let (peek_res,_) =multiple_peek [] fis_with_ub in
      match peek_res with 
      P_Success (answer) -> Some answer
    | P_Unfinished_computation (_)  
    | P_Failure -> None ;;

   let compute_reasonably_fast_opt fis_with_ub = 
    compute_fast_opt fis_with_ub ;;     

   let add fis_with_ub = 
      match compute_reasonably_fast_opt fis_with_ub with 
      None -> raise(Add_exn(current_width,fis_with_ub))
    |Some answer ->
        let _ = Hashtbl.replace main_hashtbl fis_with_ub answer in 
        answer ;; 

   let add_usual (n,scrappers) =
       add (With_upper_bound.usual_pair (n,scrappers,W current_width));;     

   let import (n,scrappers) =
     let (fis,ub) = With_upper_bound.usual_pair (n,scrappers,W current_width) in 
     let (M(sols,ext))=force_get_below (fis,ub) in 
     let sols2 = List.filter (Find_constraint.is_admissible ub) sols in 
     if sols2 = []
     then raise(Import_exn(current_width,(n,scrappers)))
     else 
    let answer = M(sols2,ext) in 
    let _ = Hashtbl.replace main_hashtbl (fis,ub) answer in 
    answer ;; 


end ;;  