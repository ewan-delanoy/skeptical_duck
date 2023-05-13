(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type breadth = Sz3_types.breadth = B of int ;; 

type finite_int_set = Sz3_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type mold = Sz3_types.mold = M of (solution list) * extension_data ;;

type upper_bound_on_breadth = 
    Sz3_types.upper_bound_on_breadth = 
   Unrestricted |Up_to of breadth ;; 

type upper_bound_on_constraint = 
   Sz3_types.upper_bound_on_constraint = UBC of width * upper_bound_on_breadth ;; 

type key = finite_int_set * upper_bound_on_constraint ;; 

type severity = Sz3_types.severity = Stern | Relaxed ;; 

type old_upper_bound_for_constraints = 
    Sz3_types.old_upper_bound_for_constraints = 
    Old_UBC of int * width ;; 

type old_key = finite_int_set * old_upper_bound_for_constraints ;; 

type old_peek_result = Sz3_types.old_peek_result =
    Old_P_Success of mold 
   |Old_P_Failure
   |Old_P_Unfinished_computation of old_key list ;;

type breadth_range = Sz3_types.breadth_range = Br_Unrestricted |Br_Up_to of int ;;

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


module Old_dnif_constraint = struct 

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
         Some(Old_UBC(List.hd l,W wmax)) ;; 


let with_upper_bound domain (Old_UBC(b,W w)) =
    let u = b + 2*w in 
    let temp1 = List.rev domain in 
    let temp2 = List.filter (fun t->t<=u) temp1 in 
    match helper_for_exact_width (W w,domain,temp2) with 
    Some (C l) -> Some(C l,Old_UBC(List.hd l,W w)) 
   |None ->
     (
        match helper_for_maximal_width (W (w-1),domain) with 
        Some(W w2,C l2) -> Some(C l2,Old_UBC(List.hd l2,W w2)) 
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
    Old_dnif_constraint.with_maximal_width max_width (to_usual_int_list fis_domain) ;; 

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
      Old_dnif_constraint.natural_upper_bound (to_usual_int_list fis_domain) w;;
   
   let relative_head_constraint fis_domain upper_bound =
    Old_dnif_constraint.with_upper_bound (to_usual_int_list fis_domain) upper_bound ;;    

end ;;    


module With_upper_bound = struct 

let decompose_wrt_translation (old_fis,Old_UBC(b,W w)) = 
   let (d,new_fis) = Finite_int_set.decompose_wrt_translation old_fis in 
   (d,(new_fis,Old_UBC(b-d,W w))) ;;


let remove_one_element (old_fis,upper_bound) k=
   let (Old_UBC(_,W w)) = upper_bound 
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
     None -> Old_UBC(max(1)(n-2*w),W w)
    |Some answer -> answer
    ) in 
    (fis,upper_bound) ;;
end ;;   

exception Get_below_exn of int * (finite_int_set * old_upper_bound_for_constraints) ;;
exception Compute_fast_exn of int * (finite_int_set * old_upper_bound_for_constraints) ;;
exception Using_translation_exn of int ;;
exception Peek_for_cumulative_case_should_never_happen_1_exn of int ;; 
exception Peek_for_fork_case_should_never_happen_1_exn of int ;;
exception Multiple_peek_exn of int ;; 
exception Simplified_multiple_peek_exn of int ;;
exception Pusher_for_needed_subcomputations_exn_1 of int ;; 
exception Pusher_for_needed_subcomputations_exn_2 of int ;;  
exception Add_exn of  int * (finite_int_set * old_upper_bound_for_constraints) ;;
exception Bad_remainder_by_three of int ;; 
exception Import_no_presolution_exn of int * (int * (int list)) ;;
exception Import_bad_presolution_exn of int * (int * (int list)) ;;



module Old_Level1 = struct 

  let current_width = 1 ;; 
  
  let main_hashtbl = ((Hashtbl.create 50) : (finite_int_set * old_upper_bound_for_constraints, mold) Hashtbl.t) ;;  
  
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

  let simpler (fis1,Old_UBC(b,W w0)) =
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


(* Beginning of Old_Level2 *)
module Old_Level2 = struct 

  let current_width = 2 ;; 
  
  let get_below (_hshtbl,severity) fis_with_ub = 
    match Old_Level1.compute_reasonably_fast_opt fis_with_ub with 
    Some answer -> (Old_P_Success(answer),true)  
    |None -> 
      match severity with  
       Stern -> raise(Get_below_exn(current_width-1,fis_with_ub))
      |Relaxed -> (Old_P_Unfinished_computation[fis_with_ub],false);;

  
  let peek_for_obvious_accesses (hashtbl,severity) helper fis_with_ub = 
    match List.assoc_opt fis_with_ub helper with 
      Some answer1 -> (Old_P_Success(answer1),false) 
    | None ->
       (
          match  Hashtbl.find_opt hashtbl (W current_width,fis_with_ub) with 
          Some answer2 -> (Old_P_Success(answer2),false)
        | None -> 
          let (fis,upper_bound) = fis_with_ub in 
         (match Finite_int_set.relative_head_constraint fis upper_bound with 
          None -> let domain = Finite_int_set.to_usual_int_list fis in 
                   (Old_P_Success(M([domain],domain)),false)
         |Some (cstr,_) ->   
            let (W w) = Constraint.width cstr in
            if w<current_width 
            then get_below (hashtbl,severity) (fis,upper_bound)
            else (Old_P_Failure,false)          
         )
       ) ;; 
  
  (*
   
  We use translations as little as possible. Most of the functions
  of this module are supposed to work on arguments where translation
  does not apply. The function below is an exception.
  
  *)

  

  let seek_obvious_accesses_using_translation 
    (hashtbl,severity) helper original_fis_with_ub = 
    let (d,translated_fis_with_ub) = 
        With_upper_bound.decompose_wrt_translation original_fis_with_ub in 
    let (peek_res,_) = peek_for_obvious_accesses (hashtbl,severity) helper translated_fis_with_ub in 
        match peek_res with 
       Old_P_Unfinished_computation(_)  -> raise(Using_translation_exn(current_width))
      |Old_P_Failure -> (None,Some translated_fis_with_ub)
      |Old_P_Success(translated_answer) ->
         let answer_to_original = Mold.translate d translated_answer in 
         (Some(answer_to_original),None);; 

  
  let peek_for_cumulative_case (hashtbl,severity) helper old_fis_with_ub = 
      let (n,new_fis_ub) = With_upper_bound.tail_and_head old_fis_with_ub in 
      let (peek_res,_) = peek_for_obvious_accesses (hashtbl,severity) helper new_fis_ub in 
        match peek_res with 
       Old_P_Unfinished_computation(_)  -> raise(Peek_for_cumulative_case_should_never_happen_1_exn(current_width))
      |Old_P_Failure -> Old_P_Unfinished_computation([new_fis_ub]) 
      |Old_P_Success(M(sols2,ext2)) ->
        let (_,old_ub) = old_fis_with_ub in 
        if not(Old_dnif_constraint.is_admissible old_ub (ext2@[n]))
        then Old_P_Success(M(sols2,[]))
        else
        let sols3 = List.filter_map (fun sol->
                    if Old_dnif_constraint.is_admissible old_ub (sol@[n]) 
                    then Some(sol@[n]) 
                    else None    
        ) sols2 in 
        if sols3 <> [] 
        then Old_P_Success(M(sols3,ext2@[n]))  
        else Old_P_Failure
    ;;

let partition_leaves_in_fork_case (hashtbl,severity) helper leaves =
  let leaves2 = Image.image (
      fun cand ->
        (cand,seek_obvious_accesses_using_translation (hashtbl,severity) helper cand)
  ) leaves in 
  let (good_leaves,bad_leaves) = 
      List.partition (fun (_,(_,opt_bad)) -> opt_bad = None ) leaves2 in 
  (Image.image (fun ( cand,( opt_good,_opt_bad)) -> (cand,Option.get opt_good)) good_leaves,
   Image.image (fun (_cand,(_opt_good, opt_bad)) -> Option.get opt_bad        ) bad_leaves) ;; 

let peek_for_fork_case (hashtbl,severity) helper old_fis_with_ub = 
  let (fis,upper_bound) = old_fis_with_ub in 
  let opt1 = Finite_int_set.relative_head_constraint fis upper_bound in 
  if opt1=None  
  then raise(Peek_for_fork_case_should_never_happen_1_exn(current_width))
  else    
  let (C cstr_l,_) = Option.get opt1 in    
  let candidates = Image.image (
         fun i-> With_upper_bound.remove_one_element old_fis_with_ub i
  ) cstr_l in 
  let (candidates2,bad_ones) = partition_leaves_in_fork_case (hashtbl,severity) helper candidates in 
  if bad_ones <> []
  then Old_P_Unfinished_computation(bad_ones)
  else   
  let lengths = Image.image (fun (_cand,M(sols,_ext))->
          List.length(List.hd sols)) candidates2 in 
  let indexed_lengths = Int_range.index_everything lengths in 
  let (min1,min_indices) = Min.minimize_it_with_care snd indexed_lengths 
  and (max1,max_indices) = Max.maximize_it_with_care snd indexed_lengths in 
  if min1 = max1 
  then let (M(sols4,_)) = snd(List.hd(List.rev candidates2)) in 
        Old_P_Success(M(sols4,[]))
  else let (max_idx,_) = List.hd(List.rev max_indices) in 
        let (M(sols5,_)) = snd(List.nth candidates2 (max_idx-1) ) in  
        let ext5 = Image.image (fun (k,_)->List.nth cstr_l (k-1)) min_indices in 
        Old_P_Success(M(sols5,ext5));;    


  let multiple_peek (hashtbl,severity) helper old_fis_with_ub = 
    let (peek_res1,to_be_remembered) = peek_for_obvious_accesses (hashtbl,severity) helper old_fis_with_ub in 
      match peek_res1 with 
      Old_P_Success (_) -> (peek_res1,to_be_remembered)
    | Old_P_Unfinished_computation (_) -> (peek_res1,false)  
    | Old_P_Failure ->
    let peek_res2= peek_for_cumulative_case (hashtbl,severity) helper old_fis_with_ub in 
      match peek_res2 with 
      Old_P_Success (_) -> (peek_res2,true)
    | Old_P_Unfinished_computation (_) -> (peek_res2,false)  
    | Old_P_Failure -> 
    let peek_res3= peek_for_fork_case (hashtbl,severity) helper old_fis_with_ub in 
      match peek_res3 with 
      Old_P_Success (_) -> (peek_res3,true)
    | Old_P_Unfinished_computation (_) -> (peek_res3,false)  
    | Old_P_Failure -> raise(Multiple_peek_exn(current_width)) ;; 
        
  let simplified_multiple_peek (hashtbl,severity) helper fis_with_ub =   
    let (peek_res,_)= multiple_peek (hashtbl,severity) helper fis_with_ub in 
    match peek_res with 
          Old_P_Failure -> raise (Simplified_multiple_peek_exn(current_width))
        | Old_P_Unfinished_computation (new_to_be_treated) -> 
             (None,Some new_to_be_treated)
        | Old_P_Success (answer) -> 
            (Some answer,None) ;; 

  let pusher_for_needed_subcomputations (hashtbl,severity) (helper,to_be_treated) =
      match to_be_treated with 
       [] -> raise (Pusher_for_needed_subcomputations_exn_1(current_width)) 
      |fis_with_ub :: others ->
        let (peek_res,to_be_remembered)= multiple_peek (hashtbl,severity) helper fis_with_ub in
        (
          match peek_res with 
          Old_P_Failure -> raise (Pusher_for_needed_subcomputations_exn_2(current_width))
        | Old_P_Unfinished_computation (new_to_be_treated) -> 
             (helper,new_to_be_treated@to_be_treated)
        | Old_P_Success (answer) -> 
            let new_helper =(
               if to_be_remembered 
               then (fis_with_ub,answer) :: helper 
               else helper 
            ) in 
            (new_helper,others)
        )  ;;     

   let rec iterator_for_needed_subcomputations (hashtbl,severity) walker = 
      if snd walker = [] then List.rev(fst walker) else 
      let new_walker = pusher_for_needed_subcomputations (hashtbl,severity) walker in      
      iterator_for_needed_subcomputations (hashtbl,severity) new_walker ;;

   let needed_subcomputations (hashtbl,severity) items = 
    iterator_for_needed_subcomputations (hashtbl,severity) ([],items) ;;  
    
   let compute_fast_opt (hashtbl,severity) fis_with_ub =
    let (peek_res,_) =multiple_peek (hashtbl,severity) [] fis_with_ub in
      match peek_res with 
      Old_P_Success (answer) -> Some answer
    | Old_P_Unfinished_computation (_)  
    | Old_P_Failure -> None ;;

   let compute_reasonably_fast_opt (hashtbl,severity) fis_with_ub = 
    compute_fast_opt (hashtbl,severity) fis_with_ub ;;     

   

end ;;  
(* End of Level2 *)


(* Beginning of Old_Level3 *)
module Old_Level3 = struct 

  let current_width = 3 ;; 
  
  let get_below (hshtbl,severity) fis_with_ub = 
    match Old_Level2.compute_reasonably_fast_opt (hshtbl,severity) fis_with_ub with 
    Some answer -> (Old_P_Success(answer),true)  
    |None -> 
      match severity with  
       Stern -> raise(Get_below_exn(current_width-1,fis_with_ub))
      |Relaxed -> (Old_P_Unfinished_computation[fis_with_ub],false);;

  
  let peek_for_obvious_accesses (hashtbl,severity) helper fis_with_ub = 
    match List.assoc_opt fis_with_ub helper with 
      Some answer1 -> (Old_P_Success(answer1),false) 
    | None ->
       (
          match  Hashtbl.find_opt hashtbl (W current_width,fis_with_ub) with 
          Some answer2 -> (Old_P_Success(answer2),false)
        | None -> 
          let (fis,upper_bound) = fis_with_ub in 
         (match Finite_int_set.relative_head_constraint fis upper_bound with 
          None -> let domain = Finite_int_set.to_usual_int_list fis in 
                   (Old_P_Success(M([domain],domain)),false)
         |Some (cstr,_) ->   
            let (W w) = Constraint.width cstr in
            if w<current_width 
            then get_below (hashtbl,severity) (fis,upper_bound)
            else (Old_P_Failure,false)          
         )
       ) ;; 
  
  (*
   
  We use translations as little as possible. Most of the functions
  of this module are supposed to work on arguments where translation
  does not apply. The function below is an exception.
  
  *)

  

  let seek_obvious_accesses_using_translation 
    (hashtbl,severity) helper original_fis_with_ub = 
    let (d,translated_fis_with_ub) = 
        With_upper_bound.decompose_wrt_translation original_fis_with_ub in 
    let (peek_res,_) = peek_for_obvious_accesses (hashtbl,severity) helper translated_fis_with_ub in 
        match peek_res with 
        Old_P_Unfinished_computation(_)  -> raise(Using_translation_exn(current_width))
      |Old_P_Failure -> (None,Some translated_fis_with_ub)
      |Old_P_Success(translated_answer) ->
         let answer_to_original = Mold.translate d translated_answer in 
         (Some(answer_to_original),None);; 

  
  let peek_for_cumulative_case (hashtbl,severity) helper old_fis_with_ub = 
      let (n,new_fis_ub) = With_upper_bound.tail_and_head old_fis_with_ub in 
      let (peek_res,_) = peek_for_obvious_accesses (hashtbl,severity) helper new_fis_ub in 
        match peek_res with 
        Old_P_Unfinished_computation(_)  -> raise(Peek_for_cumulative_case_should_never_happen_1_exn(current_width))
      |Old_P_Failure -> Old_P_Unfinished_computation([new_fis_ub]) 
      |Old_P_Success(M(sols2,ext2)) ->
        let (_,old_ub) = old_fis_with_ub in 
        if not(Old_dnif_constraint.is_admissible old_ub (ext2@[n]))
        then Old_P_Success(M(sols2,[]))
        else
        let sols3 = List.filter_map (fun sol->
                    if Old_dnif_constraint.is_admissible old_ub (sol@[n]) 
                    then Some(sol@[n]) 
                    else None    
        ) sols2 in 
        if sols3 <> [] 
        then Old_P_Success(M(sols3,ext2@[n]))  
        else Old_P_Failure
    ;;

let partition_leaves_in_fork_case (hashtbl,severity) helper leaves =
  let leaves2 = Image.image (
      fun cand ->
        (cand,seek_obvious_accesses_using_translation (hashtbl,severity) helper cand)
  ) leaves in 
  let (good_leaves,bad_leaves) = 
      List.partition (fun (_,(_,opt_bad)) -> opt_bad = None ) leaves2 in 
  (Image.image (fun ( cand,( opt_good,_opt_bad)) -> (cand,Option.get opt_good)) good_leaves,
   Image.image (fun (_cand,(_opt_good, opt_bad)) -> Option.get opt_bad        ) bad_leaves) ;; 

let peek_for_fork_case (hashtbl,severity) helper old_fis_with_ub = 
  let (fis,upper_bound) = old_fis_with_ub in 
  let opt1 = Finite_int_set.relative_head_constraint fis upper_bound in 
  if opt1=None  
  then raise(Peek_for_fork_case_should_never_happen_1_exn(current_width))
  else    
  let (C cstr_l,_) = Option.get opt1 in    
  let candidates = Image.image (
         fun i-> With_upper_bound.remove_one_element old_fis_with_ub i
  ) cstr_l in 
  let (candidates2,bad_ones) = partition_leaves_in_fork_case (hashtbl,severity) helper candidates in 
  if bad_ones <> []
  then Old_P_Unfinished_computation(bad_ones)
  else   
  let lengths = Image.image (fun (_cand,M(sols,_ext))->
          List.length(List.hd sols)) candidates2 in 
  let indexed_lengths = Int_range.index_everything lengths in 
  let (min1,min_indices) = Min.minimize_it_with_care snd indexed_lengths 
  and (max1,max_indices) = Max.maximize_it_with_care snd indexed_lengths in 
  if min1 = max1 
  then let (M(sols4,_)) = snd(List.hd(List.rev candidates2)) in 
       Old_P_Success(M(sols4,[]))
  else let (max_idx,_) = List.hd(List.rev max_indices) in 
        let (M(sols5,_)) = snd(List.nth candidates2 (max_idx-1) ) in  
        let ext5 = Image.image (fun (k,_)->List.nth cstr_l (k-1)) min_indices in 
        Old_P_Success(M(sols5,ext5));;    


  let multiple_peek (hashtbl,severity) helper old_fis_with_ub = 
    let (peek_res1,to_be_remembered) = peek_for_obvious_accesses (hashtbl,severity) helper old_fis_with_ub in 
      match peek_res1 with 
      Old_P_Success (_) -> (peek_res1,to_be_remembered)
    | Old_P_Unfinished_computation (_) -> (peek_res1,false)  
    | Old_P_Failure ->
    let peek_res2= peek_for_cumulative_case (hashtbl,severity) helper old_fis_with_ub in 
      match peek_res2 with 
      Old_P_Success (_) -> (peek_res2,true)
    | Old_P_Unfinished_computation (_) -> (peek_res2,false)  
    | Old_P_Failure -> 
    let peek_res3= peek_for_fork_case (hashtbl,severity) helper old_fis_with_ub in 
      match peek_res3 with 
      Old_P_Success (_) -> (peek_res3,true)
    | Old_P_Unfinished_computation (_) -> (peek_res3,false)  
    | Old_P_Failure -> raise(Multiple_peek_exn(current_width)) ;; 
        
  let simplified_multiple_peek (hashtbl,severity) helper fis_with_ub =   
    let (peek_res,_)= multiple_peek (hashtbl,severity) helper fis_with_ub in 
    match peek_res with 
          Old_P_Failure -> raise (Simplified_multiple_peek_exn(current_width))
        | Old_P_Unfinished_computation (new_to_be_treated) -> 
             (None,Some new_to_be_treated)
        | Old_P_Success (answer) -> 
            (Some answer,None) ;; 

  let pusher_for_needed_subcomputations (hashtbl,severity) (helper,to_be_treated) =
      match to_be_treated with 
       [] -> raise (Pusher_for_needed_subcomputations_exn_1(current_width)) 
      |fis_with_ub :: others ->
        let (peek_res,to_be_remembered)= multiple_peek (hashtbl,severity) helper fis_with_ub in
        (
          match peek_res with 
          Old_P_Failure -> raise (Pusher_for_needed_subcomputations_exn_2(current_width))
        | Old_P_Unfinished_computation (new_to_be_treated) -> 
             (helper,new_to_be_treated@to_be_treated)
        | Old_P_Success (answer) -> 
            let new_helper =(
               if to_be_remembered 
               then (fis_with_ub,answer) :: helper 
               else helper 
            ) in 
            (new_helper,others)
        )  ;;     

   let rec iterator_for_needed_subcomputations (hashtbl,severity) walker = 
      if snd walker = [] then List.rev(fst walker) else 
      let new_walker = pusher_for_needed_subcomputations (hashtbl,severity) walker in      
      iterator_for_needed_subcomputations (hashtbl,severity) new_walker ;;

   let needed_subcomputations (hashtbl,severity) items = 
    iterator_for_needed_subcomputations (hashtbl,severity) ([],items) ;;  
    
   let compute_fast_opt (hashtbl,severity) fis_with_ub =
    let (peek_res,_) =multiple_peek (hashtbl,severity) [] fis_with_ub in
      match peek_res with 
      Old_P_Success (answer) -> Some answer
    | Old_P_Unfinished_computation (_)  
    | Old_P_Failure -> None ;;

   let compute_reasonably_fast_opt (hashtbl,severity) fis_with_ub = 
    compute_fast_opt (hashtbl,severity) fis_with_ub ;;     

   

end ;;  
(* End of Level3 *)


exception Bad_index_in_selection of int ;; 

module Selector = struct 
 

let stern_hashtbl = ((Hashtbl.create 50) : (width * old_key, mold) Hashtbl.t) ;; 
let relaxed_hashtbl = ((Hashtbl.create 50) : (width * old_key, mold) Hashtbl.t) ;; 

let get_hashtbl = function 
   Stern -> stern_hashtbl 
   |Relaxed -> relaxed_hashtbl ;; 

let needed_subcomputations (W max_width) key_list=
   match max_width with 
    1 -> []
   |2 -> Old_Level2.needed_subcomputations (relaxed_hashtbl,Relaxed) key_list
   |3 -> Old_Level3.needed_subcomputations (relaxed_hashtbl,Relaxed) key_list 
   |_ -> raise(Bad_index_in_selection max_int) ;; 
  
let compute_reasonably_fast_opt (W max_width) key =
  match max_width with 
  1 -> Old_Level1.compute_reasonably_fast_opt key
 |2 -> Old_Level2.compute_reasonably_fast_opt (stern_hashtbl,Stern) key
 |3 -> Old_Level3.compute_reasonably_fast_opt (stern_hashtbl,Stern) key 
 |_ -> raise(Bad_index_in_selection max_int) ;;    

exception Easy_compute_exn of width * old_key ;;

let easy_compute max_width key =
  match compute_reasonably_fast_opt max_width key with 
   Some answer -> answer 
   | None -> raise(Easy_compute_exn(max_width,key)) ;;  

let easy_add max_width key =
    let answer = easy_compute max_width key in 
    (
      Hashtbl.replace stern_hashtbl (max_width,key) answer ;
      Hashtbl.replace relaxed_hashtbl (max_width,key) answer 
    ) ;;

exception Import_exn of width * old_key ;;    

let import (W max_width) key =
      let (M(sols,ext)) = easy_compute (W(max_width-1)) key in 
      let sols2 = List.filter (Old_dnif_constraint.is_admissible (snd key)) sols in 
      if sols2 = []
      then raise(Import_exn(W max_width,key))
      else 
      let answer = M(sols2,ext) in   
      let _=  (
          Hashtbl.replace stern_hashtbl (W max_width,key) answer ;
          Hashtbl.replace relaxed_hashtbl (W max_width,key) answer 
        ) in 
      answer;;

end ;;   



module Main = struct 

let expand_definition_of_breadth_range width ((n,scr),breadth_range)=
  match breadth_range with  
  Br_Unrestricted -> With_upper_bound.usual_pair (n,scr,width)
  | Br_Up_to(max_breadth) ->  (FIS(n,scr),Old_UBC(max_breadth,width)) ;; 

let easy_compute  width ((n,scr),breadth_range)= 
  let fis_with_ub = expand_definition_of_breadth_range width ((n,scr),breadth_range) in 
    Selector.easy_compute  width fis_with_ub ;;

let easy_add  width ((n,scr),breadth_range)= 
    let fis_with_ub = expand_definition_of_breadth_range width ((n,scr),breadth_range) in 
      Selector.easy_add  width fis_with_ub ;;

let import  width ((n,scr),breadth_range)= 
    let fis_with_ub = expand_definition_of_breadth_range width ((n,scr),breadth_range) in 
    Selector.import  width fis_with_ub ;;      

end ;;  



module Fill = struct 

let bound = 40 ;; 


let fill () =
  let _act1 = Int_range.scale (fun k->
    Main.import (W 2) ((k,[]),Br_Unrestricted) ) 1 bound in 
  ()
  ;;  


end ;;
