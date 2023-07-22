(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type finite_int_set = Sz3_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type mold = Sz3_types.mold = M of (solution list) * extension_data ;;

type point = Sz3_types.point = P of finite_int_set * width ;; 

type medium_handle = Sz3_types.medium_handle = 
      Mh_import  
    | Mh_cumulative of int 
    | Mh_fork of int * int *int  ;; 

type polished_result = Sz3_types.polished_result = 
  PR of ((solution * (string list)) list) * extension_data ;;    

type polished_pair = Sz3_types.polished_pair = 
       PP of point * ( (medium_handle option) * polished_result ) ;;    


let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersect = Ordered.intersect i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_length_preserving_sort = Ordered.length_preserving_sort i_order ;;
let i_outsert = Ordered.outsert i_order ;;
let i_setminus = Ordered.setminus i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_insert = Ordered.insert il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_min= Ordered.min il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let t_order = Total_ordering.triple_product 
   i_order i_order (Total_ordering.silex_for_intlists) ;;


module Constraint = struct 

let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  



module Mold = struct 

let translate d (M(sols, ext)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    M(Image.image tr sols,tr ext) ;; 

end ;;


module Find_highest_constraint = struct

  let rec for_exact_width (W w,domain,to_be_treated) =
    match to_be_treated with 
    [] -> None 
    |p::others ->
       if p<=2*w then None else 
       if i_is_included_in [p-2*w;p-w] domain 
       then Some (C[p-2*w;p-w;p])
       else for_exact_width (W w,domain,others) ;;     
  
  let rec for_maximal_width (W w,domain) =
   match for_exact_width (W w,domain,List.rev domain) with 
   Some (breadth_max) -> Some(breadth_max)
   |None ->
      if w<2 then None else 
      for_maximal_width (W (w-1),domain) ;;  
  
  end ;;

module Finite_int_set = struct 

  module Private = struct

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  end ;;

  let decompose_wrt_translation fis_domain = 
    let domain = Private.to_usual_int_list fis_domain in 
    let (d,core_domain) = (match domain with 
      [] -> (0,[])
      | h :: _ -> (h-1, if h=1 then domain else 
                    Image.image (fun x->x-(h-1)) domain 
                   )
    ) in 
    (d,Private.of_usual_int_list core_domain) ;; 

  let empty_set = FIS(0,[]) ;;

  let max (FIS(n,_)) = n ;; 

  let of_usual_int_list = Private.of_usual_int_list ;; 

  exception Remove_one_element_exn of finite_int_set * int ;; 

  let remove_one_element (FIS(n,scrappers)) k=
       let new_scrappers = i_insert k scrappers in 
       if k <> n then FIS(n,new_scrappers) else 
       if scrappers = Int_range.range 1 (n-1)
       then empty_set
       else   
       let new_z =  Private.to_usual_int_list (FIS(n-1,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;         

  (*
  
  remove_one_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_one_element (FIS(3,[])) 3 ;;
  remove_one_element (FIS(1,[])) 1 ;;

  *)

  let to_usual_int_list = Private.to_usual_int_list ;; 

  let translate d fis = 
    let domain = Private.to_usual_int_list fis in
    let translated_domain =  Image.image (fun t->t+d) domain in 
    Private.of_usual_int_list translated_domain;; 

end ;;    

module Point = struct 

  let decompose_wrt_translation (P(fis,w)) = 
     let (d,translated_fis) = Finite_int_set.decompose_wrt_translation fis in 
     (d,P(translated_fis,w));;

  let decrement (P(fis,W w)) = P(fis,W(w-1)) ;;    

  let highest_constraint_opt (P(fis,W w)) = 
    if w<1 then None else
    Find_highest_constraint.for_maximal_width 
      (W w,Finite_int_set.to_usual_int_list fis);;

  let max (P(fis,_w)) = Finite_int_set.max fis  ;; 

  let remove_one_element (P(fis,w)) pivot = 
    let new_fis = Finite_int_set.remove_one_element fis pivot in 
    let new_w = (
      match Find_highest_constraint.for_maximal_width (w,Finite_int_set.to_usual_int_list new_fis) with
      None -> 1
      |Some(C(l))->(List.nth l 1)-(List.nth l 0)
    ) in 
    P(new_fis,W new_w) ;;

  let supporting_set (P(fis,_)) = Finite_int_set.to_usual_int_list fis ;; 

  let subset_is_admissible (P(_,w)) subset =
      ((Find_highest_constraint.for_maximal_width (w,subset)) =None);;

  let translate d (P(fis,w)) = 
      P(Finite_int_set.translate d fis,w);;

  let width (P(_,w)) = w ;; 

end ;;   

module Extra_tools = struct 

  module Width_one = struct 
  
    exception Bad_remainder_by_three of int ;; 
    
    let compute_on_set fis =
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
          |r -> raise(Bad_remainder_by_three(r)) 
      ) intervals in 
      M([List.flatten sol_components],List.flatten forced_elements);;
  
    let compute (P(fis1,_max_width)) = compute_on_set fis1;; 
  
     let compute_opt key = Some(compute key) ;; 
  
  end ;;   
  
  let compute_opt key =  
      match List.assoc_opt (Point.width key) [
          W 1, Width_one.compute_opt
      ] with 
      None -> None 
      | Some f -> f key ;;  
  
  end ;;  

  module Hashtbl_here = struct 

    let greedy = ((Hashtbl.create 50) : (point, mold) Hashtbl.t) ;; 
  
    let add key answer =
        Hashtbl.replace greedy key answer;;    
  
  end ;;
  
module Crude = struct 


    module Peek_and_seek = struct 
  
      type peek_result = 
      P_Success of mold  
     |P_Failure
     |P_Unfinished_computation of point list ;;
  
      let seek_non_translated_obvious_access helper point = 
        match List.assoc_opt point helper with 
          Some (mold1) -> Some(mold1)
        | None ->
           (
              match  Hashtbl.find_opt Hashtbl_here.greedy point with 
              Some (mold2) -> Some(mold2)
            | None -> 
              let (P(fis,_upper_bound)) = point in 
              let domain = Finite_int_set.to_usual_int_list fis in 
              if Point.subset_is_admissible point domain 
              then  Some(M([domain],domain))
               else 
                (
                  match Extra_tools.compute_opt point with 
                  Some answer2 -> Some(answer2)
                  |None -> None         
                ) 
             ) ;; 
    
       let seek_translated_obvious_access helper point =
          let (d,translated_point) = Point.decompose_wrt_translation point in 
          match seek_non_translated_obvious_access helper translated_point with 
          None -> None 
          |Some (translated_mold) -> 
             Some(Mold.translate (-d) translated_mold);; 
    
        let peek_for_import_case helper point = 
          let smaller_point = Point.decrement point  in 
          match seek_non_translated_obvious_access helper smaller_point with 
              None -> P_Unfinished_computation([smaller_point])  
             |Some(M(sols2,ext2)) ->
            let sols3 = List.filter_map (fun sol->
                        if Point.subset_is_admissible point sol 
                        then Some(sol) 
                        else None    
            ) sols2 in 
            if sols3 <> [] 
            then P_Success(M(sols3,ext2))  
            else P_Failure
        ;;
    
        let peek_for_cumulative_case helper point pivot= 
          let smaller_point = Point.remove_one_element point pivot in 
          match seek_non_translated_obvious_access helper smaller_point with 
              None -> P_Unfinished_computation([smaller_point])  
             |Some(M(sols2,ext2)) ->
            let ext3 = i_insert pivot ext2 in 
            if not(Point.subset_is_admissible point ext3) 
            then P_Failure
            else
            let sols3 = List.filter_map (fun sol->
              let increased_sol = i_insert pivot sol in 
              if Point.subset_is_admissible point increased_sol 
              then Some(increased_sol) 
              else None    
            ) sols2 in  
            if sols3 <> [] 
            then P_Success(M(sols3,ext3))  
            else P_Failure
        ;;
    
        let peek_for_easy_case helper point =
           match seek_non_translated_obvious_access helper point with 
           Some(answer1) -> (P_Success(answer1),false)
           |None ->
            let peek_res1=peek_for_import_case helper point in 
            (match peek_res1 with 
          
               P_Success(_) -> (peek_res1,true)
              |P_Unfinished_computation(_) -> (peek_res1,false)
              |P_Failure -> 
                 let n = Point.max point in 
                  (peek_for_cumulative_case helper point n,true)
                 
             ) ;;
       
           let partition_candidates_in_fork_case helper candidates =
            let candidates2 = Image.image (
                fun triple -> 
                  let (_smaller_point,_d,translated_smaller_point) = triple in 
                  (triple,seek_non_translated_obvious_access helper translated_smaller_point)
            ) candidates in 
            let (bad_leaves,good_leaves) = 
                List.partition (fun (_,opt) -> opt = None ) candidates2 in 
            (Image.image (fun ((smaller_point,d,_translated_smaller_point),opt) -> 
                  let translated_mold = Option.get opt in 
                  (smaller_point,Mold.translate (-d) translated_mold)
              ) good_leaves,
             Image.image (fun ((_smaller_point,_d,translated_smaller_point),_)->
              translated_smaller_point
              ) bad_leaves) ;; 
          
          let peek_for_fork_case helper point (i,j,k)= 
            let cstr = [i;j;k] in 
            let candidates = Image.image (fun t->
              let smaller_point=Point.remove_one_element point t in 
              let (d,translated_smaller_point) = Point.decompose_wrt_translation smaller_point in 
              (smaller_point,d,translated_smaller_point)
            ) cstr in 
            let (candidates2,bad_ones) = partition_candidates_in_fork_case helper candidates in 
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
                  let ext5 = Image.image (fun (k,_)->List.nth cstr (k-1)) min_indices in 
                  P_Success(M(sols5,ext5));;    
    
    end ;;   
      
    module Compute = struct 
  
      exception Pusher_for_needed_subcomputations_exn_1 ;; 
      exception Pusher_for_needed_subcomputations_exn_2 ;; 
      exception Pusher_for_needed_subcomputations_exn_3 of point ;;  
  
      let pusher_for_needed_subcomputations (helper,to_be_treated) =
          match to_be_treated with 
           [] -> raise (Pusher_for_needed_subcomputations_exn_1) 
          |point :: others ->
            let (peek_res1,is_new) = Peek_and_seek.peek_for_easy_case helper point in 
            (
              match peek_res1 with 
            | Peek_and_seek.P_Unfinished_computation (new_to_be_treated) -> 
                 (helper,new_to_be_treated@to_be_treated)
            | Peek_and_seek.P_Success (answer) -> 
                let new_helper =(
                   if is_new 
                   then (point,answer) :: helper 
                   else helper 
                ) in 
                (new_helper,others)
            | Peek_and_seek.P_Failure -> 
              (
                match Point.highest_constraint_opt point with 
                 None -> raise (Pusher_for_needed_subcomputations_exn_2)
                |Some(C(cstr)) ->
                   let nth = (fun k->List.nth cstr (k-1)) in 
                    match Peek_and_seek.peek_for_fork_case helper point (nth 1,nth 2,nth 3) with 
                    | Peek_and_seek.P_Unfinished_computation (new_to_be_treated) -> 
                         (helper,new_to_be_treated@to_be_treated)
                    | Peek_and_seek.P_Success (answer) -> 
                        ((point,answer) :: helper ,others)
                    | Peek_and_seek.P_Failure ->  raise (Pusher_for_needed_subcomputations_exn_3(point))
              )
              );;      
  
          
    
       let rec iterator_for_needed_subcomputations walker = 
          if snd walker = [] then List.rev(fst walker) else 
          let new_walker = pusher_for_needed_subcomputations walker in      
          iterator_for_needed_subcomputations new_walker ;;
    
       let needed_subcomputations items = 
        iterator_for_needed_subcomputations ([],items) ;;  
        
       let compute_recursively_and_remember point = 
          match Peek_and_seek.seek_translated_obvious_access [] point with 
          Some(mold) -> mold
          |None ->
             let subcomps =  needed_subcomputations [point] in 
             let _ = List.iter (fun (point,answer)->
              Hashtbl_here.add point answer
            ) subcomps in 
             List.assoc point subcomps  ;;                           
        
    end ;;  
  
    let compute point = Compute.compute_recursively_and_remember point ;; 
  
  end ;;   
  
module  Highest_separator = struct 

  module Private = struct

    type t = EP of point * (constraint_t list);;
    
    let usual_decomposition_for_bare_point_opt pt =
       match Point.highest_constraint_opt pt with 
        None -> None 
       |Some (C l)-> 
         let current_b = List.nth l 0 in 
         let effective_w=(List.nth l 1)-current_b in 
         let candidates=Int_range.descending_scale (
             fun b->C[b;b+effective_w;b+2*effective_w]
         ) 1 (current_b-1) in 
         let (P(fis,_)) = pt in 
         let domain = Finite_int_set.to_usual_int_list fis in 
         let selected_candidates = List.filter (
            fun (C l)->i_is_included_in l domain
         ) candidates in 
         Some(EP(P(fis,W(effective_w-1)),selected_candidates), C l)
         ;;
    
    let usual_decomposition_opt (EP(pt,l_cstr)) = 
       match l_cstr with 
       [] -> usual_decomposition_for_bare_point_opt pt 
      |highest :: others -> Some(EP(pt,others),highest) ;; 
    
    let remove_one_element (EP(pt,l_cstr)) t =
       let smaller_pt = Point.remove_one_element pt t in 
       EP(smaller_pt,List.filter (fun (C l)->not(i_mem t l)) l_cstr) ;; 
    
    exception Measure_exn of t ;; 
    
    let measure = Memoized.recursive (fun old_f constrained_pt ->
       let (EP(pt,l_cstr)) = constrained_pt in 
       if l_cstr = []
       then let (M(sols,_)) = Crude.compute pt in
            List.length(List.hd sols) 
       else match usual_decomposition_opt constrained_pt with 
       None -> raise(Measure_exn(constrained_pt))
       |Some(preceding_pt,C l) ->
          Max.list ( Image.image (
             fun t->old_f(remove_one_element preceding_pt t)
          ) l )
    ) ;; 
    
    let highest_separator_opt = Memoized.recursive (fun old_f constrained_pt ->
       match usual_decomposition_opt constrained_pt with 
        None -> None 
       |Some(preceding_constrained_pt,cstr)->
          if (measure constrained_pt) <> (measure preceding_constrained_pt) 
          then Some cstr
          else old_f preceding_constrained_pt
    ) ;;   
    
    end ;; 
    
    let opt pt = Private.highest_separator_opt (Private.EP(pt,[])) ;;   

end ;;  
  
  
module Medium = struct 
  module Private = struct   


 let measure point =
   let (M(sols,_)) = Crude.compute point in 
   List.length(List.hd sols) ;;  
 
  let rigorous_test_for_import_case old_point = 
    let (W w)=Point.width(old_point) in 
    if w<2 then false else
    let simpler_point = Point.decrement old_point in 
    (measure simpler_point)=(measure old_point) ;; 


let rigorous_quest_for_individual_cumulative_case old_point pivot = 
  let simpler_point = Point.remove_one_element old_point pivot in 
  if (measure simpler_point)=(measure old_point)-1 
  then Some(pivot)  
  else None ;;

let rigorous_quest_for_cumulative_case old_point =
    let domain = Point.supporting_set old_point in 
    List.find_map (rigorous_quest_for_individual_cumulative_case old_point) domain ;; 


 let handle_opt point =
  match Highest_separator.opt point with 
  None -> None
  |Some(C cstr)-> 
  if rigorous_test_for_import_case point 
  then Some(Mh_import) 
  else
  (match rigorous_quest_for_cumulative_case point with 
    Some(pivot)->Some(Mh_cumulative(pivot))
    |None ->
        let c = (fun k->List.nth cstr (k-1))  in 
        Some(Mh_fork(c 1,c 2,c 3))
  );;

let all_solutions =Memoized.recursive(fun old_f point -> 
  let domain = Point.supporting_set point
  and is_ok = Point.subset_is_admissible point in 
  match handle_opt point with 
   None -> [domain]
  |Some(handle) -> 
    let compute_below = (fun t->
          old_f (Point.remove_one_element point t)
    ) in 
    match handle with 
     Mh_cumulative(pivot)->
          List.filter_map (
             fun sol->
               let new_sol = i_insert pivot sol in 
               if is_ok new_sol then Some new_sol else None
          )(compute_below pivot) 
    |Mh_fork(i,j,k)->
         il_fold_merge(Image.image compute_below [i;j;k])
    |Mh_import ->  
         let smaller_point = Point.decrement point in 
         List.filter is_ok (old_f smaller_point)
    );;        

  
   let helper_for_canonical_solution (sols,n) =
     let (with_n,without_n) = List.partition (List.mem n) sols in 
     if without_n=[]
     then (true,Image.image(i_outsert n) with_n) 
    else (false,without_n) ;;      
    
    let rec iterator_for_canonical_solution (to_be_treated,n,treated) =
      if n<1 then treated else  
      let (n_needed,to_be_treated2) = helper_for_canonical_solution (to_be_treated,n) in 
      let treated2 = (if n_needed then n::treated else treated) in
      iterator_for_canonical_solution (to_be_treated2,n-1,treated2) ;; 

    let canonical_solution point =
       let all_sols = all_solutions point 
       and n = Point.max point in 
       iterator_for_canonical_solution (all_sols,n,[]) ;; 
    
    

end ;;   

let all_solutions = Private.all_solutions ;; 
let canonical_solution = Private.canonical_solution ;;
let handle_opt = Private.handle_opt ;; 

end ;;
  



module SecondCrude = struct 

  type shadow =
     Bare
    |Early_stop of point * int 
    |Early_increase of point * int  
    |Lucky of point * (int list)
    |Disjunction of (point * (int list)) list;;  

  type partial_result =
    P_Finished_computation of shadow * mold  
   |P_Unfinished_computation of point list ;;

  type inner_walker = IW of 
    ( point * ((int list) list)) * 
    (
       ( (point * (int list) ) list)
       *
       ( (point * (int list) * int) list)
    ) ;;



  exception Compute_strictly_exn ;; 
  exception Pusher_for_needed_subcomputations1_exn ;; 

  let main_hashtbl =   ((Hashtbl.create 50) : (point, shadow * mold) Hashtbl.t) ;; 

let translate_shadow d = 
  let tr = Image.image (fun t->t+d) in 
  function 
 Bare -> Bare
|Early_stop(pt,n) ->  Early_stop(Point.translate d pt,n+d)
|Early_increase(pt,n) ->  Early_increase(Point.translate d pt,n+d)
|Lucky(pt,complement) -> Lucky(Point.translate d pt,tr complement)
|Disjunction(l) -> Disjunction(Image.image (
  fun (pt,complement) -> (Point.translate d pt,tr complement)
) l);;


let seek_non_translated_obvious_access helper point = 
    match List.assoc_opt point helper with 
      Some (sh1,mold1) -> P_Finished_computation(sh1,mold1)
    | None ->
       (
          match  Hashtbl.find_opt main_hashtbl point with 
          Some (sh2,mold2) -> P_Finished_computation(sh2,mold2)
        | None -> 
          let (P(fis,_upper_bound)) = point in 
          let domain = Finite_int_set.to_usual_int_list fis in 
          if Point.subset_is_admissible point domain 
          then  P_Finished_computation(Bare,M([domain],domain))
           else 
            (
              match Extra_tools.compute_opt point with 
              Some answer2 -> P_Finished_computation(Bare,answer2)
              |None -> P_Unfinished_computation [point]        
            ) 
         ) ;; 


let seek_translated_obvious_access helper point =
    let (d,translated_point) = Point.decompose_wrt_translation point in 
    let res = seek_non_translated_obvious_access helper translated_point in 
    match res with 
    P_Unfinished_computation _ -> res
    |P_Finished_computation(translated_sh,translated_mold)
    -> 
      P_Finished_computation(translated_sh,
      Mold.translate (-d) translated_mold);;

let reduce_by_removing_forbidden_elements pt (pt5,ext5) =
    let domain=List.rev(Point.supporting_set pt5) in 
    let rec tempf=(
      fun (remaining_elts,remaining_point) ->
        match remaining_elts with
        [] -> remaining_point
       |elt :: other_elts ->
         if Point.subset_is_admissible pt (i_insert elt ext5)
         then remaining_point
         else tempf(other_elts,Point.remove_one_element remaining_point elt)  
    ) in 
    tempf(domain,pt5);;

let long_case_in_inner_pusher_for_needed_subcomputations
      helper (IW((pt,sols_for_preceding_point),(failures,_hopes))) to_be_treated 
          (pt2,ext2,goal) other_hopes sols3=
          let whole = i_merge (Point.supporting_set pt2) ext2 in 
          if List.length(whole)<goal 
          then let bulky=IW( (pt,sols_for_preceding_point),
                     ((pt2,ext2)::failures,other_hopes)) in 
                 (helper,Some bulky,to_be_treated)
          else 
          if Point.subset_is_admissible pt whole 
          then let pair = (pt,(Lucky(pt2,ext2),M([whole],[]))) in  
               (pair::helper,None,to_be_treated) 
          else     
          let m3=List.length(List.hd sols3) in 
          if m3+List.length(ext2)<goal 
          then let bulky=IW( (pt,sols_for_preceding_point),
                   ((pt2,ext2)::failures,other_hopes)) in 
               (helper,Some bulky,to_be_treated)
          else 
          let sols4 = List.filter_map (fun sol->
              let increased_sol = i_merge sol ext2 in 
              if Point.subset_is_admissible pt increased_sol 
              then Some(increased_sol) 
              else None    
            ) sols3 in  
          if sols4 <> []
          then let pair = (pt,(Lucky(pt2,ext2),M(sols4,[]))) in  
               (pair::helper,None,to_be_treated) 
          else 
          let n2 = Point.max pt2 in 
          let pt5 = Point.remove_one_element pt2 n2 in 
          let ext5 = i_insert n2 ext2 in  
          if Point.subset_is_admissible pt ext5 
          then  let pt6 = reduce_by_removing_forbidden_elements pt (pt5,ext5) in 
                let bulky2=IW( (pt,sols_for_preceding_point),
                (failures,(pt5,ext2,goal)::(pt6,ext5,goal)::other_hopes)) in 
                (helper,Some bulky2,to_be_treated)
          else  let bulky3=IW( (pt,sols_for_preceding_point),
                (failures,(pt5,ext2,goal)::other_hopes)) in 
                (helper,Some bulky3,to_be_treated)                         
       ;; 


let inner_pusher_for_needed_subcomputations
      helper (IW((pt,sols_for_preceding_point),(failures,hopes))) to_be_treated =
      match hopes with 
      [] -> let pair = (pt,(Disjunction(failures),M(sols_for_preceding_point,[]))) in  
            (pair::helper,None,to_be_treated) 
     |(pt2,ext2,goal)::other_hopes ->
       (
        match seek_translated_obvious_access helper pt2 with 
        P_Unfinished_computation(l) ->
           (helper,None,l@(pt::to_be_treated))
        |P_Finished_computation(_,M(sols3,_ext3)) -> 
          long_case_in_inner_pusher_for_needed_subcomputations
          helper (IW((pt,sols_for_preceding_point),(failures,hopes))) to_be_treated 
              (pt2,ext2,goal) other_hopes sols3
       );; 


let first_analysis_on_new_item helper pt other_items= 
  match seek_translated_obvious_access helper pt with 
  P_Finished_computation(_,_) ->
     (helper,None,other_items)
  |P_Unfinished_computation _ -> 
    let n = Point.max pt in 
    let pt2 = Point.remove_one_element pt n in 
    (
      match seek_translated_obvious_access helper pt2 with 
      P_Unfinished_computation(l) ->
         (helper,None,l@(pt::other_items))
      |P_Finished_computation(_,M(sols,ext)) ->
          let ext2 = i_insert n ext in 
          if not(Point.subset_is_admissible pt ext2) 
          then let pair = (pt,(Early_stop(pt2,n),M(sols,[]))) in 
              (pair::helper,None,other_items)
          else
          let sols2 = List.filter_map (fun sol->
            let increased_sol = i_insert n sol in 
            if Point.subset_is_admissible pt increased_sol 
            then Some(increased_sol) 
            else None    
          ) sols in  
          if sols2 <> [] 
          then let pair = (pt,(Early_increase(pt2,n),M(sols2,i_insert n ext))) in 
               (pair::helper,None,other_items)
          else let goal = List.length(List.hd sols)+1 in 
               (helper,Some(IW((pt,sols),([],[(pt2,[n],goal)]))),other_items)
    )
;;

let pusher_for_needed_subcomputations
    (helper,in_progress_opt,to_be_treated)= 
    match in_progress_opt with 
    Some(inner_walker) ->
      inner_pusher_for_needed_subcomputations
      helper inner_walker to_be_treated
    |None ->
      (
        match to_be_treated with 
        []->raise(Pusher_for_needed_subcomputations1_exn)
        |item2 :: other_items ->
             first_analysis_on_new_item helper item2 other_items 
      ) ;;


let rec iterator_for_needed_subcomputations walker = 
  let (treated,in_progress_opt,to_be_treated)= walker in 
  if (in_progress_opt,to_be_treated) = (None,[]) 
  then List.rev(treated) 
   else 
  let new_walker = pusher_for_needed_subcomputations walker in      
  iterator_for_needed_subcomputations new_walker ;;
  
let needed_subcomputations items = 
      iterator_for_needed_subcomputations ([],None,items) ;;  
      
let ref_for_strict_mode = ref false ;;
let ref_for_needed_data = ref [] ;; 

let compute point = 
   match seek_translated_obvious_access [] point with 
   P_Finished_computation(sh,mold) -> (sh,mold)
  |P_Unfinished_computation _ ->
    let subcomps =  needed_subcomputations [point] in 
    if !ref_for_strict_mode
    then let _=(ref_for_needed_data :=  needed_subcomputations [point]) in 
          raise Compute_strictly_exn
    else List.assoc point subcomps ;;   


end ;;   
