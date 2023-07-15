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

  let max (FIS(n,_)) = n ;; 

  let of_usual_int_list = Private.of_usual_int_list ;; 

  let remove_one_element (FIS(n,scrappers)) k=
       let new_scrappers = i_insert k scrappers in 
       if k <> n then FIS(n,new_scrappers) else 
       let new_z =  Private.to_usual_int_list (FIS(n-1,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;     
  
  
  (*
  
  remove_one_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_one_element (FIS(3,[])) 3 ;;
  
  *)

  let to_usual_int_list = Private.to_usual_int_list ;; 


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
  
  
  



