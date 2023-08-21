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

type point = Sz3_types.point = P of finite_int_set * width ;; 

type point_with_extra_constraints = Sz3_types.point_with_extra_constraints = 
  PEC of point * (constraint_t list);;

type point_with_breadth = Sz3_types.point_with_breadth = PWB of point * int ;; 

type handle = Sz3_types.handle = 
   Discrete
  |Select of int * int * int   
  |Rightmost_overflow of int * int * int 
  |Rightmost_pivot of width
  |Fork of int * int * int ;;  


type helper = Sz3_types.helper = 
   Help_with_solution of point_with_breadth * solution 
  |Help_with_links of point_with_breadth * (int list) ;; 

type crude_mold = Sz3_types.crude_mold = CM of (solution list) * extension_data ;;  


type extra_info = Sz3_types.extra_info = I of int ;; 

type medium_mold = Sz3_types.medium_mold = MM of (solution list) * extension_data * extra_info ;;    

type medium_diagnosis  = Sz3_types.medium_diagnosis  = 
  Missing_treatment of point_with_breadth 
 |Incomplete_treatment of point_with_breadth 
 |Missing_links of point_with_breadth * (int list)
 |Finished of handle * medium_mold * bool ;; 

type fan = F of int list list ;; 

let i_order = Total_ordering.for_integers ;;
let i_does_not_intersect = Ordered.does_not_intersect i_order ;;
let i_fold_intersect = Ordered.fold_intersect i_order ;;
let i_fold_merge = Ordered.fold_merge i_order ;;
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


let fis_order = ((fun (FIS(n1,scr1)) (FIS(n2,scr2)) ->
    let trial1 = i_order n1 n2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    let trial2 = i_order (List.length scr2) (List.length scr1) in 
    if trial2<>Total_ordering_result_t.Equal then trial2 else
      Total_ordering.silex_for_intlists scr1 scr2
  ): finite_int_set Total_ordering_t.t);;

let point_order = ((fun (P(fis1,W w1)) (P(fis2,W w2)) ->
    let trial1 = i_order w1 w2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    fis_order fis1 fis2
  ): point Total_ordering_t.t);;
let point_insert = Ordered.insert point_order ;;

let order_for_triples = ((fun (W w1,scr1,b1) (W w2,scr2,b2) ->
  let trial1 = i_order w1 w2 in 
  if trial1<>Total_ordering_result_t.Equal then trial1 else 
  let trial2 = i_order (List.length scr2) (List.length scr1) in 
  if trial2<>Total_ordering_result_t.Equal then trial2 else
  let trial3 = Total_ordering.silex_for_intlists scr1 scr2 in 
  if trial3<>Total_ordering_result_t.Equal then trial3 else
    Total_ordering.for_integers b1 b2
): (width * int list * int) Total_ordering_t.t);;

module Constraint = struct 

let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  



module Crude_mold = struct 

let discrete domain = CM([domain],domain) ;;   

let of_solutions sols = CM(sols,[]) ;; 

let constructor sols ext= CM(sols,ext) ;;  

let translate d (CM(sols, ext)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    CM(Image.image tr sols,tr ext) ;; 

let solutions (CM(sols, _ext))= sols ;; 

let solutions_and_forced_elements (CM(sols, ext))= (sols,ext) ;;  

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

  let constructor n scrappers =
      let domain = i_setminus (Int_range.range 1 n) scrappers in 
      Private.of_usual_int_list domain ;; 

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

  let remove_element (FIS(n,scrappers)) k=
       let new_scrappers = i_insert k scrappers in 
       if k <> n then FIS(n,new_scrappers) else 
       if scrappers = Int_range.range 1 (n-1)
       then empty_set
       else   
       let new_z =  Private.to_usual_int_list (FIS(n-1,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;         

  (*
  
  remove_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_element (FIS(3,[])) 3 ;;
  remove_element (FIS(1,[])) 1 ;;

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

  let is_discrete pt = (highest_constraint_opt(pt)=None) ;; 

  let is_nontrivial (P(fis,w)) =
    let domain = Finite_int_set.to_usual_int_list fis in
    ((Find_highest_constraint.for_maximal_width (w,domain)) <> None);;

  let max (P(fis,_w)) = Finite_int_set.max fis  ;; 

  let remove_element (P(fis,w)) pivot = 
    let new_fis = Finite_int_set.remove_element fis pivot in 
    let new_w = (
      match Find_highest_constraint.for_maximal_width (w,Finite_int_set.to_usual_int_list new_fis) with
      None -> 0
      |Some(C(l))->(List.nth l 1)-(List.nth l 0)
    ) in 
    P(new_fis,W new_w) ;;

  let remove_elements pt pivots = List.fold_left remove_element pt pivots ;;   

  let supporting_set (P(fis,_)) = Finite_int_set.to_usual_int_list fis ;; 

  let subset_is_admissible (P(_,w)) subset =
      ((Find_highest_constraint.for_maximal_width (w,subset)) =None);;

  let translate d (P(fis,w)) = 
      P(Finite_int_set.translate d fis,w);;

  let width (P(_,w)) = w ;; 


end ;;   

module Width_one = struct 
  
  exception Bad_remainder_by_three of int ;; 
  
  let compute fis =
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
    Crude_mold.constructor 
      [List.flatten sol_components] (List.flatten forced_elements);;

end ;;   

module Precomputed = struct 

  module Private = struct

  

  let treated_widths = [
     W 1, Width_one.compute
  ] ;; 


  let precomputations_for_width_opt w = List.assoc_opt w treated_widths ;;
  
end ;;   
  

  let compute_opt pt =  
      let (P(fis,W w)) = pt in     
      match Private.precomputations_for_width_opt (W w) with 
        Some f -> Some(f fis)
      | None -> None ;;  

  
end ;;  
  

module Crude_analysis_on_bare_point = struct 

  type explanation =
     Early_stop of point * int 
    |Early_increase of point * int  
    |Lucky of point * (int list)
    |Disjunction of (point * (int list)) list;; 

  module Private = struct

   

  type partial_result =
    P_Finished_computation of (explanation option) * crude_mold  
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

  let main_hashtbl =   ((Hashtbl.create 50) : (point, crude_mold) Hashtbl.t) ;; 
  let explanation_hashtbl =   ((Hashtbl.create 50) : (point, explanation) Hashtbl.t) ;; 

let translate_shadow d = 
  let tr = Image.image (fun t->t+d) in 
  function 
 Early_stop(pt,n) ->  Early_stop(Point.translate d pt,n+d)
|Early_increase(pt,n) ->  Early_increase(Point.translate d pt,n+d)
|Lucky(pt,complement) -> Lucky(Point.translate d pt,tr complement)
|Disjunction(l) -> Disjunction(Image.image (
  fun (pt,complement) -> (Point.translate d pt,tr complement)
) l);;

let translate_shadow_option d = function  
  None -> None 
  |Some(shadow) -> Some(translate_shadow d shadow);;

let seek_non_translated_obvious_access helper point = 
    match List.assoc_opt point helper with 
      Some (sh1,mold1) -> P_Finished_computation(sh1,mold1)
    | None ->
       (
          match  Hashtbl.find_opt main_hashtbl point with 
          Some (mold2) -> P_Finished_computation(Hashtbl.find_opt explanation_hashtbl point,mold2)
        | None -> 
          let (P(fis,_upper_bound)) = point in 
          let domain = Finite_int_set.to_usual_int_list fis in 
          if Point.subset_is_admissible point domain 
          then  P_Finished_computation(None,Crude_mold.discrete domain)
           else 
            (
              match Precomputed.compute_opt point with 
              Some answer2 -> P_Finished_computation(None,answer2)
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
      Crude_mold.translate d translated_mold);;

let reduce_by_removing_forbidden_elements pt (pt5,ext5) =
    let domain=List.rev(Point.supporting_set pt5) in 
    let rec tempf=(
      fun (remaining_elts,remaining_point) ->
        match remaining_elts with
        [] -> remaining_point
       |elt :: other_elts ->
         if Point.subset_is_admissible pt (i_insert elt ext5)
         then remaining_point
         else tempf(other_elts,Point.remove_element remaining_point elt)  
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
          then let pair = (pt,(Some(Lucky(pt2,ext2)),Crude_mold.of_solutions [whole])) in  
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
          then let pair = (pt,(Some(Lucky(pt2,ext2)),Crude_mold.of_solutions sols4)) in  
               (pair::helper,None,to_be_treated) 
          else 
          let n2 = Point.max pt2 in 
          let pt5 = Point.remove_element pt2 n2 in 
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
      [] -> let pair = (pt,(Some(Disjunction(failures)),
             Crude_mold.of_solutions sols_for_preceding_point )) in  
            (pair::helper,None,to_be_treated) 
     |(pt2,ext2,goal)::other_hopes ->
       (
        match seek_translated_obvious_access helper pt2 with 
        P_Unfinished_computation(l) ->
           (helper,None,l@(pt::to_be_treated))
        |(P_Finished_computation(_,mold3)) -> 
          let sols3 = Crude_mold.solutions mold3 in 
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
    let pt2 = Point.remove_element pt n in 
    (
      match seek_translated_obvious_access helper pt2 with 
      P_Unfinished_computation(l) ->
         (helper,None,l@(pt::other_items))
      |P_Finished_computation(_,mold) -> 
          let (sols,ext) = Crude_mold.solutions_and_forced_elements mold in 
          let ext2 = i_insert n ext in 
          if not(Point.subset_is_admissible pt ext2) 
          then let pair = (pt,(Some(Early_stop(pt2,n)),Crude_mold.of_solutions sols)) in 
              (pair::helper,None,other_items)
          else
          let sols2 = List.filter_map (fun sol->
            let increased_sol = i_insert n sol in 
            if Point.subset_is_admissible pt increased_sol 
            then Some(increased_sol) 
            else None    
          ) sols in  
          if sols2 <> [] 
          then let pair = (pt,(Some(Early_increase(pt2,n)),
               Crude_mold.constructor sols2 (i_insert n ext))) in 
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

let compute point = 
   match seek_translated_obvious_access [] point with 
   P_Finished_computation(_sh,mold) -> mold
  |P_Unfinished_computation _ ->
    let subcomps =  needed_subcomputations [point] in 
    let _ = List.iter (
        fun (pt,(explanation_opt,mold))->
            Hashtbl.add main_hashtbl pt mold;
            (
              match explanation_opt with 
              None -> ()
              |Some(explanation) -> Hashtbl.add explanation_hashtbl pt explanation
            )
    ) subcomps in 
    snd(List.assoc point subcomps) ;;   

  let explain pt = Hashtbl.find_opt explanation_hashtbl pt ;;

  end ;; 

let compute = Private.compute ;;  
let explain = Private.explain ;; 

end ;;   

module Point_with_extra_constraints = struct 

   module Private = struct

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
      Some(PEC(P(fis,W(effective_w-1)),selected_candidates), C l)
      ;;  

    let usual_decomposition_opt (PEC(pt,l_cstr)) = 
        match l_cstr with 
      [] -> usual_decomposition_for_bare_point_opt pt 
    |highest :: others -> Some(PEC(pt,others),highest) ;;     

    let is_discrete pwc = ((usual_decomposition_opt pwc)=None);;
    
    let translate d (PEC(pt,l_cstr)) =
         PEC(Point.translate d pt,
           Image.image (fun (C l)->C(Image.image (fun t->t+d) l)) l_cstr
         ) ;;

    let subset_is_admissible (PEC(pt,l_cstr)) subset =
        if not(Point.subset_is_admissible pt subset)
        then false 
        else List.for_all (fun (C cstr)->not(i_is_included_in cstr subset)) l_cstr;;       

  end ;;    

  let decompose_wrt_translation pwc = 
    let (PEC(pt,_l_cstr)) = pwc in 
    let (d,_) = Point.decompose_wrt_translation pt in 
    (d,Private.translate (-d) pwc);; 

  let is_discrete = Private.is_discrete ;; 

  let remove_element (PEC(pt,l_cstr)) t =
    let smaller_pt = Point.remove_element pt t in 
    PEC(smaller_pt,List.filter (fun (C l)->not(i_mem t l)) l_cstr) ;; 

  let remove_rightmost_element pt_with_constraints =
     let (PEC(pt,_)) = pt_with_constraints in 
     remove_element  pt_with_constraints (Point.max pt) ;; 

  let remove_rightmost_element_but_keep_constraints (PEC(pt,l_cstr)) =
      let (W w) = Point.width pt and n=Point.max pt in 
      let smaller_pt = Point.remove_element pt n in
      let constraints1 = 
         (Int_range.descending_scale (fun d->[n-(2*d);n-d]) w 1)@
         (Image.image (fun (C l)->i_outsert n l) l_cstr) in
      let constraints2 = Ordered_misc.minimal_elts_wrt_inclusion constraints1 in 
      let (singletons,constraints3) = List.partition (fun cstr->List.length(cstr)=1) constraints2 in
      let removable_subset = List.flatten singletons in 
      let final_pt = Point.remove_elements smaller_pt removable_subset 
      and final_constraints = Image.image (fun l->C l) constraints3 in 
      PEC(final_pt,final_constraints) ;;   
  
  let translate = Private.translate ;; 

  let subset_is_admissible = Private.subset_is_admissible ;;  

  let supporting_set (PEC(pt,_)) = Point.supporting_set pt ;; 

end ;;   


module Analysis_with_extra_constraints = struct 

module Private = struct

let measure = Memoized.recursive (fun 
   old_f pwc-> 
     let (PEC(pt,l_cstr)) = pwc in 
     let stays_admissible = (fun z->List.for_all (
        fun (C cstr)->not(i_is_included_in cstr z)
     ) l_cstr) in 
     let trial1 = Crude_mold.solutions(Crude_analysis_on_bare_point.compute pt) in 
     if List.exists stays_admissible  trial1 
     then List.length(List.hd trial1)
     else 
     let pwc2 = Point_with_extra_constraints.remove_rightmost_element pwc
     and pwc3 = Point_with_extra_constraints.remove_rightmost_element_but_keep_constraints pwc in 
     max(old_f pwc2)((old_f pwc3)+1)
);;

let standard_solution  = Memoized.recursive (fun 
  old_f pwc-> 
  let (PEC(pt,_l_cstr)) = pwc in 
  if Point_with_extra_constraints.is_discrete pwc 
  then Point.supporting_set pt 
  else  
  let pwc2 = Point_with_extra_constraints.remove_rightmost_element pwc
  and pwc3 = Point_with_extra_constraints.remove_rightmost_element_but_keep_constraints pwc in 
  if (measure pwc2)>=((measure pwc3)+1)
  then old_f(pwc2)
  else (old_f(pwc3))@[Point.max pt]  
);;

let look_for_pivot  pwc = 
    let (PEC(pt,_l_cstr)) = pwc in 
    let domain = List.rev (Point.supporting_set pt) 
    and m = measure(pwc)-1 in  
    List.find_opt (
       fun p -> measure(Point_with_extra_constraints.remove_element pwc p)=m
    )  domain;;

let test_for_rightmost_pivot pwc = 
  let (PEC(pt,_l_cstr)) = pwc in 
  let n = Point.max pt in 
  measure(Point_with_extra_constraints.remove_element pwc n)=
    measure(pwc)-1 ;; 



end ;; 


let measure = Private.measure ;;
let standard_solution = Private.standard_solution ;; 
let test_for_rightmost_pivot = Private.test_for_rightmost_pivot ;; 

end ;;  

module Point_with_breadth = struct 

module Private = struct

let to_extra_constraints (PWB(pt,b)) =
    if b = 0 then PEC(pt,[]) else 
    let (W w)=Point.width pt 
    and domain = Point.supporting_set pt in 
    let all_constraints = Int_range.descending_scale 
       (fun k->C[k;k+(w+1);k+2*(w+1)]) b 1 in 
    let meaningful_constraints = List.filter(
      fun (C cstr) -> i_is_included_in cstr domain
    )  all_constraints in 
    PEC(pt,meaningful_constraints) ;;    

let small_standardization pwb =
    let (PWB(pt,b)) = pwb in
    let (P(fis,W w)) = pt in 
    let support = Point.supporting_set pt in 
    match List.find_opt (fun t->i_is_included_in [t;t+(w+1);t+2*(w+1)] support) (List.rev(Int_range.range 1 b)) with
    Some(b0)->PWB(pt,b0)
    |None ->
    match Point.highest_constraint_opt pt with  
     None -> PWB(P(fis,W 0),0)
    |Some(C cstr) -> 
      let nth = (fun k->List.nth cstr (k-1)) in 
      let w = W((nth 2)-(nth 1)-1) in 
      PWB(P(fis,w),nth 1);;
    ;; 

let usual_decomposition_opt pwb =
  let (PWB(pt,b)) = small_standardization pwb in 
  let (P(_,W w)) = pt in 
  if b=0
  then None 
  else Some(small_standardization(PWB(pt,b-1)),C[b;b+(w+1);b+2*(w+1)]);;

  (* when d<0, this opeartion makes sense 
  only if the lowest element is >|d|. *)    
  let translate d (PWB(pwc,b)) = 
    (*
      when b+d<0, there is no extra constraint, 
      and this is equivalent to setting the new b to 0S    
    *) 
    let new_b = max(0)(b+d) in
      PWB(Point.translate d pwc,new_b) ;;

  let supporting_set pwb = Point_with_extra_constraints.supporting_set (to_extra_constraints pwb) ;;     

   let complementary_pairs pwb =
     let (PWB(P(FIS(n,_scr),W max_w),b)) = pwb 
     and domain = supporting_set pwb in 
     let candidates = Int_range.range 1 (max_w+1) in 
     List.filter_map (
         fun w->
            let u = n-2*w and v=n-w in 
           if not(i_is_included_in [u;v] domain) then None else
           if w<=max_w then Some(u,v) else 
           if u<=b then Some(u,v) else None  
     ) candidates ;;

   let obstructions (PWB(P(FIS(n,_scr),W wmax),b)) = 
     let obstructions_for_width = (fun w->Int_range.scale(fun t->[t;t+w;t+2*w]) 1 (n-2*w)) in 
       List.flatten((Int_range.scale obstructions_for_width 1 wmax)@
       [Int_range.scale(fun t->[t;t+(wmax+1);t+2*(wmax+1)]) 1 b]);;

   let solutions pwb offset =
      let temp1 = il_sort(List_again.power_set (supporting_set pwb)) in 
      let obstrs = obstructions pwb in
      let temp2 = List.filter (fun y->List.for_all (fun obs->not(i_is_included_in obs y))obstrs) temp1 in 
      let m = List.length(List.hd(List.rev temp2)) in 
      List.filter (fun y->List.length(y)=m-offset) temp2 ;; 

   let rightmost_largest_width (PWB(P(FIS(n,_scr),W w),b)) =
      if b>=(n-2*(w+1))
      then W(w+1)
      else W(w) ;;   

end ;;  

let breadth (PWB(_pt,b))= b ;;
let complementary_pairs = Private.complementary_pairs ;;
let decompose_wrt_translation pwb = 
  let (PWB(pt,_b)) = pwb in 
  let (d,_) = Point.decompose_wrt_translation pt in 
  (d,Private.translate (-d) pwb);; 

let everything_but_the_size (PWB(P(FIS(_n,scr),w),b)) = (w,scr,b) ;;  
let is_discrete pwb = Point_with_extra_constraints.is_discrete (Private.to_extra_constraints pwb) ;; 
let max (PWB(pt,_b)) = Point.max pt ;;
let to_extra_constraints = Private.to_extra_constraints ;; 
let remove_element (PWB(pt,b)) elt = Private.small_standardization(PWB(Point.remove_element pt elt,b));;
let rightmost_largest_width = Private.rightmost_largest_width ;; 
let size (PWB(P(FIS(n,_scr),_w),_b)) = n ;;
let solutions = Private.solutions ;;  
let subset_is_admissible pwb subset = 
     Point_with_extra_constraints.subset_is_admissible (Private.to_extra_constraints pwb) subset;; 

let supporting_set = Private.supporting_set ;; 
let translate = Private.translate ;; 
let usual_decomposition_opt = Private.usual_decomposition_opt ;; 
let width (PWB(pt,_b))= Point.width pt ;;

end ;;  


module Fan = struct 

  let constructor ll =
    let sorted_ll = il_sort ll in 
    F (Ordered_misc.minimal_elts_wrt_inclusion(sorted_ll));;

  let select selector (F ll) =
      F(List.filter (fun z->not(i_is_included_in selector z)) ll);;  

  let rightmost_pivot (complements,n) (F ll)=
   let test =(fun z -> List.for_all (fun (u,v)->not(i_is_included_in [u;v] z)) complements) in 
   F(List.filter_map (fun z->if test z then Some(i_insert n z) else None) ll);;

  let union (F ll1) (F ll2) = constructor(ll1@ll2) ;;  

end ;;   

module Analysis_with_breadth = struct 

module Private = struct

let standard_solution = Memoized.make(fun pwb->
    Analysis_with_extra_constraints.standard_solution (Point_with_breadth.to_extra_constraints pwb)
)  ;;

let measure = Memoized.make(fun pwb->
  Analysis_with_extra_constraints.measure (Point_with_breadth.to_extra_constraints pwb)
)  ;;

let test_for_individual_rightmost_overflow left_pwb m (u,v) = 
    List.for_all (fun t->measure(Point_with_breadth.remove_element left_pwb t)=m-1) [u;v] ;;

let test_for_rightmost_overflow pwb m =
    let pairs =  Point_with_breadth.complementary_pairs pwb 
    and left_pwb = Point_with_breadth.remove_element pwb (Point_with_breadth.max pwb) in 
    List.find_opt (test_for_individual_rightmost_overflow left_pwb m) pairs ;; 

let decompose = Memoized.make(fun pwb->
      match Point_with_breadth.usual_decomposition_opt pwb with 
          None -> (Discrete,PWB(P(FIS(0,[]),W 0),0))
          |Some(preceding_pwb,C cstr) ->
           let nth = (fun k->List.nth cstr (k-1)) in 
           let m = measure pwb in 
           if measure preceding_pwb = m
           then (Select(nth 1,nth 2,nth 3),preceding_pwb)
           else 
             let n = Point_with_breadth.max pwb in
             let left_pwb = Point_with_breadth.remove_element pwb n in 
             let pwc = Point_with_breadth.to_extra_constraints pwb in 
             if Analysis_with_extra_constraints.test_for_rightmost_pivot pwc 
             then (Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),left_pwb)
             else 
             (  
             match test_for_rightmost_overflow pwb m with 
             (Some(u,v))->(Rightmost_overflow(u,v,n),left_pwb)
             |None ->   
             (Fork(nth 1,nth 2,nth 3),preceding_pwb)    
))  ;;     
   

end ;; 

let decompose = Private.decompose ;;
let handle pwb = fst(Private.decompose pwb) ;;
let measure = Private.measure ;;
let standard_solution = Private.standard_solution ;;

end ;;   
  

module Extra_info = struct 

let table_for_rightmost_overflow = ref [] ;; 
let table_for_rightmost_pivot = ref [] ;;    
let table_for_select = ref [] ;;    
let table_for_last_minute_deductions = ref [] ;; 

let unregistered = I 0 ;;

let discrete _domain = unregistered  ;;

let rightmost_overflow (u,v,_n) (I old_info) = 
  match List.assoc_opt (old_info,(u,v)) (!table_for_rightmost_overflow) with 
  Some(new_info) -> I new_info
 |None -> unregistered ;;   
    

let rightmost_pivot (W w) (I old_info) =
  match List.assoc_opt (old_info,W w) (!table_for_rightmost_pivot) with 
  Some(new_info) -> I new_info
  |None -> unregistered ;;    

let select (x,y,z) n (I old_info) = 
  match List.assoc_opt ((x-n,y-n,z-n),old_info) (!table_for_select) with 
  Some(new_info) -> I new_info
  |None -> unregistered ;;
  
let last_minute_deduction (I info) n =
    match List.assoc_opt info (!table_for_last_minute_deductions) with 
    None -> None
   |Some f-> f n ;;  

end ;;   

     
module Medium_mold = struct 

    let add_links (MM(sols,ext,extra_info)) links = MM(sols,i_merge links ext,extra_info) ;;

    let add_solution (MM(sols,ext,extra_info)) new_sol = MM(il_insert new_sol sols,ext,extra_info) ;;

    let discrete domain = MM([domain],domain,Extra_info.discrete domain) ;;   
    
    let fork (MM(_sols1,ext1,_),MM(_sols2,ext2,_),MM(sols3,ext3,_)) =
      let final_ext = i_fold_intersect [ext1;ext2;ext3] in 
      MM(sols3,final_ext,Extra_info.unregistered);;   ;;

    let forced_elements (MM(_sols, ext,_))= ext ;; 

    let last_minute_deduction pwb mold = 
      let n = Point_with_breadth.max pwb in 
      let (MM(sols,ext,info)) = mold in 
      match Extra_info.last_minute_deduction info n with 
      None -> mold 
      |Some extra_forced_elements->
           MM(sols,i_merge extra_forced_elements ext,info) ;;

    
    let measure (MM(sols, _ext,_)) = List.length(List.hd sols) ;; 

    let rightmost_overflow (u,v,n) (MM(sols,_left_ext,info)) 
           = MM(sols,[],Extra_info.rightmost_overflow (u,v,n) info) ;;
    
    let rightmost_pivot w (MM(_sols,ext,info)) n (new_sols:solution list) = 
          MM(new_sols,i_insert n ext,Extra_info.rightmost_pivot w info) ;;

    let select (i,j,k) n (MM(_sols,ext,info)) (new_sols:solution list) = 
          MM(new_sols,ext,Extra_info.select (i,j,k) n info) ;;

    let solutions (MM(sols, _ext,_))= sols ;; 
    
    let solutions_and_forced_elements (MM(sols, ext,_))= (sols,ext) ;;  

    let translate d (MM(sols, ext,extra_info)) =
        let tr = (fun x->Image.image(fun t->t+d) x) in 
        MM(Image.image tr sols,tr ext,extra_info) ;; 
    
    
    
    
    
  end ;;


module Handle = struct 

let translate d handle = 
   match handle with 
  Discrete
| Rightmost_pivot(_) -> handle 
| Select (i,j,k) -> Select (i+d,j+d,k+d)
| Rightmost_overflow (i,j,k) -> Rightmost_overflow (i+d,j+d,k+d)
| Fork (i,j,k) -> Fork(i+d,j+d,k+d) ;; 

end ;;  

module Diagnosis = struct 

  let is_unfinished  = function
    Missing_treatment(_) 
  | Incomplete_treatment (_) 
  | Missing_links(_,_) -> true
  | Finished(_,_,_) -> false ;;

  let translate d  diag = match diag with   
    Missing_treatment(_) 
  | Incomplete_treatment (_) 
  | Missing_links(_,_) -> diag 
  | Finished(handle,mold,is_new) -> Finished(Handle.translate d handle,Medium_mold.translate d mold,is_new) ;; 
  
  exception To_bare_answer_exn of medium_diagnosis ;; 

  let to_bare_answer  diag = match diag with
    Missing_treatment(_) 
  | Incomplete_treatment (_) 
  | Missing_links(_,_) -> raise(To_bare_answer_exn(diag))
  | Finished(handle,mold,_) -> (handle,mold) ;;

  end ;;  

module Store = struct 

  exception Select_case_opt_exn of point_with_breadth ;;
  exception Fork_case_opt_exn of point_with_breadth ;;
  exception Missing_links_in_store_exn of point_with_breadth * (int list);;
  
  exception With_outside_help_exn of point_with_breadth ;;
  exception Missing_prelude_to_link_exn of point_with_breadth * point_with_breadth ;;

  module Private = struct

  let helpers_ref = ref [

  ] ;; 

  let pair_level_ref = ref [
     
  ] ;;
  let triple_level_ref = ref [
     (* (W 1,[],0),(fun n->Width_one.compute(FIS(n,[]))) *)
  ] ;;
  let low_level_ref = ref [] ;;
  
    

    let translate_pair d (handle,mold) =
         (Handle.translate d handle,Medium_mold.translate d mold);;


    let no_expansions_on_grounded_point_opt pwb = 
      if Point_with_breadth.is_discrete pwb 
      then let domain = Point_with_breadth.supporting_set pwb in 
           Some(Discrete,Medium_mold.discrete domain) 
      else     
      let (PWB(P(FIS(n,scr),w),b)) = pwb in  
      let wpair = (w,scr) in
      match List.assoc_opt wpair (!pair_level_ref) with 
      Some (f) -> let (handle,mold) =f b n in 
                  Some(handle,mold)    
    | None ->
      let wtriple = (w,scr,b) 
      and n =  Point_with_breadth.size  pwb  in 
      match List.assoc_opt wtriple (!triple_level_ref) with 
        Some (f) -> let (handle,mold) =f n in 
                    Some(handle,mold)    
      | None ->
         (  
          match List.assoc_opt pwb (!low_level_ref) with 
          Some (answer) -> let (handle,mold) =answer in 
                           Some(handle,mold)    
        | None -> None
           ) ;;    
    
    let no_expansion_on_translatable_point pwb =
      let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
      match no_expansions_on_grounded_point_opt translated_pwb with
       None -> (None,Some translated_pwb)
      |Some(handle,mold) -> (Some(translate_pair d (handle,mold)),None);;    
                
    let no_expansion_on_translatable_removal pwb t =
      no_expansion_on_translatable_point (Point_with_breadth.remove_element pwb t) ;;

    let no_expansion_on_translatable_removals pwb elts =
       let rec tempf = (fun
          (treated,to_be_treated) -> match to_be_treated with 
           [] -> (Some (List.rev treated),None) 
          |elt :: other_elts ->
            (
              let (opt_good,opt_bad) = no_expansion_on_translatable_removal pwb elt in 
              match opt_good with 
               None -> (None,opt_bad)
              |Some(_handle,mold) -> tempf ((elt,mold)::treated,other_elts)
            )
       ) in 
       tempf ([],elts) ;; 
    

           

           
  let explore_select_possibility_on_grounded_point pwb prec_pwb cstr prec_mold= 
    let prec_sols = Medium_mold.solutions prec_mold in 
    let new_sols = List.filter (Point_with_breadth.subset_is_admissible pwb) prec_sols in 
    if new_sols = []
    then Incomplete_treatment(prec_pwb)
    else let nth = (fun k->List.nth cstr (k-1)) in  
         let i=nth 1 and j=nth 2 and k=nth 3 in 
         let n = Point_with_breadth.max prec_pwb in 
         Finished(Select(i,j,k),Medium_mold.select (i,j,k) n prec_mold new_sols,true);;   
    
  let explore_rightmost_overflow_possibility_on_grounded_point left_pwb left_mold left_ext (u,v,n) =
    let forgotten_links = i_setminus [u;v] left_ext in 
    if forgotten_links = []
    then Finished(Rightmost_overflow(u,v,n),Medium_mold.rightmost_overflow (u,v,n) left_mold,true)
    else 
    let (_opt_good,opt_bad) = no_expansion_on_translatable_removals left_pwb [u;v] in   
    match opt_bad with 
     None -> Missing_links(left_pwb,forgotten_links) 
     |Some pwb4 -> Missing_treatment pwb4  ;;            
              
              
  let explore_rightmost_pivot_possibility_on_grounded_point pwb left_pwb left_mold left_sols n = 
        let new_sols = List.filter_map (
                              fun sol -> 
                                let new_sol = i_insert n sol in 
                                if Point_with_breadth.subset_is_admissible pwb new_sol 
                                then Some new_sol
                                else None  
        ) left_sols in 
        if new_sols = []
        then Incomplete_treatment(left_pwb)
        else Finished(Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),
            Medium_mold.rightmost_pivot (Point_with_breadth.rightmost_largest_width pwb) left_mold n new_sols,true);;    
            
    exception Explore_fork_possibility_exn of point_with_breadth ;;
                
    let explore_fork_possibility_on_grounded_point prec_pwb cstr prec_ext = 
            let nth = (fun k->List.nth cstr (k-1)) in  
            let i=nth 1 and j=nth 2 and k=nth 3 in 
            let forgotten_links=i_setminus [i;j;k] prec_ext in 
            if forgotten_links<>[]
            then Missing_links(prec_pwb,forgotten_links) 
            else     
            let (opt_good,opt_bad) = no_expansion_on_translatable_removals prec_pwb [i;j;k] in 
            match opt_bad with
              Some(grounded_pwb2)-> Missing_treatment grounded_pwb2
            |None ->
              let mth = (fun k->snd(List.nth (Option.get opt_good) (k-1))) in  
              Finished(Fork (i,j,k),Medium_mold.fork (mth 1,mth 2,mth 3),true);;               

    let rec finished_item_in_image_opt f pairs = 
       match pairs with 
       [] -> None 
       |pair::other_pairs ->
         let diag = f pair in 
         if Diagnosis.is_unfinished diag 
         then  finished_item_in_image_opt f other_pairs
         else Some diag
      ;;

    let minimal_expansions_opt_without_outside_help pwb = 
      let opt1 = no_expansions_on_grounded_point_opt pwb in
      if opt1<>None
      then let (handle,mold) = Option.get opt1 in Finished(handle,mold,false)
      else
      let opt2 = Point_with_breadth.usual_decomposition_opt pwb in 
      if opt2 = None 
      then let domain = Point_with_breadth.supporting_set pwb in Finished(Discrete,Medium_mold.discrete domain,false)
      else
      let (prec_pwb,C cstr) = Option.get opt2 in 
      let opt3 = no_expansions_on_grounded_point_opt prec_pwb in 
      if opt3 = None
      then Missing_treatment(prec_pwb)
      else 
      let (_,prec_mold) = Option.get opt3 in 
      let trial1 =  explore_select_possibility_on_grounded_point pwb prec_pwb cstr prec_mold in 
      if not(Diagnosis.is_unfinished trial1)
      then trial1
      else
      let n = Point_with_breadth.max pwb in 
      let left_pwb = Point_with_breadth.remove_element pwb n in 
      let opt4 = no_expansions_on_grounded_point_opt left_pwb in 
      if opt4 = None
      then Missing_treatment(left_pwb)
      else 
      let (_,left_mold) = Option.get opt4 in   
      let left_sols = Medium_mold.solutions left_mold in 
      let trial2 = explore_rightmost_pivot_possibility_on_grounded_point pwb left_pwb left_mold left_sols n in 
      if not(Diagnosis.is_unfinished trial1)
      then trial2
      else
      let left_ext = Medium_mold.forced_elements left_mold in   
      let complements = Point_with_breadth.complementary_pairs pwb in  
      let opt5 = finished_item_in_image_opt (
                fun (u,v) -> explore_rightmost_overflow_possibility_on_grounded_point left_pwb left_mold left_ext (u,v,n)
      ) complements in 
      if opt5<>None
      then Option.get opt5
      else 
      let prec_ext = Medium_mold.forced_elements prec_mold in    
      explore_fork_possibility_on_grounded_point prec_pwb cstr prec_ext ;;
  
  let select_case_with_outside_help pwb prec_pwb cstr=
    match no_expansions_on_grounded_point_opt prec_pwb with 
     None -> Missing_treatment(prec_pwb)
    |Some(_,prec_mold) -> explore_select_possibility_on_grounded_point pwb prec_pwb cstr prec_mold;;    

  let rightmost_pivot_with_outside_help pwb = 
      let n = Point_with_breadth.max pwb in 
      let left_pwb = Point_with_breadth.remove_element pwb n in 
        match no_expansions_on_grounded_point_opt left_pwb with 
        None -> Missing_treatment(left_pwb)
      |Some(_,left_mold) ->
        let left_sols = Medium_mold.solutions left_mold in 
        explore_rightmost_pivot_possibility_on_grounded_point pwb left_pwb left_mold left_sols n;;    

   let rightmost_overflow_with_outside_help pwb (u,v,n) = 
      let left_pwb = Point_with_breadth.remove_element pwb n in 
      match no_expansions_on_grounded_point_opt left_pwb with 
        None -> Missing_treatment(left_pwb)
      |Some(_,left_mold) ->
        let left_ext = Medium_mold.forced_elements left_mold in 
        explore_rightmost_overflow_possibility_on_grounded_point left_pwb left_mold left_ext (u,v,n) ;;    

  let fork_case_with_outside_help prec_pwb cstr = 
            match no_expansions_on_grounded_point_opt prec_pwb with 
             None -> Missing_treatment(prec_pwb)
            |Some(_,prec_mold) -> 
              let prec_ext = Medium_mold.forced_elements prec_mold in  
              explore_fork_possibility_on_grounded_point prec_pwb cstr prec_ext;;  


  let minimal_expansions_opt_with_outside_help pwb handle = 
    match no_expansions_on_grounded_point_opt pwb with  
        Some(handle,mold) -> Finished(handle,mold,false)
       |None ->
        (match Point_with_breadth.usual_decomposition_opt pwb with 
          None -> let domain = Point_with_breadth.supporting_set pwb in 
                  Finished(Discrete,Medium_mold.discrete domain,false)
          |Some(prec_pwb,C cstr) -> 
    match handle with
    Discrete -> (* this should never happen, the discrete case is already treated above *) 
                 raise(With_outside_help_exn(pwb))
   |Select(_,_,_)->select_case_with_outside_help pwb prec_pwb cstr          
   |Rightmost_pivot(_)->rightmost_pivot_with_outside_help pwb
   |Rightmost_overflow(u,v,n)->rightmost_overflow_with_outside_help pwb (u,v,n)
   |Fork(_,_,_)->fork_case_with_outside_help prec_pwb cstr);;

  let minimal_expansions_opt pwb ~use_outside_help =
      if use_outside_help
      then minimal_expansions_opt_with_outside_help pwb (Analysis_with_breadth.handle pwb)
      else minimal_expansions_opt_without_outside_help pwb ;;

      module Help = struct 

        exception False_links_exn of point_with_breadth * (int list);;
        exception False_solution_exn of point_with_breadth * solution ;;
      
      let argument = function 
      Help_with_solution (pwb,_) -> pwb
      | Help_with_links (pwb,_) -> pwb ;;
      
      let rec assoc_opt pwb helper_list = match helper_list with 
        [] -> None 
        | helper :: other_helpers ->
            if argument helper = pwb
            then Some helper
            else assoc_opt pwb other_helpers ;;   
      
      let with_links  pwb old_mold data_for_links  =
         let m = Medium_mold.measure(old_mold) -1 in 
         let counterexamples = List.filter (fun 
           (_t,md)->Medium_mold.measure(md)<>m
         ) data_for_links in 
         if counterexamples<>[]
         then raise(False_links_exn(pwb,Image.image fst counterexamples)) 
         else
         let links = Image.image fst data_for_links in 
         Medium_mold.add_links old_mold links;;
        
        let with_solution  pwb old_mold new_sol  =
         let m = Medium_mold.measure(old_mold) in 
         if List.length(new_sol)<>m
         then raise(False_solution_exn(pwb,new_sol)) 
         else Medium_mold.add_solution old_mold  new_sol;;
      
         let apply_helper_to_mold helper pwb mold =
          match helper with 
           Help_with_solution(_,sol) -> with_solution pwb mold sol
          |Help_with_links(_,links) ->  
             let (good_links_opt,bad_link_opt) = no_expansion_on_translatable_removals pwb links in 
             match bad_link_opt with 
              Some bad_link-> raise(Missing_prelude_to_link_exn(pwb,bad_link))
              |None -> with_links pwb mold (Option.get good_links_opt) ;;

         let apply_current_helplist_to_mold pwb mold =
          match  assoc_opt pwb (!helpers_ref) with 
          None -> mold
         |(Some helper) -> apply_helper_to_mold helper pwb mold ;;

         let apply_both_help_and_last_minute_deductions_on_mold pwb mold =
          Medium_mold.last_minute_deduction pwb (apply_current_helplist_to_mold pwb mold);; 

         let apply_both_help_and_last_minute_deductions_on_diagnosis pwb diag =
             match diag with 
              Missing_treatment(_) |Incomplete_treatment (_) |Missing_links (_,_) -> diag
             |Finished(handler,mold,is_new) -> 
              Finished(handler,apply_both_help_and_last_minute_deductions_on_mold pwb mold,is_new) ;;

      end ;;   
        
    let compute_on_grounded_point_opt pwb ~use_outside_help= 
       Help.apply_both_help_and_last_minute_deductions_on_diagnosis pwb 
        (minimal_expansions_opt pwb ~use_outside_help);;                 
  


  let compute_opt pwb ~use_outside_help=
    let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
    Diagnosis.translate d (compute_on_grounded_point_opt translated_pwb ~use_outside_help);;    
  

   let unsafe_low_level_add pwb mold = (low_level_ref:=(pwb,mold)::(!low_level_ref));;  
   let unsafe_pair_level_add pair f = (pair_level_ref:=(pair,f)::(!pair_level_ref));;  
   let unsafe_triple_level_add triple f = (triple_level_ref:=(triple,f)::(!triple_level_ref));;  

   let reset_all () =
       (
         low_level_ref:=[];
         pair_level_ref:=[];
         triple_level_ref:=[];
       );;
    let reset_low_level () = (low_level_ref:=[]) ;;

  end ;;     
  
   
  let compute_opt = Private.compute_opt ;;
  let reset_low_level = Private.reset_low_level ;;
  let unsafe_low_level_add = Private.unsafe_low_level_add ;; 
  let unsafe_pair_level_add = Private.unsafe_pair_level_add ;; 
  let unsafe_triple_level_add = Private.unsafe_triple_level_add ;; 

  end ;;  
 

module Medium_analysis = struct 

  exception Walk_scale_exn of int * medium_diagnosis ;; 
  exception Nonstandard_handler_exn of point_with_breadth * handle * medium_mold ;;
    
    module Private = struct
    
      let try_to_compute_on_grounded_point pwb = 
        let diag = Store.compute_opt pwb ~use_outside_help:true in 
        let _ =(match diag with 
         Missing_treatment(_) |Incomplete_treatment (_) |Missing_links (_,_) -> ()
        |Finished(handler,mold,is_new)->
          if is_new 
          then  (
                if handler<>Analysis_with_breadth.handle pwb
                then raise(Nonstandard_handler_exn(pwb,handler,mold))
                else Store.unsafe_low_level_add pwb (handler,mold)
              )
        ) in 
        diag ;; 


      let try_to_compute pwb =     
        let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
        let  translated_diag = try_to_compute_on_grounded_point translated_pwb in 
        Diagnosis.translate d translated_diag;;

      let walk_scale (w,scr,b) bound = 
         let base = Int_range.range 1 bound in 
          Image.image (fun n->
            let pwb = PWB(P(Finite_int_set.constructor n scr,w),b) in 
            (n,Diagnosis.to_bare_answer(try_to_compute pwb))
            ) base ;;  
    
    
      end ;;     
    
      let force_compute pwb =Diagnosis.to_bare_answer(Private.try_to_compute pwb);;
      let try_to_compute = Private.try_to_compute ;;
      let walk_scale = Private.walk_scale ;; 
    
    end ;;  
    
      
module Safe_initialization = struct 

exception Undefined_during_check_exn of point_with_breadth * medium_diagnosis ;;   
exception Unequal_during_check_exn of (point_with_breadth * (handle * medium_mold) * (handle * medium_mold)) ;;


module Private = struct 

   let max_size = 25 ;;
   let max_breadth = 25 ;; 
   let counterexamples_ref = ref [];;

  let check_for_triple (w,scr,b) f must_return_to_old_state= 
    let old_state = (!(Store.Private.low_level_ref)) in 
     let analize =(fun n->
       let pwb = PWB(P(Finite_int_set.constructor n scr,w),b) in 
       let diag = Medium_analysis.try_to_compute pwb in 
       if Diagnosis.is_unfinished diag 
       then  raise(Undefined_during_check_exn(pwb,diag))
       else
       let hm = Diagnosis.to_bare_answer diag in 
       if hm <> (f n)
       then counterexamples_ref:=(pwb,hm,f n)::(!counterexamples_ref)
     ) in 
     let _ = (
       for k= 1 to max_size do analize k done;
       if must_return_to_old_state 
       then Store.Private.low_level_ref:=old_state
     ) in 
     let counterexamples = (!counterexamples_ref) in 
     if counterexamples <> []
     then raise(Unequal_during_check_exn(List.hd counterexamples))
     else () ;;
      
   let check_for_pair (w,scr) g = 
    let old_state = (!(Store.Private.low_level_ref)) in 
    (
    for b= 0 to max_breadth do check_for_triple (w,scr,b) (g b) false done;
    Store.Private.low_level_ref:=old_state
    )
  ;;
       
   

end ;;   

let counterexamples_from_last_computation () =
    List.rev(!(Private.counterexamples_ref)) ;; 

let pair_level_add (w,scr) g =
  let _ = Private.check_for_pair (w,scr) g in 
  Store.unsafe_pair_level_add (w,scr) g ;;

let triple_level_add (w,scr,b) f=
  let _ = Private.check_for_triple (w,scr,b) f true in 
  Store.unsafe_triple_level_add (w,scr,b) f ;;  

end ;;  

module Initialization = struct 

  
end ;;   
