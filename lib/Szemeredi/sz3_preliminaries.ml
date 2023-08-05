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

type crude_handle = Sz3_types.crude_handle = 
    Cr_Discrete
   |Cr_Select of int * int * int 
   |Cr_Rightmost_pivot 
   |Cr_Fork of int * int * int ;;

type helper = Sz3_types.helper = 
   Help_with_solution of point_with_breadth * solution 
  |Help_with_links of point_with_breadth * (int list) ;; 

type crude_mold = Sz3_types.crude_mold = CM of (solution list) * extension_data ;;  

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

let small_standardization_opt pwb =
    let (PWB(pt,b)) = pwb in 
    if b<>0
    then Some pwb 
    else 
      let (P(fis,_)) = pt in 
    match Point.highest_constraint_opt pt with  
     None -> None
    |Some(C cstr) -> 
      let nth = (fun k->List.nth cstr (k-1)) in 
      let w = W((nth 2)-(nth 1)-1) in 
      Some(PWB(P(fis,w),nth 1));;
    ;; 

let usual_decomposition_opt pwb =
   match small_standardization_opt pwb with 
    None -> None 
   |Some(PWB(pt,b)) -> 
      let (W w) = Point.width pt in 
      Some(PWB(pt,b-1),C[b;b+(w+1);b+2*(w+1)]);;

  (* when d<0, this opeartion makes sense 
  only if the lowest element is >|d|. *)    
  let translate d (PWB(pwc,b)) = 
    (*
      when b+d<0, there is no extra constraint, 
      and this is equivalent to setting the new b to 0S    
    *) 
    let new_b = max(0)(b+d) in
      PWB(Point.translate d pwc,new_b) ;;


end ;;  

let breadth (PWB(_pt,b))= b ;;
let decompose_wrt_translation pwb = 
  let (PWB(pt,_b)) = pwb in 
  let (d,_) = Point.decompose_wrt_translation pt in 
  (d,Private.translate (-d) pwb);; 

let everything_but_the_size (PWB(P(FIS(_n,scr),w),b)) = (w,scr,b) ;;  
let is_discrete pwb = Point_with_extra_constraints.is_discrete (Private.to_extra_constraints pwb) ;; 
let max (PWB(pt,_b)) = Point.max pt ;;
let to_extra_constraints = Private.to_extra_constraints ;; 
let remove_element (PWB(pt,b)) elt = PWB(Point.remove_element pt elt,b);;
let size (PWB(P(FIS(n,_scr),_w),_b)) = n ;;  
let subset_is_admissible pwb subset = 
     Point_with_extra_constraints.subset_is_admissible (Private.to_extra_constraints pwb) subset;; 

let supporting_set pwb = Point_with_extra_constraints.supporting_set (Private.to_extra_constraints pwb) ;; 
let translate = Private.translate ;; 
let usual_decomposition_opt = Private.usual_decomposition_opt ;; 
let width (PWB(pt,_b))= Point.width pt ;;

end ;;  


module Analysis_with_breadth = struct 

exception Explain_exn of point_with_breadth ;;

module Private = struct

let standard_solution = Memoized.make(fun pwb->
    Analysis_with_extra_constraints.standard_solution (Point_with_breadth.to_extra_constraints pwb)
)  ;;

let measure = Memoized.make(fun pwb->
  Analysis_with_extra_constraints.measure (Point_with_breadth.to_extra_constraints pwb)
)  ;;

let compute_crude_handle = Memoized.make(fun pwb->
   match Point_with_breadth.usual_decomposition_opt pwb with 
       None -> Cr_Discrete
       |Some(preceding_pwb,C cstr) ->
        let nth = (fun k->List.nth cstr (k-1)) in 
        if measure pwb = measure preceding_pwb 
        then Cr_Select(nth 1,nth 2,nth 3)
        else (
          let pwc = Point_with_breadth.to_extra_constraints pwb in 
          if Analysis_with_extra_constraints.test_for_rightmost_pivot pwc 
          then Cr_Rightmost_pivot
          else Cr_Fork(nth 1,nth 2,nth 3)
        )    
    )  ;; 
   

end ;; 

let compute_crude_handle = Private.compute_crude_handle ;;
let measure = Private.measure ;;
let standard_solution = Private.standard_solution ;;

end ;;   

module Crude_handle = struct 

let translate d handle = match handle with  
    Cr_Discrete
  | Cr_Rightmost_pivot -> handle
  | Cr_Select (i,j,k) -> Cr_Select (i+d,j+d,k+d)
  | Cr_Fork (i,j,k) -> Cr_Fork (i+d,j+d,k+d) ;; 

end ;;  

  

type medium_mold = MM of (solution list) * extension_data ;;  

type medium_diagnosis  = 
      Missing_treatment of point_with_breadth 
     |Incomplete_treatment of point_with_breadth 
     |Missing_links of point_with_breadth * (int list)
     |Finished of medium_mold;;   


  module Medium_mold = struct 

    let add_links (MM(sols,ext)) links = MM(sols,i_merge links ext) ;;

    let add_solution (MM(sols,ext)) new_sol = MM(il_insert new_sol sols,ext) ;;

    let constructor sols ext= MM(sols,ext) ;;  

    let discrete domain = MM([domain],domain) ;;   
    
    let fork (MM(_sols1,ext1),MM(_sols2,ext2),MM(sols3,ext3)) =
      let final_ext = i_fold_intersect [ext1;ext2;ext3] in 
      MM(sols3,final_ext);;   ;;

    let forced_elements (MM(_sols, ext))= ext ;; 
    
    let of_solutions sols = MM(sols,[]) ;; 
    
    let rightmost_overflow (MM(sols,_ext)) = MM(sols,[]) ;;
    
    let rightmost_pivot (MM(_sols,ext)) n (new_sols:solution list) = MM(new_sols,i_insert n ext) ;;

    let select (MM(_sols,ext)) (new_sols:solution list) = MM(new_sols,ext) ;;

    let translate d (MM(sols, ext)) =
        let tr = (fun x->Image.image(fun t->t+d) x) in 
        MM(Image.image tr sols,tr ext) ;; 
    
    let measure (MM(sols, _ext)) = List.length(List.hd sols) ;; 
    
    let solutions (MM(sols, _ext))= sols ;; 
    
    let solutions_and_forced_elements (MM(sols, ext))= (sols,ext) ;;  
    
  end ;;

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


end ;;   


module Store = struct 

  exception Select_case_opt_exn of point_with_breadth ;;
  exception Fork_case_opt_exn of point_with_breadth ;;
  exception Missing_links_in_store_exn of point_with_breadth * (int list);;
  

  module Private = struct

  let helpers_ref = ref [

  ] ;; 

  let pair_level_ref = ref [
     
  ] ;;
  let triple_level_ref = ref [
     (* (W 1,[],0),(fun n->Width_one.compute(FIS(n,[]))) *)
  ] ;;
  let low_level_ref = ref [] ;;
  
  module Without_translations = struct 

    let no_expansions_opt pwb = 
      if Point_with_breadth.is_discrete pwb 
      then let domain = Point_with_breadth.supporting_set pwb in 
           Some(Medium_mold.discrete domain) 
      else     
      let (PWB(P(FIS(n,scr),w),b)) = pwb in  
      let wpair = (w,scr) in
      match List.assoc_opt wpair (!pair_level_ref) with 
      Some (f) -> Some (f b n)
    | None ->
      let wtriple = (w,scr,b) 
      and n =  Point_with_breadth.size  pwb  in 
      match List.assoc_opt wtriple (!triple_level_ref) with 
        Some (f) -> Some (f n)
      | None ->
         (  
          match List.assoc_opt pwb (!low_level_ref) with 
          Some (answer) -> Some answer
        | None -> None
           ) ;;    
  
    let minimal_expansions_opt pwb = 
        match no_expansions_opt pwb with  
        Some easy_answer -> (Some easy_answer,false)
       |None ->(
          let n = Point_with_breadth.max pwb in 
          let left_pwb = Point_with_breadth.remove_element pwb n in 
            match no_expansions_opt left_pwb with 
             None -> (None,false) 
            |Some(left_mold) ->
                  let (left_sols,left_ext) = Medium_mold.solutions_and_forced_elements left_mold in 
                  let new_ext = i_insert n left_ext in 
                  if (not (Point_with_breadth.subset_is_admissible pwb new_ext))
                  then (Some(Medium_mold.rightmost_overflow left_mold),true)
                  else
                  let new_sols = List.filter_map (fun old_sol->
                    let new_sol = i_insert n old_sol in 
                     if Point_with_breadth.subset_is_admissible pwb new_sol
                     then Some new_sol
                     else None 
                  ) left_sols in 
                  if new_sols <> []
                  then (Some(Medium_mold.rightmost_pivot left_mold n new_sols),true)
                else (
                  match Point_with_breadth.usual_decomposition_opt pwb with 
                   None -> (None,false)
                   | Some(prec_pwb,_) ->
                match no_expansions_opt prec_pwb with 
                None -> (None,false)
                | Some(prec_mold) ->  
                  let prec_sols = Medium_mold.solutions prec_mold in 
                  let new_sols = List.filter (Point_with_breadth.subset_is_admissible pwb) prec_sols in 
                  if new_sols <> []
                  then (Some(Medium_mold.select prec_mold new_sols),true)
                  else (None,false)
                )      
             ) ;;
        
  let select_case_opt pwb = 
      match Point_with_breadth.usual_decomposition_opt pwb with 
        None -> raise(Select_case_opt_exn(pwb))
      |Some(prec_pwb,_) ->
          match no_expansions_opt prec_pwb with 
            None -> Missing_treatment(prec_pwb)
          |Some(prec_mold) ->
            let prec_sols = Medium_mold.solutions prec_mold in 
             let new_sols = List.filter (Point_with_breadth.subset_is_admissible pwb) prec_sols in 
              if new_sols = []
              then Incomplete_treatment(prec_pwb)
              else Finished(Medium_mold.select prec_mold new_sols);;              

    let rightmost_pivot_case_opt pwb = 
        let n = Point_with_breadth.max pwb in 
        let left_pwb = Point_with_breadth.remove_element pwb n in 
          match no_expansions_opt left_pwb with 
           None -> Missing_treatment(left_pwb)
         |Some(left_mold) ->
          let left_sols = Medium_mold.solutions left_mold in 
          let new_sols = List.filter_map (
                  fun sol -> 
                    let new_sol = i_insert n sol in 
                    if Point_with_breadth.subset_is_admissible pwb new_sol 
                    then Some new_sol
                    else None  
          ) left_sols in 
          if new_sols = []
          then Incomplete_treatment(left_pwb)
          else Finished(Medium_mold.rightmost_pivot left_mold n new_sols);;  
  
  let no_expansions_but_allow_translations_opt pwb = 
    let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
      match no_expansions_opt translated_pwb with 
      None -> None              
      |Some translated_mold -> Some(Medium_mold.translate d translated_mold);;  

      exception Try_direct_fork_case_exn of point_with_breadth ;;
  
  let fork_case_opt pwb = 
    match Point_with_breadth.usual_decomposition_opt pwb with 
    None -> raise(Fork_case_opt_exn(pwb))
  |Some(prec_pwb,C cstr) ->
    match no_expansions_opt prec_pwb with 
    None -> Missing_treatment(prec_pwb)
  |Some(prec_mold) -> 
    let prec_ext = Medium_mold.forced_elements prec_mold in  
    let nth = (fun k->List.nth cstr (k-1)) in  
    let i=nth 1 and j=nth 2 and k=nth 3 in 
    let offshoots = Image.image (fun t->
            let pwb2=Point_with_breadth.remove_element prec_pwb t in 
            let (d,core_pwb2) = Point_with_breadth.decompose_wrt_translation pwb2 in 
            let core_opt = no_expansions_opt core_pwb2 in
            let opt = Option.map (Medium_mold.translate d) core_opt in
            (pwb2,core_pwb2,opt)
    ) [i;j;k] in
    match List.find_opt (fun (_,_,opt)->opt=None) offshoots with
     Some(_,core_pwb3,_)-> Missing_treatment core_pwb3
    |None ->
      let forgotten_links=i_setminus [i;j;k] prec_ext in 
      if forgotten_links<>[]
      then Missing_links(prec_pwb,forgotten_links) 
      else     
      let offshoots2 = Image.image (
               fun (_,_,opt)->Option.get opt
      ) offshoots in 
      let mth = (fun k->List.nth offshoots2 (k-1)) in  
      Finished(Medium_mold.fork (mth 1,mth 2,mth 3));;  
  
  let without_helpers_opt pwb = 
      let (opt,is_new) =  minimal_expansions_opt pwb in 
        match opt with 
      Some mold -> Some(mold,is_new)
     |None -> 
        (
          match select_case_opt pwb with 
            Finished(mold2) -> Some (mold2,true)
           |Missing_treatment(_) |Incomplete_treatment(_) |Missing_links(_,_) ->
              match rightmost_pivot_case_opt pwb with 
              Finished(mold3) -> Some (mold3,true)
             |Missing_treatment(_) |Incomplete_treatment(_) |Missing_links(_,_) ->
              match fork_case_opt pwb with 
                Finished(mold4) -> Some (mold4,true)
               |Missing_treatment(_) |Incomplete_treatment(_) |Missing_links(_,_) ->
                            None
          ) ;;             

    let apply_helper helper pwb pre_answer =
          match helper with 
           Help_with_solution(_,sol) -> Help.with_solution pwb pre_answer sol
          |Help_with_links(_,links) ->  
             let temp1 = Image.image (fun t->
               (t,no_expansions_opt(Point_with_breadth.remove_element pwb t))
              ) links in 
              let (bad_links,good_links) = List.partition (fun (_t,opt)->opt=None) temp1 in
              if bad_links<>[]
              then raise(Missing_links_in_store_exn(pwb,Image.image fst bad_links))
              else let links_with_data = Image.image (fun (t,opt)->(t,Option.get opt)) good_links in 
                   Help.with_links pwb pre_answer links_with_data
         ;;

   let compute_opt pwb = 
      match without_helpers_opt pwb with 
      None -> None 
     |Some (pre_answer,is_new) ->
             (
             match Help.assoc_opt pwb (!helpers_ref) with 
              None -> Some(pre_answer,is_new)
             |(Some helper) ->  
               Some(apply_helper helper pwb pre_answer,is_new)
               );;    


  end ;;

  
  let select_case_opt pwb = 
    let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
    let diag = Without_translations.select_case_opt translated_pwb in 
    match diag with 
    Missing_treatment (_)
  | Incomplete_treatment (_)
  | Missing_links (_,_) -> diag
  | Finished (translated_mold) ->
      Finished(Medium_mold.translate d translated_mold);;

  
  let rightmost_pivot_case_opt pwb = 
    let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
    let diag = Without_translations.rightmost_pivot_case_opt translated_pwb in 
    match diag with 
    Missing_treatment (_)
  | Incomplete_treatment (_)
  | Missing_links (_,_) -> diag
  | Finished (translated_mold) ->
      Finished(Medium_mold.translate d translated_mold);;      
  
   
  let fork_case_opt pwb = 
        let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
        let diag = Without_translations.fork_case_opt translated_pwb in 
        match diag with 
        Missing_treatment (_)
      | Incomplete_treatment (_)
      | Missing_links (_,_) -> diag
      | Finished (translated_mold) ->
          Finished(Medium_mold.translate d translated_mold);;   



  let compute_opt pwb =
    let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
    match Without_translations.compute_opt translated_pwb with
      None -> None
    |Some(sol,is_new) -> Some(Medium_mold.translate d sol,is_new);;    
  

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
  let fork_case_opt = Private.fork_case_opt ;;  
  let rightmost_pivot_case_opt = Private.rightmost_pivot_case_opt ;;  
  let reset_all = Private.reset_all ;;
  let reset_low_level = Private.reset_low_level ;;
  let select_case_opt = Private.select_case_opt ;;
  let unsafe_low_level_add = Private.unsafe_low_level_add ;; 
  let unsafe_pair_level_add = Private.unsafe_pair_level_add ;; 
  let unsafe_triple_level_add = Private.unsafe_triple_level_add ;; 

  end ;;  
 
module Medium_analysis = struct 

  exception Walk_scale_exn of int * medium_diagnosis ;; 
    
    module Private = struct
    
      let try_to_compute_in_select_case pwb = 
        let direct =Store.select_case_opt pwb in 
        match direct with 
        Finished(mold3) -> (Some mold3,None)
        |Missing_treatment(_) |Incomplete_treatment(_) |Missing_links(_,_) -> (None,Some direct) ;; 
    
    
    let try_to_compute_in_rightmost_pivot_case pwb = 
      let direct =Store.rightmost_pivot_case_opt pwb in 
      match direct with 
      Finished(mold3) -> (Some mold3,None)
      |Missing_treatment(_) |Incomplete_treatment(_) |Missing_links(_,_) -> (None,Some direct) ;; 
    
    let try_to_compute_in_fork_case pwb = 
        let direct =Store.fork_case_opt pwb in 
        match direct with 
        Finished(mold3) -> (Some mold3,None)
        |Missing_treatment(_) |Incomplete_treatment(_) |Missing_links(_,_) -> (None,Some direct) ;; 
      
      
    exception Try_to_compute_exn of point_with_breadth;;
    
    let try_to_compute_without_using_translations pwb =
      match Store.compute_opt pwb with 
      Some (mold,is_new) -> ((Some(mold),None),is_new)
      |None -> 
        (
          match Analysis_with_breadth.compute_crude_handle pwb with 
          Cr_Discrete -> (* this should never happen, the discrete case
                          is already treated elsewhere *) 
                      raise(Try_to_compute_exn(pwb))
          |Cr_Rightmost_pivot->(try_to_compute_in_rightmost_pivot_case pwb,true) 
          |Cr_Select(_,_,_)->(try_to_compute_in_select_case pwb,true) 
          |Cr_Fork(_,_,_)->(try_to_compute_in_fork_case pwb,true)            
        ) ;; 
    
      let try_to_compute pwb =
        let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
        let ((opt_mold,extra_info),is_new) = try_to_compute_without_using_translations translated_pwb in 
        match opt_mold with 
         None -> (None,extra_info)
        |Some(translated_mold) -> 
            let final_sol = Medium_mold.translate d translated_mold in 
            let _ = (
             if is_new 
             then   Store.unsafe_low_level_add translated_pwb translated_mold) in 
            (Some(final_sol),None);;

      let walk_scale (w,scr,b) bound = 
         let base = Int_range.range 1 bound in 
          let temp1 = Image.image (fun n->
            let pwb = PWB(P(Finite_int_set.constructor n scr,w),b) in 
            (n,try_to_compute pwb)
            ) base in 
          match List.find_opt (fun (_n,(mold_opt,_extra_info))->mold_opt=None) temp1 with 
         Some(n0,(_,extra_info0))->raise(Walk_scale_exn(n0,Option.get extra_info0))
        |None -> Image.image (fun (n,(mold_opt,_extra_info))->(n,Option.get mold_opt)) temp1;;  
    
    
      end ;;     
    
      let force_compute pwb = (Analysis_with_breadth.compute_crude_handle pwb, Option.get(fst(Private.try_to_compute pwb))) ;;
      let try_to_compute = Private.try_to_compute ;;
      let walk_scale = Private.walk_scale ;; 
    
    end ;;  
    
      
module Safe_initialization = struct 

exception Undefined_during_check_exn of point_with_breadth * medium_diagnosis ;;   
exception Unequal_during_check_exn of (point_with_breadth * medium_mold * medium_mold) ;;


module Private = struct 

   let max_size = 25 ;;
   let max_breadth = 25 ;; 
   let counterexamples_ref = ref [];;

  let check_for_triple (w,scr,b) f must_return_to_old_state= 
    let old_state = (!(Store.Private.low_level_ref)) in 
     let analize =(fun n->
       let pwb = PWB(P(Finite_int_set.constructor n scr,w),b) in 
       let (opt_mold,extra_info) = Medium_analysis.try_to_compute pwb in 
       match opt_mold with 
        None -> raise(Undefined_during_check_exn(pwb,Option.get extra_info))
       |Some(mold) -> 
          if mold <> (f n)
          then counterexamples_ref:=(pwb,mold,f n)::(!counterexamples_ref)
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

  let tf1 =(fun r n->
    let m = min(n/3)(r) in
    let end_part =Int_range.range (3*m+1) n 
    and beginning = List.flatten (Int_range.scale (fun j->[3*j-2;3*j-1]) 1 m) in 
    Medium_mold.constructor [beginning@end_part] end_part     
  ) ;;
  
  let tf2 b n = tf1 (Basic.frac_ceiling b 3) n ;; 

  Safe_initialization.pair_level_add (W 0,[]) tf2 ;;  

  exception Bad_remainder_by_three of int;;

  let tf3 =(fun n->
    let r = n mod 3 in 
    let core = List.filter (fun j->(j mod 3)<>0)(Int_range.range 1 n) in
    let end_part =(
      match r with 
      0 -> [] | 1->[n] |2 ->[n-1;n] |_->raise(Bad_remainder_by_three(r))
    )   in 
    Medium_mold.constructor [core] end_part     
  ) ;;

  let tf4 _b n = tf3 n ;; 
 
  Safe_initialization.pair_level_add (W 1,[]) tf4 ;;   
  Safe_initialization.triple_level_add (W 2,[],0) tf3 ;;

  let tf5 =(fun n->
    match List.assoc_opt n 
    [
       1, Medium_mold.constructor  [[1]] [1];
       2, Medium_mold.constructor  [[1;2]] [1;2];
       3, Medium_mold.constructor  [[1;2]] [];     
    ] with
    Some answer -> answer
    |None ->
    let r = n mod 3 in 
    let core = [1;2]@(List.filter (fun j->(j mod 3)<>1)(Int_range.range 5 n)) in
    let end_part =(
      match r with 
      1 -> [] | 2->[n] |0 ->[n-1;n] |_->raise(Bad_remainder_by_three(r))
    )   in 
    Medium_mold.constructor [core] end_part   
  ) ;;
  
  Safe_initialization.triple_level_add (W 2,[4],0) tf5 ;;

end ;;   
  