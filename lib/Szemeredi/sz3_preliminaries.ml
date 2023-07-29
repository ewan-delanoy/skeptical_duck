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

type point_with_extra_constraints = Sz3_types.point_with_extra_constraints = 
  PEC of point * (constraint_t list);;

type point_with_breadth = Sz3_types.point_with_breadth = PWB of point * int ;; 

type explanation = Sz3_types.explanation = 
   Discrete
  |Pivot of int 
  |Select of int * int * int 
  |Fork of int * int * int ;;

let i_order = Total_ordering.for_integers ;;
let i_does_not_intersect = Ordered.does_not_intersect i_order ;;
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
      None -> 1
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
    M([List.flatten sol_components],List.flatten forced_elements);;

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
    P_Finished_computation of (explanation option) * mold  
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

  let main_hashtbl =   ((Hashtbl.create 50) : (point, mold) Hashtbl.t) ;; 
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
          then  P_Finished_computation(None,M([domain],domain))
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
      Mold.translate d translated_mold);;

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
          then let pair = (pt,(Some(Lucky(pt2,ext2)),M([whole],[]))) in  
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
          then let pair = (pt,(Some(Lucky(pt2,ext2)),M(sols4,[]))) in  
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
      [] -> let pair = (pt,(Some(Disjunction(failures)),M(sols_for_preceding_point,[]))) in  
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
    let pt2 = Point.remove_element pt n in 
    (
      match seek_translated_obvious_access helper pt2 with 
      P_Unfinished_computation(l) ->
         (helper,None,l@(pt::other_items))
      |P_Finished_computation(_,M(sols,ext)) ->
          let ext2 = i_insert n ext in 
          if not(Point.subset_is_admissible pt ext2) 
          then let pair = (pt,(Some(Early_stop(pt2,n)),M(sols,[]))) in 
              (pair::helper,None,other_items)
          else
          let sols2 = List.filter_map (fun sol->
            let increased_sol = i_insert n sol in 
            if Point.subset_is_admissible pt increased_sol 
            then Some(increased_sol) 
            else None    
          ) sols in  
          if sols2 <> [] 
          then let pair = (pt,(Some(Early_increase(pt2,n)),M(sols2,i_insert n ext))) in 
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
   old_f ptwc-> 
     let (PEC(pt,l_cstr)) = ptwc in 
     let stays_admissible = (fun z->List.for_all (
        fun (C cstr)->not(i_is_included_in cstr z)
     ) l_cstr) in 
     let (M(trial1,_)) = Crude_analysis_on_bare_point.compute pt in 
     if List.exists stays_admissible  trial1 
     then List.length(List.hd trial1)
     else 
     let ptwc2 = Point_with_extra_constraints.remove_rightmost_element ptwc
     and ptwc3 = Point_with_extra_constraints.remove_rightmost_element_but_keep_constraints ptwc in 
     max(old_f ptwc2)((old_f ptwc3)+1)
);;

let standard_solution  = Memoized.recursive (fun 
  old_f ptwc-> 
  let (PEC(pt,_l_cstr)) = ptwc in 
  if Point_with_extra_constraints.is_discrete ptwc 
  then Point.supporting_set pt 
  else  
  let ptwc2 = Point_with_extra_constraints.remove_rightmost_element ptwc
  and ptwc3 = Point_with_extra_constraints.remove_rightmost_element_but_keep_constraints ptwc in 
  if (measure ptwc2)>=((measure ptwc3)+1)
  then old_f(ptwc2)
  else (old_f(ptwc3))@[Point.max pt]  
);;

let look_for_pivot  ptwc = 
    let (PEC(pt,_l_cstr)) = ptwc in 
    let domain = List.rev (Point.supporting_set pt) 
    and m = measure(ptwc)-1 in  
    List.find_opt (
       fun p -> measure(Point_with_extra_constraints.remove_element ptwc p)=m
    )  domain;;

end ;; 


let look_for_pivot = Private.look_for_pivot ;;
let measure = Private.measure ;;
let standard_solution = Private.standard_solution ;; 


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
let to_extra_constraints = Private.to_extra_constraints ;; 
let usual_decomposition_opt = Private.usual_decomposition_opt ;; 
let remove_element (PWB(pt,b)) elt = PWB(Point.remove_element pt elt,b);;
let size (PWB(P(FIS(n,_scr),_w),_b)) = n ;;  
let subset_is_admissible pwb subset = 
     Point_with_extra_constraints.subset_is_admissible (Private.to_extra_constraints pwb) subset;; 

let supporting_set pwb = Point_with_extra_constraints.supporting_set (Private.to_extra_constraints pwb) ;; 
let translate = Private.translate ;; 
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

let explain = Memoized.make(fun pwb->
   let pwc = Point_with_breadth.to_extra_constraints pwb in 
   if Point_with_extra_constraints.is_discrete pwc 
   then Discrete
   else 
   match Analysis_with_extra_constraints.look_for_pivot pwc with
   Some pivot -> Pivot(pivot)
   | None ->(
       match Point_with_breadth.usual_decomposition_opt pwb with 
       None -> raise(Explain_exn(pwb))
       |Some(preceding_pwb,C cstr) ->
        let nth = (fun k->List.nth cstr (k-1)) in 
        if measure pwb = measure preceding_pwb 
        then Select(nth 1,nth 2,nth 3)
        else Fork(nth 1,nth 2,nth 3)  
   )) ;; 
   

end ;; 

let explain = Private.explain ;;
let measure = Private.measure ;;
let standard_solution = Private.standard_solution ;;

end ;;   



module Medium_analysis = struct 

let current_bound = 20 ;; 

module Private = struct

let high_level_ref = ref [
   (W 1,[],0),(fun n->Width_one.compute(FIS(n,[])))
] ;;
let low_level_ref = ref [] ;;


let easy_compute_without_using_translations_opt pwb = 
  if Point_with_breadth.is_discrete pwb 
  then let domain = Point_with_breadth.supporting_set pwb in 
       Some(M([domain],domain)) 
  else     
  let triple = Point_with_breadth.everything_but_the_size  pwb 
  and n =  Point_with_breadth.size  pwb  in 
  match List.assoc_opt triple (!high_level_ref) with 
    Some (f) -> Some (f n)
  | None ->
     (  
      match List.assoc_opt pwb (!low_level_ref) with 
      Some (answer) -> Some answer
    | None -> None
       ) ;; 

let easy_compute_opt pwb = 
  let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
  match easy_compute_without_using_translations_opt translated_pwb with 
   None -> None
  |Some translated_mold -> Some(Mold.translate d translated_mold);;

let try_to_compute_in_pivot_case p pwb = 
  let simpler_pwb = Point_with_breadth.remove_element pwb p in 
  match easy_compute_opt simpler_pwb with 
    None -> (None,Some(Pivot(p),Some(simpler_pwb,None)),true)
   |Some(M(old_sols,old_ext)) ->
      let new_sols = List.filter_map (
         fun sol -> 
          let new_sol = i_insert p sol in 
          if Point_with_breadth.subset_is_admissible pwb new_sol 
          then Some new_sol
          else None  
      ) old_sols in 
      if new_sols = []
      then let missing_sol = i_outsert p (Analysis_with_breadth.standard_solution pwb) in
           (None,Some(Pivot(p),Some(simpler_pwb,Some missing_sol)),true)
      else (Some(M(new_sols,i_insert p old_ext)),None,true);;    
      
exception Try_to_compute_in_select_case of (int * int * int) * point_with_breadth;;

let try_to_compute_in_select_case (i,j,k) pwb = 
  match Point_with_breadth.usual_decomposition_opt pwb with 
   None -> raise(Try_to_compute_in_select_case((i,j,k),pwb))
  |Some(simpler_pwb,_) ->
    match easy_compute_opt simpler_pwb with 
    None -> (None,Some(Select(i,j,k),Some(simpler_pwb,None)),true)
  |Some(M(old_sols,old_ext)) ->
    let new_sols = List.filter (Point_with_breadth.subset_is_admissible pwb) old_sols in 
    if new_sols = []
    then let missing_sol = Analysis_with_breadth.standard_solution pwb in
                 (None,Some(Select(i,j,k),Some(simpler_pwb,Some missing_sol)),true)
    else (Some(M(new_sols,old_ext)),None,true);;      
      
let try_to_compute_in_fork_case (i,j,k) pwb = 
  let offshoots = Image.image (fun t->
    let pwb2=Point_with_breadth.remove_element pwb t in 
    (pwb2,easy_compute_opt pwb2)
    ) [i;j;k] in
  match List.find_opt (fun (_,opt)->opt=None) offshoots with
  Some(pwb3,_)-> (None,Some(Fork(i,j,k),Some(pwb3,None)),true)
  |None ->
     let offshoots2 = Image.image (
       fun (_,opt)->Option.get opt
     ) offshoots in 
     let final_ext = i_fold_merge (Image.image (fun (M(_sol,ext))->ext) offshoots2)
     and M(sols,_) = List.nth offshoots2 2 in 
     (Some(M(sols,final_ext)),None,true);;   


exception Try_to_compute_exn of point_with_breadth;;

let try_to_compute_without_using_translations pwb =
  match  easy_compute_without_using_translations_opt pwb with 
  Some mold -> (Some(mold),None,false)
  |None -> 
    (
      match Analysis_with_breadth.explain pwb with 
       Discrete -> (* this should never happen, the discrete case
                      is already treated in *) 
                  raise(Try_to_compute_exn(pwb))
      |Pivot(p)->try_to_compute_in_pivot_case p pwb 
      |Select(i,j,k)->try_to_compute_in_select_case (i,j,k) pwb 
      |Fork(i,j,k)->try_to_compute_in_fork_case (i,j,k) pwb            
    ) ;; 

    
  ;;


  let try_to_compute pwb =
    let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
    let (opt_sol,extra_info,noticeable) = try_to_compute_without_using_translations translated_pwb in 
    match opt_sol with 
     None -> (None,extra_info)
    |Some(sol) -> 
        let final_sol = Mold.translate d sol in 
        let _ = (
         if noticeable then   
         low_level_ref:=(pwb,final_sol)::(!low_level_ref)) in 
        (Some(final_sol),None);;

  let walk_scale (w,scr,b) = 
     let base = Int_range.range 1 current_bound in 
      let temp1 = Image.image (fun n->
        let pwb = PWB(P(Finite_int_set.constructor n scr,w),b) in 
        (pwb,try_to_compute pwb)
        ) base in 
      List.find_opt (fun (_pwb,(sol_opt,_extra_info))->sol_opt=None) temp1;;  


  end ;;     

  let try_to_compute = Private.try_to_compute ;;
  let walk_scale = Private.walk_scale ;; 

  

end ;;  

