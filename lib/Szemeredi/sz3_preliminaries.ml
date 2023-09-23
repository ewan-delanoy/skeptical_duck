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

type fan = Sz3_types.fan = F of int list list ;; 

type torsion = Sz3_types.torsion = T of (int*fan) list ;;   

type torsionfree_mold = Sz3_types.torsionfree_mold = TFM of (solution list) * extension_data ;;  

type mold = Sz3_types.mold = MM of (solution list) * extension_data * torsion ;;    

type diagnosis  = Sz3_types.diagnosis  = 
  Missing_treatment of point_with_breadth 
 |Incomplete_treatment of point_with_breadth 
 |Missing_links of point_with_breadth * (int list)
 |Finished of handle * mold * bool ;; 

type grocery =  Sz3_types.grocery = {
  helpers : helper list;
  pair_level : ((width * int list) * (int -> int -> handle * mold)) list;
  triple_level : ((width * int list * int) * (int -> handle * mold)) list;
  low_level : (point_with_breadth * (handle * mold)) list;
} ;; 


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



module Torsionfree_mold = struct 

let add_links (TFM(sols,ext)) links = TFM(sols,i_merge links ext) ;;

let add_solution (TFM(sols,ext)) new_sol = TFM(il_insert new_sol sols,ext) ;;

let discrete domain = TFM([domain],domain) ;;   

let forced_elements (TFM(_sols, ext))= ext ;; 

let fork (TFM(_sols1,ext1),TFM(_sols2,ext2),TFM(sols3,ext3)) =
  let final_ext = i_fold_intersect [ext1;ext2;ext3] in 
  TFM(sols3,final_ext);;  

let of_solutions sols = TFM(sols,[]) ;; 

let constructor sols ext= TFM(sols,ext) ;;  

let measure (TFM(sols, _ext)) = List.length(List.hd sols) ;; 

let rightmost_overflow (TFM(sols,_ext)) = TFM(sols,[]) ;; 

let rightmost_pivot (TFM(_old_sols,ext)) n (new_sols:solution list) = TFM(new_sols,i_insert n ext) ;;

let select (TFM(_sols,ext)) new_sols = TFM(new_sols,ext);;

let solutions (TFM(sols, _ext))= sols ;; 

let solutions_and_forced_elements (TFM(sols, ext))= (sols,ext) ;;  

let translate d (TFM(sols, ext)) =
  let tr = (fun x->Image.image(fun t->t+d) x) in 
  TFM(Image.image tr sols,tr ext) ;; 

end ;;


module Find_highest_constraint = struct

  let rec for_exact_width (W w) domain to_be_treated =
    match to_be_treated with 
    [] -> None 
    |p::others ->
       if p<=2*w then None else 
       if i_is_included_in [p-2*w;p-w] domain 
       then Some (C[p-2*w;p-w;p])
       else for_exact_width (W w) domain others ;;     
  
  let rec below_maximal_width (W w) domain =
   match for_exact_width (W w) domain (List.rev domain) with 
   Some (cstr) -> Some(cstr)
   |None ->
      if w<2 then None else 
      below_maximal_width (W (w-1)) domain ;;  
  
  let below_width_bound_pair (W w,bound) domain =
    match List.find_opt(fun b->
      i_is_included_in [b;b+(w+1);b+2*(w+1)] domain
      ) (List.rev(Int_range.range 1 bound)) with 
    Some bmax ->  Some (C[bmax;bmax+(w+1);bmax+2*(w+1)])
    | None -> below_maximal_width (W w) domain ;; 

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

  let remove_element fis k=
    let (FIS(n,scrappers)) = fis in 
    if (k>n)||(k<1) then fis else 
    let new_scrappers = i_insert k scrappers in 
    if k <> n then FIS(n,new_scrappers) else 
    if scrappers = Int_range.range 1 (n-1)
    then empty_set
    else   
    let new_z =  Private.to_usual_int_list (FIS(n-1,scrappers)) in 
    let new_max = List.hd(List.rev new_z) in 
    FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;         

  (*
  
  remove_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_element (FIS(3,[])) 3 ;;
  remove_element (FIS(1,[])) 1 ;;
  remove_element (FIS(1,[])) 4 ;;

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
    Find_highest_constraint.below_maximal_width 
      (W w) (Finite_int_set.to_usual_int_list fis);;

  let is_discrete pt = (highest_constraint_opt(pt)=None) ;; 

  let is_nontrivial (P(fis,w)) =
    let domain = Finite_int_set.to_usual_int_list fis in
    ((Find_highest_constraint.below_maximal_width w domain) <> None);;

  let max (P(fis,_w)) = Finite_int_set.max fis  ;; 

  let remove_element (P(fis,w)) pivot = 
    let new_fis = Finite_int_set.remove_element fis pivot in 
    let new_w = (
      match Find_highest_constraint.below_maximal_width w (Finite_int_set.to_usual_int_list new_fis) with
      None -> 0
      |Some(C(l))->(List.nth l 1)-(List.nth l 0)
    ) in 
    P(new_fis,W new_w) ;;

  let remove_elements pt pivots = List.fold_left remove_element pt pivots ;;   

  let supporting_set (P(fis,_)) = Finite_int_set.to_usual_int_list fis ;; 

  let subset_is_admissible (P(_,w)) subset =
      ((Find_highest_constraint.below_maximal_width w subset) =None);;

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
    Torsionfree_mold.constructor 
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
    P_Finished_computation of (explanation option) * torsionfree_mold  
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

  let main_hashtbl =   ((Hashtbl.create 50) : (point, torsionfree_mold) Hashtbl.t) ;; 
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
          then  P_Finished_computation(None,Torsionfree_mold.discrete domain)
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
      Torsionfree_mold.translate d translated_mold);;

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
          then let pair = (pt,(Some(Lucky(pt2,ext2)),Torsionfree_mold.of_solutions [whole])) in  
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
          then let pair = (pt,(Some(Lucky(pt2,ext2)),Torsionfree_mold.of_solutions sols4)) in  
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
             Torsionfree_mold.of_solutions sols_for_preceding_point )) in  
            (pair::helper,None,to_be_treated) 
     |(pt2,ext2,goal)::other_hopes ->
       (
        match seek_translated_obvious_access helper pt2 with 
        P_Unfinished_computation(l) ->
           (helper,None,l@(pt::to_be_treated))
        |(P_Finished_computation(_,mold3)) -> 
          let sols3 = Torsionfree_mold.solutions mold3 in 
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
          let (sols,ext) = Torsionfree_mold.solutions_and_forced_elements mold in 
          let ext2 = i_insert n ext in 
          if not(Point.subset_is_admissible pt ext2) 
          then let pair = (pt,(Some(Early_stop(pt2,n)),Torsionfree_mold.of_solutions sols)) in 
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
               Torsionfree_mold.constructor sols2 (i_insert n ext))) in 
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
     let trial1 = Torsionfree_mold.solutions(Crude_analysis_on_bare_point.compute pt) in 
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

    let inclusion_test_for_non_isolation wmax b domain w candidate =
        if w=wmax+1
        then (i_is_included_in candidate domain) && (List.hd(candidate)<=b)
        else i_is_included_in candidate domain  ;;

   let atomic_test_for_non_isolation wmax b domain x w = 
       if w>wmax+1 then false else 
        let incl_test = inclusion_test_for_non_isolation wmax b domain w in 
       (incl_test [x-2*w;x-w;x])
       || 
       (incl_test [x-w;x;x+w]) 
       ||
       (incl_test [x;x+w;x+2*w]) ;; 


   let individual_test_for_non_isolation wmax b domain x=
       List.exists(atomic_test_for_non_isolation wmax b domain x) (Int_range.range 1 (wmax+1)) ;;

    let nonisolated_version (PWB(P(fis,W wmax),b)) = 
       let domain = Finite_int_set.to_usual_int_list fis in 
       let (non_isolated,isolated) = List.partition (individual_test_for_non_isolation wmax b domain) domain in 
       let new_fis = Finite_int_set.of_usual_int_list non_isolated in 
       (PWB(P(new_fis,W wmax),b),isolated);;
    
    let remove_element (PWB(P(fis,W wmax),b)) elt = 
      let new_fis = Finite_int_set.remove_element fis elt in 
      let new_domain = Finite_int_set.to_usual_int_list new_fis in 
      match Find_highest_constraint.below_width_bound_pair (W wmax,b) new_domain with
      None -> PWB(P(new_fis,W 0),0)
      |Some(C cstr)->
        let nth = (fun k->List.nth cstr (k-1)) in 
        let new_wmax = (nth 2)-(nth 1)-1 in 
        PWB(P(new_fis,W new_wmax),nth 1);;

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
let nonisolated_version = Private.nonisolated_version ;;
let to_extra_constraints = Private.to_extra_constraints ;; 
let remove_element = Private.remove_element ;;
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
  
  module Private = struct

  let distribute (F rays) addendum= F(Image.image (i_merge addendum) rays) ;;  

  end ;;  

  let constructor ll =
    let sorted_ll = il_sort ll in 
    F (Ordered_misc.minimal_elts_wrt_inclusion(sorted_ll));;

  let core (F ll) = i_fold_intersect ll ;; 

  let impose l_cstr (F rays) =  F(List.filter 
    (fun ray->List.for_all( fun (C cstr) ->not(i_is_included_in cstr ray)) l_cstr ) rays);;
  
  let impose_and_distribute  (l_cstr,addendum) fan = 
      Private.distribute ( impose l_cstr fan) addendum ;;

  let translate d (F rays) = F(Image.image (fun ray->Image.image (fun t->t+d) ray) rays);;

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
  

module Torsion = struct 

module Private = struct 

let unregistered = T [] ;;

end ;;   

let discrete _domain = Private.unregistered  ;;

let extra_links (T data) = 
  match data with 
   [] -> []
  |(idx1,fan1) :: _ ->
     if idx1 = 0 
     then Fan.core fan1
     else []  ;;

let fork (i,j,k) (T data) = 
  let c_constraints = [C[i;j;k]] in  
  T(List.filter_map (
        fun (i,old_indication)->
           if i=0 then None else
           Some(i-1,Fan.impose c_constraints old_indication) 
  )  data) ;;       

let rightmost_pivot left_pwb (T data) = 
    let c_pairs = Point_with_breadth.complementary_pairs left_pwb 
    and n = Point_with_breadth.max left_pwb in 
    let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
    let old_range = Image.image fst data in 
    let new_range = List.filter (fun i->(i=0)||(i_mem (i-1) old_range)) old_range in   
    let usual = Fan.impose_and_distribute (c_constraints,[n]) 
    and get = (fun i->List.assoc i data) in 
    T(Image.image (
      fun i->
         if i=0 then (i,usual(get 0)) else
         (i,Fan.union (usual(get i)) (get(i-1))) 
    )  new_range) ;;     

let select (i,j,k) (T data) = 
      let c_constraints = [C[i;j;k]] in  
      T(Image.image (
            fun (i,old_indication)->
               (i-1,Fan.impose c_constraints old_indication) 
      )  data) ;;    

let translate (d:int) (T data) = 
    (T (Image.image (fun (idx,fan)->(idx,Fan.translate d fan)) data)) ;;  

let update_torsion
  ~preceding_point:(_prec_pwb:point_with_breadth) ~preceding_torsion:(_old_torsion:torsion) 
  ~current_point:(_pwb:point_with_breadth) (_cstr:constraint_t) (_update_is_a_selection:bool) = 
  Private.unregistered  ;;

end ;;   

     
module Medium_mold = struct 


  let constructor pwb sols ext torsion = 
    let (_,isolated_points) = Point_with_breadth.nonisolated_version pwb 
    and extra_links = Torsion.extra_links torsion in 
    MM(sols,i_fold_merge [ext; isolated_points ; extra_links],torsion) ;; 

  let discrete domain = MM([domain],domain,Torsion.discrete domain) ;;  

  let to_torsionfree_mold (MM(sols,ext,_torsion)) = TFM(sols,ext) ;; 

  let torsion (MM(_sols, _ext,torsion)) = torsion ;; 

  let translate d (MM(sols, ext,torsion)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    MM(Image.image tr sols,tr ext,torsion) ;;  
    
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
  
  exception To_bare_answer_exn of diagnosis ;; 

  let to_bare_answer  diag = match diag with
    Missing_treatment(_) 
  | Incomplete_treatment (_) 
  | Missing_links(_,_) -> raise(To_bare_answer_exn(diag))
  | Finished(handle,mold,_) -> (handle,mold) ;;

  end ;;  

module Grocery = struct 

let empty_one = {
  helpers = [];
  pair_level = [];
  triple_level  = [];
  low_level = [];
}  ;;  

let immediate_eval_opt grc_ref pwb = 
  if Point_with_breadth.is_discrete pwb 
  then let domain = Point_with_breadth.supporting_set pwb in 
       Some(Discrete,Medium_mold.discrete domain) 
  else     
  let (PWB(P(FIS(n,scr),w),b)) = pwb in  
  let wpair = (w,scr) in
  match List.assoc_opt wpair (!grc_ref).pair_level with 
  Some (f) -> let (handle,mold) =f b n in 
              Some(handle,mold)    
| None ->
  let wtriple = (w,scr,b) 
  and n =  Point_with_breadth.size  pwb  in 
  match List.assoc_opt wtriple (!grc_ref).triple_level with 
    Some (f) -> let (handle,mold) =f n in 
                Some(handle,mold)    
  | None ->
     (  
      match List.assoc_opt pwb (!grc_ref).low_level with 
      Some (answer) -> let (handle,mold) =answer in 
                       Some(handle,mold)    
    | None -> None
       ) ;;    


end ;;  


module Store = struct 
  
  exception Select_case_opt_exn of point_with_breadth ;;
  exception Fork_case_opt_exn of point_with_breadth ;;
  exception Missing_links_in_store_exn of point_with_breadth * (int list);;
  
  exception With_outside_help_exn of point_with_breadth ;;
  exception Missing_prelude_to_link_exn of point_with_breadth * point_with_breadth ;;
  exception Impatient_eval_exn of point_with_breadth ;;

  module Private = struct

  let grocery_ref = ref (Grocery.empty_one);;  

  let helpers_ref = ref [
    (*
       Help_with_links(PWB(P(FIS(7,[]), W 1),3),[1;4]);
       Help_with_links(PWB(P(FIS(7,[2]), W 1),3),[4]);
    *)   
  ] ;; 


  
  type torsionfree_diagnosis   = 
    TFD_Missing_treatment of point_with_breadth 
   |TFD_Incomplete_treatment of point_with_breadth 
   |TFD_Missing_links of point_with_breadth * (int list)
   |TFD_Finished of handle * torsionfree_mold * bool ;; 

   module Torsionfree_diagnosis = struct 
 
    let is_unfinished  = function
    TFD_Missing_treatment(_) 
  | TFD_Incomplete_treatment (_) 
  | TFD_Missing_links(_,_) -> true
  | TFD_Finished(_,_,_) -> false ;;  
      
  end ;;  
  


    let translate_pair d (handle,mold) =
         (Handle.translate d handle,Torsionfree_mold.translate d mold);;


    let no_expansions_on_grounded_point_opt pwb = 
      Grocery.immediate_eval_opt grocery_ref pwb ;;
     
    
    let no_expansion_on_translatable_point pwb =
      let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
      match no_expansions_on_grounded_point_opt translated_pwb with
       None -> (None,Some translated_pwb)
      |Some(handle,mold) -> 
          let cr_mold = Medium_mold.to_torsionfree_mold mold in 
         (Some(translate_pair d (handle,cr_mold)),None);;    
                
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
    let prec_sols = Torsionfree_mold.solutions prec_mold in 
    let new_sols = List.filter (Point_with_breadth.subset_is_admissible pwb) prec_sols in 
    if new_sols = []
    then TFD_Incomplete_treatment(prec_pwb)
    else let nth = (fun k->List.nth cstr (k-1)) in  
         let i=nth 1 and j=nth 2 and k=nth 3 in 
         TFD_Finished(Select(i,j,k),Torsionfree_mold.select prec_mold new_sols,true);;   
    
  let explore_rightmost_overflow_possibility_on_grounded_point left_pwb left_mold left_ext (u,v,n) =
    let forgotten_links = i_setminus [u;v] left_ext in 
    if forgotten_links = []
    then TFD_Finished(Rightmost_overflow(u,v,n),Torsionfree_mold.rightmost_overflow left_mold,true)
    else 
    let (_opt_good,opt_bad) = no_expansion_on_translatable_removals left_pwb [u;v] in   
    match opt_bad with 
     None -> TFD_Missing_links(left_pwb,forgotten_links) 
     |Some pwb4 -> TFD_Missing_treatment pwb4  ;;            
              
              
  let explore_rightmost_pivot_possibility_on_grounded_point pwb left_pwb left_mold left_sols n = 
        let new_sols = List.filter_map (
                              fun sol -> 
                                let new_sol = i_insert n sol in 
                                if Point_with_breadth.subset_is_admissible pwb new_sol 
                                then Some new_sol
                                else None  
        ) left_sols in 
        if new_sols = []
        then TFD_Incomplete_treatment(left_pwb)
        else TFD_Finished(Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),
            Torsionfree_mold.rightmost_pivot left_mold n new_sols,true);;    
            
    exception Explore_fork_possibility_exn of point_with_breadth ;;
                
    let explore_fork_possibility_on_grounded_point prec_pwb cstr prec_ext = 
            let nth = (fun k->List.nth cstr (k-1)) in  
            let i=nth 1 and j=nth 2 and k=nth 3 in 
            let forgotten_links=i_setminus [i;j;k] prec_ext in 
            if forgotten_links<>[]
            then TFD_Missing_links(prec_pwb,forgotten_links) 
            else     
            let (opt_good,opt_bad) = no_expansion_on_translatable_removals prec_pwb [i;j;k] in 
            match opt_bad with
              Some(grounded_pwb2)-> TFD_Missing_treatment grounded_pwb2
            |None ->
              let mth = (fun k->snd(List.nth (Option.get opt_good) (k-1))) in  
              TFD_Finished(Fork (i,j,k),Torsionfree_mold.fork (mth 1,mth 2,mth 3),true);;               

    let rec finished_item_in_image_opt f pairs = 
       match pairs with 
       [] -> None 
       |pair::other_pairs ->
         let diag = f pair in 
         if Torsionfree_diagnosis.is_unfinished diag 
         then  finished_item_in_image_opt f other_pairs
         else Some diag
      ;;

    let minimal_expansions_opt_without_outside_help pwb prec_pwb prec_mold (C cstr)= 
      let trial1 =  explore_select_possibility_on_grounded_point pwb prec_pwb cstr prec_mold in 
      if not(Torsionfree_diagnosis.is_unfinished trial1)
      then trial1
      else
      let n = Point_with_breadth.max pwb in 
      let left_pwb = Point_with_breadth.remove_element pwb n in 
      let opt4 = no_expansions_on_grounded_point_opt left_pwb in 
      if opt4 = None
      then TFD_Missing_treatment(left_pwb)
      else 
      let (_,medium_left_mold) = Option.get opt4 in 
      let left_mold = Medium_mold.to_torsionfree_mold medium_left_mold in    
      let left_sols = Torsionfree_mold.solutions left_mold in 
      let trial2 = explore_rightmost_pivot_possibility_on_grounded_point pwb left_pwb left_mold left_sols n in 
      if not(Torsionfree_diagnosis.is_unfinished trial2)
      then trial2
      else
      let left_ext = Torsionfree_mold.forced_elements left_mold in   
      let complements = Point_with_breadth.complementary_pairs pwb in  
      let opt5 = finished_item_in_image_opt (
                fun (u,v) -> explore_rightmost_overflow_possibility_on_grounded_point left_pwb left_mold left_ext (u,v,n)
      ) complements in 
      if opt5<>None
      then Option.get opt5
      else 
      let prec_ext = Torsionfree_mold.forced_elements prec_mold in    
      explore_fork_possibility_on_grounded_point prec_pwb cstr prec_ext ;;
  
  let select_case_with_outside_help pwb prec_pwb cstr=
    match no_expansions_on_grounded_point_opt prec_pwb with 
     None -> TFD_Missing_treatment(prec_pwb)
    |Some(_,medium_prec_mold) -> 
      let prec_mold = Medium_mold.to_torsionfree_mold medium_prec_mold in 
       explore_select_possibility_on_grounded_point pwb prec_pwb cstr prec_mold;;    

  let rightmost_pivot_with_outside_help pwb = 
      let n = Point_with_breadth.max pwb in 
      let left_pwb = Point_with_breadth.remove_element pwb n in 
        match no_expansions_on_grounded_point_opt left_pwb with 
        None -> TFD_Missing_treatment(left_pwb)
      |Some(_,medium_left_mold) ->
        let left_mold = Medium_mold.to_torsionfree_mold medium_left_mold in 
        let left_sols = Torsionfree_mold.solutions left_mold in 
        explore_rightmost_pivot_possibility_on_grounded_point pwb left_pwb left_mold left_sols n;;    

   let rightmost_overflow_with_outside_help pwb (u,v,n) = 
      let left_pwb = Point_with_breadth.remove_element pwb n in 
      match no_expansions_on_grounded_point_opt left_pwb with 
        None -> TFD_Missing_treatment(left_pwb)
      |Some(_,medium_left_mold) -> 
        let left_mold = Medium_mold.to_torsionfree_mold medium_left_mold in 
        let left_ext = Torsionfree_mold.forced_elements left_mold in 
        explore_rightmost_overflow_possibility_on_grounded_point left_pwb left_mold left_ext (u,v,n) ;;    

  let fork_case_with_outside_help prec_pwb cstr = 
            match no_expansions_on_grounded_point_opt prec_pwb with 
             None -> TFD_Missing_treatment(prec_pwb)
            |Some(_,medium_prec_mold) -> 
              let prec_mold = Medium_mold.to_torsionfree_mold medium_prec_mold in 
              let prec_ext = Torsionfree_mold.forced_elements prec_mold in  
              explore_fork_possibility_on_grounded_point prec_pwb cstr prec_ext;;  


  let minimal_expansions_opt_with_outside_help pwb prec_pwb (C cstr) handle = 
    match handle with
    Discrete -> TFD_Finished(Discrete,Torsionfree_mold.discrete (Point_with_breadth.supporting_set pwb),false)
   |Select(_,_,_)->select_case_with_outside_help pwb prec_pwb cstr          
   |Rightmost_pivot(_)->rightmost_pivot_with_outside_help pwb
   |Rightmost_overflow(u,v,n)->rightmost_overflow_with_outside_help pwb (u,v,n)
   |Fork(_,_,_)->fork_case_with_outside_help prec_pwb cstr;;

  let minimal_expansions_opt pwb prec_pwb prec_mold (C cstr)~use_outside_help =
      if use_outside_help
      then minimal_expansions_opt_with_outside_help pwb prec_pwb (C cstr) (Analysis_with_breadth.handle pwb)
      else minimal_expansions_opt_without_outside_help pwb prec_pwb prec_mold (C cstr);;

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
         let m = Torsionfree_mold.measure(old_mold) -1 in 
         let counterexamples = List.filter (fun 
           (_t,md)->Torsionfree_mold.measure(md)<>m
         ) data_for_links in 
         if counterexamples<>[]
         then raise(False_links_exn(pwb,Image.image fst counterexamples)) 
         else
         let links = Image.image fst data_for_links in 
         Torsionfree_mold.add_links old_mold links;;
        
        let with_solution  pwb old_mold new_sol  =
         let m = Torsionfree_mold.measure(old_mold) in 
         if List.length(new_sol)<>m
         then raise(False_solution_exn(pwb,new_sol)) 
         else Torsionfree_mold.add_solution old_mold  new_sol;;
      
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

         let apply_current_helplist_on_diagnosis pwb diag =
             match diag with 
              TFD_Missing_treatment(_) |TFD_Incomplete_treatment (_) |TFD_Missing_links (_,_) -> diag
             |TFD_Finished(handler,mold,is_new) -> 
              TFD_Finished(handler,apply_current_helplist_to_mold pwb mold,is_new) ;;

      end ;;   
        
  let compute_on_grounded_point_opt pwb ~use_outside_help = 
    match no_expansions_on_grounded_point_opt pwb with  
    Some(handle,mold) -> Finished(handle,mold,false)
   |None ->
    (match Point_with_breadth.usual_decomposition_opt pwb with 
      None -> let domain = Point_with_breadth.supporting_set pwb in 
              Finished(Discrete,Medium_mold.discrete domain,false)
      |Some(prec_pwb,C cstr) -> 
        (
          match no_expansions_on_grounded_point_opt prec_pwb with  
             None -> Missing_treatment(prec_pwb)
            |Some(_handle,medium_prec_mold) -> 
              let prec_mold = Medium_mold.to_torsionfree_mold medium_prec_mold in 
              match Help.apply_current_helplist_on_diagnosis pwb (minimal_expansions_opt pwb prec_pwb prec_mold (C cstr) ~use_outside_help) with 
              TFD_Missing_treatment(pwb2) ->  Missing_treatment(pwb2)
              |TFD_Incomplete_treatment (pwb2) ->  Incomplete_treatment (pwb2)
              |TFD_Missing_links (pwb2,links) -> Missing_links (pwb2,links)
              |TFD_Finished(handler,old_mold,is_new) -> 
                  let (sols,ext) = Torsionfree_mold.solutions_and_forced_elements old_mold 
                  and old_torsion = Medium_mold.torsion medium_prec_mold in 
                  let m1 = Torsionfree_mold.measure (Medium_mold.to_torsionfree_mold medium_prec_mold)
                  and m2 = Torsionfree_mold.measure old_mold in 
                  let update_is_a_selection = (m1=m2) in 
                  let new_torsion = Torsion.update_torsion 
                  ~preceding_point:prec_pwb
                    ~preceding_torsion:old_torsion ~current_point:pwb (C cstr) update_is_a_selection in 
                  Finished(handler,Medium_mold.constructor pwb sols ext new_torsion,is_new)        
        ) );; 

  let compute_opt pwb ~use_outside_help=
    let (d,translated_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
    Diagnosis.translate d (compute_on_grounded_point_opt translated_pwb ~use_outside_help);;    
  

   let unsafe_low_level_add pwb mold = 
      let old_grocery = (!grocery_ref) in 
      let new_grocery = {
          old_grocery with 
          low_level=(pwb,mold)::(old_grocery.low_level)
      } in 
      grocery_ref:=new_grocery ;;
    
    let unsafe_pair_level_add pair f = 
        let old_grocery = (!grocery_ref) in 
        let new_grocery = {
            old_grocery with 
            pair_level=(pair,f)::(old_grocery.pair_level)
        } in 
        grocery_ref:=new_grocery ;;   

     let unsafe_triple_level_add triple f = 
          let old_grocery = (!grocery_ref) in 
          let new_grocery = {
              old_grocery with 
              triple_level=(triple,f)::(old_grocery.triple_level)
          } in 
          grocery_ref:=new_grocery ;;     

   let reset_all () = grocery_ref := Grocery.empty_one ;;
   
   let set_low_level v = 
    let old_grocery = (!grocery_ref) in 
      let new_grocery = {
          old_grocery with 
          low_level=v
      } in 
      grocery_ref:=new_grocery ;;

   let reset_low_level () = set_low_level [] ;; 
    



  let impatient_eval pwb =
     match compute_opt pwb ~use_outside_help:false with 
     Finished(_,mold,_)-> mold 
    |Missing_treatment(_)
    |Incomplete_treatment(_)
    | Missing_links(_,_) -> raise(Impatient_eval_exn(pwb));;

  end ;;     
  
   
  let experimental_eval pwb = Private.compute_opt pwb ~use_outside_help:true ;;
  let get_low_level () = (!(Private.grocery_ref)).low_level ;; 
  let impatient_eval = Private.impatient_eval ;;
  let reset_low_level = Private.reset_low_level ;;
  let set_low_level v = Private.set_low_level v;; 
  let unsafe_low_level_add = Private.unsafe_low_level_add ;; 
  let unsafe_pair_level_add = Private.unsafe_pair_level_add ;; 
  let unsafe_triple_level_add = Private.unsafe_triple_level_add ;; 

  end ;;  
 

module Medium_analysis = struct 

  exception Walk_scale_exn of int * diagnosis ;; 
  exception Nonstandard_handler_exn of point_with_breadth * handle * mold ;;
    
    module Private = struct
    
      let try_to_compute_on_grounded_point pwb = 
        let diag = Store.experimental_eval pwb  in 
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

      let walk_inner_web n (w,scr) =
        Image.image (fun k->walk_scale (w,scr,k) n) (Int_range.range 0 n);;

      let walk_web n data =
         let data2 = List.flatten(Image.image (fun (width,l)->Image.image(fun scr->(width,scr)) l) data) in 
         let _ = Image.image (walk_inner_web n) data2 in 
         () ;; 
    
      end ;;     
    
      let force_compute pwb =Diagnosis.to_bare_answer(Private.try_to_compute pwb);;
      let try_to_compute = Private.try_to_compute ;;
      let walk_scale = Private.walk_scale ;; 
      let walk_web = Private.walk_web ;; 

    end ;;  
    
      
module Safe_initialization = struct 

exception Undefined_during_check_exn of point_with_breadth * diagnosis ;;   
exception Unequal_during_check_exn of (point_with_breadth * (handle * mold) * (handle * mold)) ;;


module Private = struct 

   let max_size = 25 ;;
   let max_breadth = 25 ;; 
   let counterexamples_ref = ref [];;

  let check_for_triple (w,scr,b) f must_return_to_old_state= 
    let old_state = Store.get_low_level () in 
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
       then Store.set_low_level old_state
     ) in 
     let counterexamples = (!counterexamples_ref) in 
     if counterexamples <> []
     then raise(Unequal_during_check_exn(List.hd counterexamples))
     else () ;;
      
   let check_for_pair (w,scr) g = 
    let old_state = Store.get_low_level () in 
    (
    for b= 0 to max_breadth do check_for_triple (w,scr,b) (g b) false done;
    Store.set_low_level old_state
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

module More = struct 

type 'a small_advance =
    SA_Missing_treatment of point_with_breadth
  | SA_Incomplete_treatment of point_with_breadth
  | SA_Missing_links of point_with_breadth * int list
  | SA_Finished of 'a ;;   
  

  
end ;;   
