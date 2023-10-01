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

(* let point_order = ((fun (P(fis1,W w1)) (P(fis2,W w2)) ->
    let trial1 = i_order w1 w2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    fis_order fis1 fis2
  ): point Total_ordering_t.t);;
let point_insert = Ordered.insert point_order ;; *)

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

  let order = ((fun (P(fis1,W w1)) (P(fis2,W w2)) ->
    let trial1 = i_order w1 w2 in 
    if trial1<>Total_ordering_result_t.Equal then trial1 else 
    fis_order fis1 fis2
  ): point Total_ordering_t.t);;

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


    let order = ((fun (PWB(pt1,b1)) (PWB(pt2,b2)) ->
          let trial1 = Point.order pt1 pt2 in 
          if trial1<>Total_ordering_result_t.Equal then trial1 else 
          i_order b1 b2
        ): point_with_breadth Total_ordering_t.t);;    

    let subset_is_admissible (PWB(pt,b)) subset =
      if not(Point.subset_is_admissible pt subset)
      then false 
      else 
        let (P(_,W w)) = pt in 
        List.for_all (fun t->not(i_is_included_in [t;t+(w+1);t+2*(w+1)] subset)) (Int_range.range 1 b);;    


end ;;  

let breadth (PWB(_pt,b))= b ;;
let complementary_pairs = Private.complementary_pairs ;;
let decompose_wrt_translation pwb = 
  let (PWB(pt,_b)) = pwb in 
  let (d,_) = Point.decompose_wrt_translation pt in 
  (d,Private.translate (-d) pwb);; 

let everything_but_the_size (PWB(P(FIS(_n,scr),w),b)) = (w,scr,b) ;;  
let is_discrete pwb = (Private.usual_decomposition_opt pwb=None) ;; 
let max (PWB(pt,_b)) = Point.max pt ;;
let nonisolated_version = Private.nonisolated_version ;;
let order = Private.order ;; 
let projection pwb = snd(decompose_wrt_translation pwb);;
let to_extra_constraints = Private.to_extra_constraints ;; 
let remove_element = Private.remove_element ;;
let rightmost_largest_width = Private.rightmost_largest_width ;; 
let size (PWB(P(FIS(n,_scr),_w),_b)) = n ;;
let solutions = Private.solutions ;;  
let subset_is_admissible= Private.subset_is_admissible ;; 
let supporting_set (PWB(pt,_)) = Point.supporting_set pt ;;
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


let rightmost_overflow full_pwb (T old_data) = 
  let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
  and n = Point_with_breadth.max full_pwb in 
  let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
  let old_range = Image.image fst old_data in 
  let new_range = List.filter (fun i->(i_mem (i+1) old_range)) old_range in   
  let usual = Fan.impose_and_distribute (c_constraints,[n]) 
  and get = (fun i->List.assoc i old_data) in 
  T(Image.image (
    fun i->
       (i,Fan.union (usual(get (i+1))) (get(i))) 
  )  new_range) ;;     

let rightmost_pivot full_pwb (T old_data) = 
    let c_pairs = Point_with_breadth.complementary_pairs full_pwb 
    and n = Point_with_breadth.max full_pwb in 
    let c_constraints = Image.image (fun (i,j)->C[i;j]) c_pairs in  
    let old_range = Image.image fst old_data in 
    let new_range = List.filter (fun i->(i=0)||(i_mem (i-1) old_range)) old_range in   
    let usual = Fan.impose_and_distribute (c_constraints,[n]) 
    and get = (fun i->List.assoc i old_data) in 
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

let unregistered = Private.unregistered  ;;

let update_torsion
  ~preceding_point:(_prec_pwb:point_with_breadth) ~preceding_torsion:(_old_torsion:torsion) 
  ~current_point:(_pwb:point_with_breadth) (_cstr:constraint_t) (_update_is_a_selection:bool) = 
  Private.unregistered  ;;

end ;;   

     
module Medium_mold = struct 

  let add_isolated_set (MM(sols,ext,_torsion)) isolated_set =
      let add = i_merge isolated_set in 
      MM(Image.image add sols,add ext,Torsion.unregistered) ;; 

  let constructor_opt pwb sols ext torsion = 
    let (_,isolated_points) = Point_with_breadth.nonisolated_version pwb 
    and extra_links = Torsion.extra_links torsion in 
    if sols<>[]
    then Some(MM(sols,i_fold_merge [ext; isolated_points ; extra_links],torsion))
    else None ;; 

  let discrete domain = MM([domain],domain,Torsion.discrete domain) ;;  

  let fork_opt pwb (MM(_prec_sols,_prec_ext,prec_torsion)) (MM(left_sols,_left_ext,_left_torsion)) (i,j,k) = 
    constructor_opt pwb left_sols [] (Torsion.fork (i,j,k) prec_torsion) ;; 

  let rightmost_overflow_opt pwb (MM(sols,_ext,old_torsion)) = 
    constructor_opt pwb sols [] (Torsion.rightmost_overflow pwb old_torsion);; 

  let rightmost_pivot_opt pwb (MM(left_sols,left_ext,left_torsion)) = 
    let n = Point_with_breadth.max pwb in 
    let new_sols = List.filter_map (
                         fun sol -> 
                           let new_sol = i_insert n sol in 
                           if Point_with_breadth.subset_is_admissible pwb new_sol 
                           then Some new_sol
                           else None  
    ) left_sols in 
    constructor_opt pwb new_sols (i_insert n left_ext) (Torsion.rightmost_pivot pwb left_torsion);; 

  let select_opt pwb prec_mold (i,j,k) = 
    let (MM(prec_sols,prec_ext,prec_torsion)) = prec_mold in 
    let selected_sols = List.filter (Point_with_breadth.subset_is_admissible pwb) prec_sols in 
    constructor_opt pwb selected_sols prec_ext (Torsion.select (i,j,k) prec_torsion) ;; 
      
  let shallow sols = MM(sols,[],Torsion.unregistered ) ;;      

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

module Grocery = struct 

 module Low_level = struct 

  let handle_order = ((fun handle1 handle2 ->Total_ordering.standard handle1 handle2 
  ): handle Total_ordering_t.t);; 

  let mold_order = ((fun mold1 mold2 ->Total_ordering.standard mold1 mold2 
  ): mold Total_ordering_t.t);; 

  let hm_order = Total_ordering.product handle_order mold_order ;;
 
  let order = Total_ordering.product Point_with_breadth.order hm_order ;; 
 
  let rec get_opt key = function 
   [] -> None 
   | (key2,val2) :: others ->
      match Point_with_breadth.order key key2 with 
       Total_ordering_result_t.Lower -> None
      |Total_ordering_result_t.Greater -> get_opt key others  
      |Total_ordering_result_t.Equal -> Some val2 ;;  
    
  let insert new_key data = Ordered.insert order new_key data ;; 
  

 end ;; 

let empty_one = {
  helpers = [];
  pair_level = [];
  triple_level  = [];
  low_level = [];
}  ;;  

let add_to_low_level grc pwb pair = {
   grc with 
   low_level = Low_level.insert (pwb,pair) (grc.low_level);
} ;;

let add_to_low_level_if_nondiscrete grc pwb pair =
  if Point_with_breadth.is_discrete pwb 
  then grc
  else add_to_low_level grc pwb pair;;

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
      match Low_level.get_opt pwb (!grc_ref).low_level with 
      Some (answer) -> let (handle,mold) =answer in 
                       Some(handle,mold)    
    | None -> None
       ) ;;    


end ;;  


 

module Generic = struct 

module Impatient = struct 

  module Private = struct

 let immediate_opt grc pwb =  
    let (d,grounded_pwb) = Point_with_breadth.decompose_wrt_translation pwb in 
     match Grocery.immediate_eval_opt (ref grc) grounded_pwb with 
      None -> None
     |Some(handle,mold) -> Some(Handle.translate d handle,Medium_mold.translate d mold) ;; 
 

let fork_opt grc pwb =
  match Point_with_breadth.usual_decomposition_opt pwb with 
  None -> None
 |Some(prec_pwb,C cstr) ->
    match immediate_opt grc prec_pwb with 
    None -> None
  | Some(_,prec_mold) -> 
        let (MM(_sols,ext,_torsion)) = prec_mold in 
        let nth_cstr = (fun k->List.nth cstr (k-1)) in 
        let ijk=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
        let (i,j,k) = ijk in 
        if not(i_is_included_in [i;j;k] ext)
        then 
              let n = Point_with_breadth.max pwb in 
              let left_pwb = Point_with_breadth.remove_element pwb n in 
              (match immediate_opt grc left_pwb with 
                None -> None
              | Some(_,left_mold) -> 
                 (
                  match Medium_mold.fork_opt pwb prec_mold left_mold ijk with 
                     None -> None 
                    |Some mold -> Some(Fork(i,j,k),mold) 
                 )
              )
        else None;;   


           
let rightmost_overflow_opt grc pwb  = 
 let n = Point_with_breadth.max pwb in 
 let left_pwb = Point_with_breadth.remove_element pwb n in 
 match immediate_opt grc left_pwb with 
     None -> None
   | Some(_,left_mold) -> 
  let (MM(_left_sols,left_ext,_left_torsion)) = left_mold 
  and complements = Point_with_breadth.complementary_pairs pwb in  
  match List.find_opt  (
              fun (u,v) -> i_is_included_in [u;v] left_ext 
    ) complements with
    None -> None 
   |Some(u,v) ->
      (
        match Medium_mold.rightmost_overflow_opt pwb left_mold with 
           None -> None 
          |Some mold -> Some(Rightmost_overflow(u,v,n),mold) 
       )
    ;;

           

 let rightmost_pivot_opt grc pwb  = 
   let n = Point_with_breadth.max pwb in 
   let left_pwb = Point_with_breadth.remove_element pwb n in 
   match immediate_opt grc left_pwb with 
       None -> None
     | Some(_,left_mold) -> 
     (match Medium_mold.rightmost_pivot_opt pwb left_mold with 
       None -> None 
      |Some mold -> Some(Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),mold) 
     )    ;;  
 
let select_opt grc pwb =
  match Point_with_breadth.usual_decomposition_opt pwb with 
  None -> None
 |Some(prec_pwb,C cstr) ->
    match immediate_opt grc prec_pwb with 
    None -> None
  | Some(_,prec_mold) -> 
             let nth_cstr = (fun k->List.nth cstr (k-1)) in 
             let ijk=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
             let (i,j,k) = ijk in 
             (
                 match Medium_mold.select_opt pwb prec_mold ijk with 
                    None -> None 
                   |Some mold -> Some(Select(i,j,k),mold) 
             );;        

let eval_opt grc pwb =
  match immediate_opt grc pwb with 
  Some(answer0) -> Some answer0
 | None -> 
  if Point_with_breadth.is_discrete pwb 
  then Some(Discrete,Medium_mold.discrete(Point_with_breadth.supporting_set pwb))
  else    
  (match rightmost_pivot_opt grc pwb with 
   Some(answer1) -> Some answer1
  | None -> 
    (
      match rightmost_overflow_opt grc pwb with 
        Some(answer2) -> Some answer2
      | None -> 
        (
          match select_opt grc pwb with 
            Some(answer3) -> Some answer3
          | None -> fork_opt grc pwb
        )     
    )
   ) ;;
    
  let update_if_possible grc pwb =  
     match immediate_opt grc pwb with 
     Some pair1 -> (Some pair1,grc) 
     | None -> 
      (
        match eval_opt grc pwb with 
       Some pair2 -> (Some pair2,Grocery.add_to_low_level_if_nondiscrete grc pwb pair2) 
     | None -> (None,grc)
      ) ;;

  end ;;  

  let eval_opt = Private.eval_opt ;;
  let immediate_opt = Private.immediate_opt ;;
  let update_if_possible = Private.update_if_possible ;; 

end ;;  

module Painstaking = struct 

exception Push_exn ;; 

exception Should_never_happen_in_push_1_exn of point_with_breadth;; 

let pusher (grc,to_be_treated) = match to_be_treated with 
   [] -> raise Push_exn 
  | pwb :: others ->
  let (opt_pair1,grc1) = Impatient.update_if_possible grc pwb in 
  if opt_pair1<>None then (grc1,others) else 
  let (nonisolated_pwb,isolated_elts) = Point_with_breadth.nonisolated_version pwb in 
  if isolated_elts<>[]
  then let (opt_pair6,grc6) = Impatient.update_if_possible grc1 nonisolated_pwb in 
       if opt_pair6=None then (grc6,(Point_with_breadth.projection nonisolated_pwb)::to_be_treated) else 
        let (_handle,nonisolated_mold) = Option.get opt_pair6 in
        let mold = Medium_mold.add_isolated_set nonisolated_mold isolated_elts in 
       (Grocery.add_to_low_level grc6 pwb (Rightmost_pivot(W 0),mold),others) 
  else
  let opt2 = Point_with_breadth.usual_decomposition_opt pwb in 
  if opt2=None then raise(Should_never_happen_in_push_1_exn(pwb)) else
  let (_,C cstr) = Option.get opt2 in 
  let nth_cstr = (fun k->List.nth cstr (k-1)) in 
  let (i,j,k)=(nth_cstr 1,nth_cstr 2,nth_cstr 3) in 
  let pwb_i = Point_with_breadth.remove_element pwb i 
  and pwb_j = Point_with_breadth.remove_element pwb j 
  and pwb_k = Point_with_breadth.remove_element pwb k in 
  let (opt_pair3,grc3) = Impatient.update_if_possible grc1 pwb_i in 
  if opt_pair3=None then (grc3,(Point_with_breadth.projection pwb_i)::to_be_treated) else
  let (_,mold_i) = Option.get opt_pair3 in 
  let (opt_pair4,grc4) = Impatient.update_if_possible grc3 pwb_j in 
  if opt_pair4=None then (grc4,(Point_with_breadth.projection pwb_j)::to_be_treated) else
  let (_,mold_j) = Option.get opt_pair4 in 
  let (opt_pair5,grc5) = Impatient.update_if_possible grc4 pwb_k in 
  if opt_pair5=None then (grc5,(Point_with_breadth.projection pwb_k)::to_be_treated) else
  let (_,mold_k) = Option.get opt_pair5 in  
  let candidates = il_fold_merge(Image.image (fun (MM(sols,_,_))->sols) [mold_i;mold_j;mold_k]) in 
  let (_,final_sols) = Max.maximize_it_with_care List.length candidates in 
  let answer=(Fork(i,j,k),Medium_mold.shallow final_sols) in
  (Grocery.add_to_low_level grc5 pwb answer,others) ;;

let rec iterator (grc,to_be_treated) =
    if to_be_treated = [] 
    then grc
    else iterator(pusher (grc,to_be_treated)) ;;
    
let eval grc_ref pwb =
    let new_grc = iterator (!grc_ref,[pwb]) in 
    let _ = (grc_ref:=new_grc) in 
    Option.get(Impatient.immediate_opt new_grc pwb);;    


end ;;   

end ;; 

module Impatient = struct 

  module Private = struct
    let impatient_ref = ref Grocery.empty_one ;;
  end ;;

  let eval_opt = Generic.Impatient.eval_opt (!(Private.impatient_ref)) ;; 
  let immediate_opt = Generic.Impatient.immediate_opt (!(Private.impatient_ref)) ;; 
  let update_if_possible pwb =
     let (opt_answer,new_grc) = Generic.Impatient.update_if_possible (!(Private.impatient_ref)) pwb in 
     let _ = (Private.impatient_ref:=new_grc) in 
     opt_answer ;; 

end ;;

module Painstaking = struct 

  module Private = struct
    let painstaking_ref = ref Grocery.empty_one ;;
  end ;;

  let eval = Generic.Painstaking.eval Private.painstaking_ref ;; 
  let measure pwb = let (_,MM(sols,_,_)) = eval pwb in List.length(List.hd sols) ;; 

end ;;


module Extra_constraints = struct
  
  type paint_with_extra_constraints = PWEC of point * constraint_t list ;;
  
  module Private = struct

  let usual_decomposition_for_bare_point_opt_for_pwc pt =
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
      Some(PWEC(P(fis,W(effective_w-1)),selected_candidates), C l)
      ;;  
  
    let usual_decomposition_opt_for_pwc (PWEC(pt,l_cstr)) = 
        match l_cstr with 
      [] -> usual_decomposition_for_bare_point_opt_for_pwc pt 
    |highest :: others -> Some(PWEC(pt,others),highest) ;; 
  
  
  let pwc_is_discrete pwc = ((usual_decomposition_opt_for_pwc pwc)=None);;
  
  
  let remove_element_on_pwc (PWEC(pt,l_cstr)) t =
    let smaller_pt = Point.remove_element pt t in 
    PWEC(smaller_pt,List.filter (fun (C l)->not(i_mem t l)) l_cstr) ;; 
  
  let remove_rightmost_element_on_pwc pt_with_constraints =
    let (PWEC(pt,_)) = pt_with_constraints in 
    remove_element_on_pwc  pt_with_constraints (Point.max pt) ;; 
  
  let remove_rightmost_element_but_keep_constraints_on_pwc (PWEC(pt,l_cstr)) =
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
     PWEC(final_pt,final_constraints) ;;  
  
  
     let measure_for_pwc = Memoized.recursive (fun 
     old_f pwc-> 
       let (PWEC(pt,l_cstr)) = pwc in 
       let stays_admissible = (fun z->List.for_all (
          fun (C cstr)->not(i_is_included_in cstr z)
       ) l_cstr) in 
       let trial1 = Torsionfree_mold.solutions(Crude_analysis_on_bare_point.compute pt) in 
       if List.exists stays_admissible  trial1 
       then List.length(List.hd trial1)
       else 
       let pwc2 = remove_rightmost_element_on_pwc pwc
       and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
       max(old_f pwc2)((old_f pwc3)+1)
  );;
  
  let standard_solution_for_pwc  = Memoized.recursive (fun 
    old_f pwc-> 
    let (PWEC(pt,_l_cstr)) = pwc in 
    if pwc_is_discrete pwc 
    then Point.supporting_set pt 
    else  
    let pwc2 = remove_rightmost_element_on_pwc pwc
    and pwc3 = remove_rightmost_element_but_keep_constraints_on_pwc pwc in 
    if (measure_for_pwc pwc2)>=((measure_for_pwc pwc3)+1)
    then old_f(pwc2)
    else (old_f(pwc3))@[Point.max pt]  
  );;
  
  let pwb_to_extra_constraints (PWB(pt,b)) =
    if b = 0 then PWEC(pt,[]) else 
    let (W w)=Point.width pt 
    and domain = Point.supporting_set pt in 
    let all_constraints = Int_range.descending_scale 
       (fun k->C[k;k+(w+1);k+2*(w+1)]) b 1 in 
    let meaningful_constraints = List.filter(
      fun (C cstr) -> i_is_included_in cstr domain
    )  all_constraints in 
    PWEC(pt,meaningful_constraints) ;;


    end ;;


  let of_point_with_breadth = Private.pwb_to_extra_constraints ;;   
  let measure = Private.measure_for_pwc ;;  
  let remove = Private.remove_element_on_pwc ;; 
  let standard_solution = Private.standard_solution_for_pwc ;;  
  

  end ;;    

module Compute_Standard_solution = struct 

  let compute = Memoized.make(fun pwb->
    Extra_constraints.standard_solution
       (Extra_constraints.of_point_with_breadth pwb)
  )  ;;
  
end;;
  
module Decompose = struct 

  module Private = struct
  
  let test_for_individual_rightmost_overflow left_pwb m (u,v) = 
        List.for_all (fun t->Painstaking.measure(Point_with_breadth.remove_element left_pwb t)=m-1) [u;v] ;;
    
  let test_for_rightmost_overflow pwb m =
        let pairs =  Point_with_breadth.complementary_pairs pwb 
        and left_pwb = Point_with_breadth.remove_element pwb (Point_with_breadth.max pwb) in 
        List.find_opt (test_for_individual_rightmost_overflow left_pwb m) pairs ;; 
  
  
  let decompose = Memoized.make(fun pwb->
    match Point_with_breadth.usual_decomposition_opt pwb with 
        None -> (Discrete,PWB(P(FIS(0,[]),W 0),0))
        |Some(prec_pwb,C cstr) -> 
          let n = Point_with_breadth.max pwb in
          let left_pwb = Point_with_breadth.remove_element pwb n in 
          let pwc = Extra_constraints.of_point_with_breadth pwb in  
          if Extra_constraints.measure(Extra_constraints.remove pwc n)=
            Extra_constraints.measure(pwc)-1   
          then (Rightmost_pivot(Point_with_breadth.rightmost_largest_width pwb),left_pwb)
          else   
          let m = Painstaking.measure pwb in   
            ( match test_for_rightmost_overflow pwb m with 
           (Some(u,v))->(Rightmost_overflow(u,v,n),left_pwb)
           |None ->   
         
         let nth = (fun k->List.nth cstr (k-1)) in 
         if Painstaking.measure prec_pwb = m
         then 
              (Select(nth 1,nth 2,nth 3),prec_pwb)
         else (Fork(nth 1,nth 2,nth 3),prec_pwb)   
  ))  ;;     
  
  let rec iterator_for_chain (treated,pwb_to_be_treated) =
     let (handle,prec_pwb) = decompose pwb_to_be_treated in 
     let newly_treated = (handle,prec_pwb) :: treated in 
     if handle = Discrete 
     then newly_treated
     else  iterator_for_chain (newly_treated,prec_pwb) ;;   
  
       ;;
        
  
  
  end ;;
  
  
  let chain pwb = Private.iterator_for_chain ([],pwb) ;; 
  let decompose = Private.decompose ;; 
  
  end ;;

  module Diagnose = struct 

    type temporarily_new_diagnosis =
       Missing_forced_elements of (int list) * point_with_breadth 
      |Missing_solution of solution * point_with_breadth 
      |Missing_subcomputation_for_fork of point_with_breadth 
      |Missing_switch_in_fork of int * point_with_breadth ;;
      
    exception Nothing_to_diagnose_exn ;;
    exception Discrete_not_diagnosable_exn ;; 
    exception Missing_in_diagnose_rightmost_overflow_exn of point_with_breadth ;;
      

    
    module Private = struct
    
    let find_precedent pwb = 
       let temp1 = (Image.image snd (Decompose.chain pwb))@[pwb] in
        match List.find_map (fun pwb2->
           match  Impatient.update_if_possible pwb2 with 
           Some _ -> None 
           |None -> Some(pwb2)
        ) temp1   with 
        None -> raise(Nothing_to_diagnose_exn) 
        |Some precedent -> precedent ;; 
    
      let diagnose_rightmost_overflow (u,v,_n)  left_pwb = 
         match  Impatient.immediate_opt left_pwb with 
         None -> raise(Missing_in_diagnose_rightmost_overflow_exn(left_pwb))
         |Some (_,MM(_sols,ext,_torsion)) -> 
           Missing_forced_elements(i_setminus [u;v] ext,left_pwb) ;; 
    
     let diagnose_rightmost_pivot pwb left_pwb = 
        let the_sol = Compute_Standard_solution.compute pwb 
        and n = Point_with_breadth.max pwb in
        Missing_solution(i_outsert n the_sol,left_pwb) ;; 
    
      let diagnose_select pwb prec_pwb = 
          let the_sol = Compute_Standard_solution.compute pwb in
          Missing_solution(the_sol,prec_pwb) ;;  
    
      let diagnose_fork (i,j,k) pwb = 
        let the_sol = Compute_Standard_solution.compute pwb in 
        let l = List.find (fun t->not(i_mem t the_sol)) [k;j;i] in
        let shorter_pwb = Point_with_breadth.remove_element pwb l in 
        match  Impatient.immediate_opt shorter_pwb with 
         None -> Missing_subcomputation_for_fork(shorter_pwb)
         |Some (_,MM(sols,_ext,_torsion)) -> 
           if not(List.mem the_sol sols)
           then Missing_solution(the_sol,shorter_pwb)
           else Missing_switch_in_fork(l,pwb) ;;  
          
    
    let diagnose_precedent pwb =
      let (handle,pwb2) = Decompose.decompose pwb in 
        match handle with
         Discrete -> raise(Discrete_not_diagnosable_exn)
        |Rightmost_overflow(u,v,n) ->  diagnose_rightmost_overflow (u,v,n) pwb2
        |Rightmost_pivot(_) -> diagnose_rightmost_pivot pwb pwb2
        |Select (_,_,_) -> diagnose_select pwb pwb2 
        |Fork (i,j,k) -> diagnose_fork (i,j,k) pwb ;;  
    
    end ;;
    
    let diagnose pwb =
        let precedent = Private.find_precedent pwb in 
        (precedent,Private.diagnose_precedent precedent) ;; 
    
    end ;;