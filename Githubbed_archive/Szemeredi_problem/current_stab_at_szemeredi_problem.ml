(*

#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)

open Needed_values ;; 
open Sz_types ;; 


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

let cil_order = ((fun (C x) (C y)->il_order x y) : constraint_t Total_ordering_t.t) ;;

let concretize (n,scrappers) = i_setminus (Int_range.range 1 n) scrappers ;; 

let abstractize domain =
   if domain = [] then (0,[]) else 
   let n = List.hd(List.rev domain) in 
   (n,i_setminus (Int_range.range 1 n) domain) ;;   

module Parameter_pair_for_obstruction = struct 

let predecessor max_in_set (width,breadth) = 
  if breadth < 1 
  then (if width < 2 then None else Some(width-1,max_in_set-2*(width-1)) )  
  else (Some(width,breadth-1)) ;;
  
let check_for_meaningful_obstruction (width,breadth) domain =
   if breadth < 1 
   then false 
   else i_is_included_in [breadth;breadth+width;breadth+2*width] domain ;;  

end ;;  

let test_for_admissibility_up_to_max_with max_width z =
  if max_width<1 then true else 
  Sz_preliminaries.test_for_admissibility (Sz_max_width_t.MW (max_width)) z ;;

let test_for_admissiblity width breadth z =
   (test_for_admissibility_up_to_max_with (width-1) z)
   &&
   (List.for_all (fun t->
    not(i_is_included_in [t;t+width;t+2*width] z)) (Int_range.range 1 breadth))  ;;

let remove_one_element (n,scrappers) k=
  let new_scrappers = i_insert k scrappers in 
  if k <> n then (n,new_scrappers) else 
  let new_z =  concretize (n,new_scrappers) in 
  let new_max = List.hd(List.rev new_z) in 
  (new_max,List.filter (fun t->t<new_max) new_scrappers) ;;


(*

remove_one_element (10,[3;7;8;9]) 10 ;;

*)



module Constraint = struct

let extra_constraints_from_boundary_increment width breadth n =
    let mainstream = Int_range.scale (fun j->
        let k = width-j in C[n-2*k;n-k]
     ) 1 (width-1) in
     let lower_end = n-2*width in 
     if (lower_end>=1) && (lower_end<=breadth) 
     then (C[lower_end;lower_end+width])::mainstream 
     else mainstream ;;   

let satisfied_by_individual l_constr l =
  List.for_all (fun (C constr)->not(i_is_included_in constr l)) l_constr

let satisfied_by_all_in_list l_constr ll=
  List.for_all (satisfied_by_individual l_constr) ll ;;

let merge_constraints l_constr1 l_constr2 = 
    let simplifier = Image.image (fun (C x)->x) in
    Image.image (fun x->C x)
    (Ordered_misc.minimal_elts_wrt_inclusion (il_merge 
     (simplifier l_constr1) (simplifier l_constr2))) ;;

let insert_new (n,scrappers) (old_constraints,extension) (C new_constraint)= 
  let whole = concretize (n,scrappers) in 
  let remaining_constraint = i_setminus new_constraint extension in 
  if remaining_constraint = [] 
  then None 
  else 
  if (i_setminus remaining_constraint whole)<>[] 
  then Some (old_constraints)    
  else Some (merge_constraints [C remaining_constraint] old_constraints) ;;  
   
let insert_several  (n,scrappers) (old_constraints,extension) new_constraints =
   let rec tempf = (
      fun (constraints_walker,to_be_treated) ->
         match to_be_treated with 
         [] -> Some constraints_walker 
         | new_constraint :: others ->  
        (match  insert_new (n,scrappers) (constraints_walker,extension) new_constraint with    
           None -> None 
          | Some new_walker -> tempf(new_walker,others) 
        )
   ) in 
   tempf(old_constraints,new_constraints);;
   


end ;;  


module Point = struct 
    
  let width (P(w,b,n,s)) = w ;;
  let breadth (P(w,b,n,s)) = b ;;
  let size (P(w,b,n,s)) = n ;;
  let scrappers (P(w,b,n,s)) = s ;;
  let unveil (P(w,b,n,s)) = (w,b,n,s) ;;
  let concretize (P(w,b,n,s)) = concretize (n,s) ;;

end ;;  


module Qualified_point = struct 

let extend_with qp extension =  
  match qp with 
  Q(pt,old_constraints,extension2) -> 
  Q(pt,old_constraints,i_merge extension extension2)   ;;

let append_right qp extension =  
    match qp with 
    Q(pt,old_constraints,extension2) -> 
    Q(pt,old_constraints,extension2@extension)   ;;  

let insert_several_constraints new_constraints (Q(pt,old_constraints,extension)) =
  let n = Point.size pt and scrappers = Point.scrappers pt in 
  match Constraint.insert_several (n,scrappers) (old_constraints,extension) new_constraints 
  with
    None -> None 
   |(Some final_constraints) ->  Some((Q(pt,final_constraints,extension))) ;; 

let compute_full_replacement 
   (Q(pt,constraints,extension)) list_of_solutions = 
     let temp1 = List.filter (Constraint.satisfied_by_individual constraints) list_of_solutions in 
     Image.image (i_merge extension) temp1 ;;

let compute_full_replacement_for_list (qp,list_of_solutions) qp_list =
  let (tester,others) = List.partition (fun qp2->qp2=qp) qp_list in 
  if tester = [] 
  then None 
  else Some(compute_full_replacement qp list_of_solutions,others) ;;             


end ;;  

module Mold = struct 

let common_length (M(reps,qpoints)) =
    List.length(List.hd reps);;

(* it is assumed that compatibility has already been checked *)   
let extend_with (M(reps,qpoints)) extension =
  M(Image.image (i_merge extension) reps,
  Image.image (fun qpoint->Qualified_point.extend_with qpoint extension) qpoints
  ) ;;  

let append_right (M(reps,qpoints)) extension =
    M(Image.image (fun x->x@extension) reps,
    Image.image (fun qpoint->Qualified_point.append_right qpoint extension) qpoints
    ) ;;  
  

let insert_several_constraints extra_constraints (M(reps,qpoints)) = 
  M(List.filter (Constraint.satisfied_by_individual extra_constraints) reps,
     Option.filter_and_unpack (
      Qualified_point.insert_several_constraints extra_constraints
     ) qpoints) ;; 
  
let compute_full_replacement (qp,list_of_solutions) mold =
  let (M(reps,qpoints)) = mold in 
  match Qualified_point.compute_full_replacement_for_list  (qp,list_of_solutions) qpoints with
  None -> ([],mold) 
  |Some(new_reps,remaining_qpoints) -> (new_reps,M(il_merge reps new_reps,remaining_qpoints)) ;;   

let insert_several_constraints_opt extra_constraints mold = 
   let new_mold = insert_several_constraints extra_constraints mold in 
   if new_mold = M([],[])
   then None 
   else Some new_mold ;;   

let apply_passive_repeat  pt mold =
   let (width,b,_,_) = Point.unveil pt in 
   insert_several_constraints_opt [C[b;b+width;b+2*width]] mold;;

let apply_boundary_increment pt mold = 
    let (width,breadth,n,_) = Point.unveil pt in 
    let new_constraints = Constraint.extra_constraints_from_boundary_increment width breadth n in 
    match insert_several_constraints_opt new_constraints mold with 
     None -> None 
    |Some new_mold -> Some(extend_with new_mold [n]) ;;

let apply_fork ll =
  let (_,temp1) = Max.maximize_it_with_care 
    (fun (_,mold)->common_length mold)  ll in  
  let chosen_reps = (fun (_,M(reps,_))->reps) (List.hd(List.rev(temp1))) in 
  (*
   for the second component, we deliberately do not expand,
   to keep the stored expressions small   
  *)
  M(chosen_reps,Image.image fst temp1) ;; 

let apply_hook_naively pt hook args =  
    match hook with 
    Passive_repeat -> apply_passive_repeat pt (snd(List.hd args))
  | Boundary_increment -> apply_boundary_increment pt (snd(List.hd args))
  | Fork ->   Some(apply_fork args )
  | Jump -> Some(snd(List.hd args));; 


exception Apply_hook_exn of ((qualified_point* mold) list) * hook * point ;;
  
let apply_hook pt hook args  = 
  match apply_hook_naively pt hook args  with 
    None -> None 
  |Some mold ->
    let (M(reps,_)) = mold in 
    if reps = []
    then raise (Apply_hook_exn(args,hook,pt)) 
    else Some mold ;;

let singleton z = M([z],[])  ;;


end ;;


module Partial_result = struct 


  


end ;;  

module Ancestry_info = struct 

let extend_with (AI l) extension =
   AI(
     Image.image (fun (pt,ext1)->
        (pt,i_merge ext1 extension)
      ) l
   ) ;;
    
let append_right (AI l) extension =
    AI(
      Image.image (fun (pt,ext1)->
         (pt,ext1@extension)
       ) l
    ) ;;


end ;;   


module Bulgarian_for_nonparametrized_sets = struct 

  module Private = struct 
  
  let rec iterator_for_meaningful_obstruction (domain,max_in_domain,w,b) =
    if Parameter_pair_for_obstruction.check_for_meaningful_obstruction (w,b) domain 
    then Some(w,b)
    else
    match Parameter_pair_for_obstruction.predecessor max_in_domain (w,b) with 
     None -> None  
     |Some(new_w,new_b) -> iterator_for_meaningful_obstruction (domain,max_in_domain,new_w,new_b) ;;
  
  let find_meaningful_obstruction (w,b) domain = 
     if domain = [] then None else 
     let max_in_domain = List.hd(List.rev domain) in 
     iterator_for_meaningful_obstruction (domain,max_in_domain,w,b) ;; 
  
  let inner_test_for_detachability width breadth domain x w = 
      if not(i_is_included_in [x-2*w;x-w] domain)
      then true
      else if w<width 
           then false
           else breadth < (x-2*w)  ;;   
  
  let test_for_detachability width breadth domain x = 
    let idx_range = Int_range.range 1 (min (width)((x-1)/2))  in 
      List.for_all (inner_test_for_detachability width breadth domain x) idx_range ;;
            
  let rec iterator_for_detachment (width,breadth,domain,treated,to_be_treated) = 
      match to_be_treated with 
       [] -> ([],treated)
      | x :: others -> 
         if test_for_detachability width breadth domain x 
         then iterator_for_detachment (width,breadth,domain,x::treated,others)
         else (List.rev to_be_treated,treated);;    
  
  let detach (width,breadth) domain = iterator_for_detachment (width,breadth,domain,[],List.rev domain) ;;
  
  end ;;
  
  let decompose (old_width,old_breadth) domain = 
    if (old_width,old_breadth)=(1,0) then None else 
    match Private.find_meaningful_obstruction (old_width,old_breadth) domain with 
      None -> None
      | Some (width,breadth) -> Some((width,breadth),Private.detach (width,breadth) domain);;  
  
  
  end ;;  
  
  module Bulgarian = struct 
  
    let decompose pt =
        let (old_width,old_breadth,n,scrappers) = Point.unveil pt in 
        let domain = concretize (n,scrappers) in 
        match Bulgarian_for_nonparametrized_sets.decompose (old_width,old_breadth) domain with
        None -> None
      | (Some((new_width,new_breadth),(new_domain,adjustment))) -> 
         let (new_n,new_scrappers) = abstractize new_domain in 
          Some(P(new_width,new_breadth,new_n,new_scrappers),adjustment);;
      
  (*
     
  let check1 = (decompose (P(1,4,6,[])) =  (P (1, 4, 6, []), [])) ;;
  let check2 = (decompose (P(1,3,6,[])) =  (P (1, 3, 5, []), [6])) ;;
  
  *)
  
  end ;;   
  
  let nonbulgarian_ancestors_for_hook pt hook = 
    let (width,breadth,n,scrappers) = Point.unveil pt in  
    match hook with 
    Passive_repeat -> [P(width,breadth-1,n,scrappers)]     
   | Boundary_increment ->
    let (m,new_scrappers) = remove_one_element (n,scrappers) n in  
    [P(width,breadth,m,new_scrappers)]
   | Fork ->     
       Int_range.scale (fun k->
          let (m,scr) = remove_one_element  (n,scrappers)  (breadth+k*width) in 
          P(width,breadth-1,m,scr)
        ) 0 2 
   | Jump -> [P(width-1,n-2*(width-1),n,scrappers)] ;;

exception Already_decomposable_point of point ;; 

let ancestors_for_hook pt0 hook = 
  match Bulgarian.decompose pt0 with 
  None -> raise(Already_decomposable_point(pt0))
  |Some(pt1,adj1) ->
  let temp1 = nonbulgarian_ancestors_for_hook pt1 hook in 
  (AI(Image.image (fun pt2-> 
    match Bulgarian.decompose pt2 with 
     None -> (pt2,adj1)
    |Some(pt3,adj3) -> (pt3,i_merge adj1 adj3)
  ) temp1))  ;; 

module Bulk_result = struct 

    let common_length (BR(_,pres)) = Mold.common_length pres ;;
    let partial (BR(_,pres)) = pres ;;  
    let make ancestor_info pres = BR(ancestor_info,pres) ;;
    let singleton ancestry z = BR(ancestry,Mold.singleton z)  ;;
    
    let extend_with (BR(ancestry_opt,pres)) extension = 
       let new_ancestry_opt = (
          match ancestry_opt with 
            None -> None 
           |Some(hook,anc_info) -> Some(hook,Ancestry_info.extend_with anc_info extension)
       ) in 
      BR(
        new_ancestry_opt,
        Mold.extend_with pres extension
      );;
    
    let append_right (BR(ancestry_opt,pres)) extension = 
        let new_ancestry_opt = (
           match ancestry_opt with 
             None -> None 
            |Some(hook,anc_info) -> Some(hook,Ancestry_info.append_right anc_info extension)
        ) in 
       BR(
         new_ancestry_opt,
         Mold.append_right pres extension
       );;
       
    
    let extend_with_opt bres_opt extension = match bres_opt with 
      None -> None 
      |Some bres -> Some (extend_with bres extension) ;;     
    
    let apply_hook pt hook args  = 
       let partial_args = Image.image (fun (qp,bres)->(qp,partial bres)) args in  
       match Mold.apply_hook  pt hook partial_args  with 
        None -> None 
       |Some new_pres ->
            let anc_info = Some(hook,ancestors_for_hook pt hook) in  
            Some(BR(anc_info,new_pres));;
      
    
    end ;;  
    
    
    module Parametrized = struct 
      
    
      let eval_fos fos n =
         match fos with 
            Usual_fos f -> f n ;; 
      
      let eval_fobas fobas breadth n = 
        match fobas with 
         Usual_fobas f -> f breadth n ;;  
    
       
    
    end ;;   
    
    
module Parametrized_Example = struct 
    
let sf1 n = List.filter (fun t->List.mem(t mod 3)[1;2]) (Int_range.range 1 n) ;;
  
(*
let check_sf1 = 
  let temp1 = Int_range.scale (fun n->
   let (BR(opt,PR(reps,_))) = force_compute (P(1,n-2,n,[])) in 
   (n,reps,[Parametrized_Example.sf1 n])) 1 40 in 
  List.filter (fun (n,a,b)->a<>b) temp1 ;; 
*)  

let vp1 = P (1, 0, 3, [1]) ;; 
let vp2 = P (1, 0, 3, [2]) ;; 

let pf1 x = P (1, x-2, x, []) ;; 

let aif1 n =
   match List.assoc_opt n [1,None;2,None;
    3, Some
    (Fork,
     AI
      [(vp1, []); (vp2, []); (pf1 2, [])])
   ] with 
   Some answer -> answer 
   | None ->
   (match n mod 3 with 
   0 -> Some(Fork,AI[(pf1(n-3), [n-1; n]); (pf1(n-2), [n]); (pf1(n-1), [])])
  |1|2 ->Some(Passive_repeat,AI[(pf1(n-1), [n])])
  |_ ->failwith("impossible remainder by 3"));;
    
(*
   
let check_aif1 = 
   let temp1 = Int_range.scale (fun n->
    let (BR(opt,_)) = force_compute (P(1,n-2,n,[])) in 
    (n,opt,Parametrized_Example.aif1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 

*)

let vqp1 = Q (vp1, [], []) ;;
let vqp2 = Q (vp2, [], []) ;;
let vqp3 = Q (vp2, [], [4]) ;;

let qpf1 n = Q (pf1 (n-3), [], [n-1; n]) ;;
let qpf2 n = Q (pf1 (n-2), [], [n]) ;;
let qpf3 n = Q (pf1 (n-1), [], []) ;;

let fdf1 n =
  match List.assoc_opt n [
    1,M([[1]],[]);3,M([sf1(3)],[vqp1; vqp2;qpf3(3)]);
    4,M([sf1(4)],[vqp3; qpf2(4)])
  ]  with Some answer -> answer | None ->
  (match (n mod 3) with 
    0 -> M([sf1(n)],[qpf1(n); qpf2(n)  ;qpf3(n)]) 
  | 1 -> M([sf1(n)],[qpf1(n); qpf2(n)])
  | 2 -> M([sf1(n)],[])
  |_ ->failwith("impossible remainder by 3")) ;;  


(*
let check_fdf1 = 
   let temp1 = Int_range.scale (fun n->
    let (BR(opt,PR(reps,mold))) = force_compute (P(1,n-2,n,[])) in 
    (n,mold,Parametrized_Example.osf2 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 
*)    

let bresf1 n = BR(aif1(n),fdf1(n)) ;;
  
(*
let check_bresf1 = 
   let temp1 = Int_range.scale (fun n->
    let bres = force_compute (P(2,0,n,[])) in 
    (n,bres,Parametrized_Example.bresf1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 
*)    

let bresf2 breadth n = 
  if breadth = 0 then Bulk_result.singleton None (Int_range.range 1 n) else
  if n <= breadth + 2 then bresf1 n else 
  Bulk_result.append_right (bresf1(breadth+2)) (Int_range.range (breadth+3) n)  ;;

(*  
let check_bresf2 =
    let temp1 = Cartesian.product (Int_range.range 0 30) (Int_range.range 1 30) in 
    let temp2 = Image.image (
      fun (b,n) ->((b,n),force_compute (P(1,b,n,[])),Parametrized_Example.bresf2 b n)
    ) temp1 in
    List.filter (fun (n,a,b)->a<>b) temp2 ;;
*)

end ;;   
      




module Accumulator_with_optional_anticipator = struct 

let low_hashtbl = Hashtbl.create 50 ;;
let low_anticipator = ref [] ;; 
    
  
  
let get_from_low_hashtbl ~with_anticipation pt =
      if not(with_anticipation)
      then  Hashtbl.find_opt low_hashtbl pt 
      else
          match List.assoc_opt pt (!low_anticipator) with 
          Some anticiped_answer -> Some anticiped_answer 
          | None -> Hashtbl.find_opt low_hashtbl pt  ;;
  
let add_to_low_hashtbl  ~with_anticipation pt vaal=
    if not(with_anticipation)
    then   Hashtbl.replace low_hashtbl pt vaal
    else low_anticipator := (pt,vaal) :: (!low_anticipator)  ;;
  
  
end ;;   
  


let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;

let generic_access_opt  ~with_anticipation pt = 
 match Bulgarian.decompose pt with 
 None -> Some (Bulk_result.singleton None (Point.concretize pt))
 | Some(pt2,adj) ->
let (width,breadth,n,scrappers) = Point.unveil pt2 in 
let pre_res=(
match Hashtbl.find_opt rose_hashtbl (width,scrappers) with 
Some summary -> Some (Parametrized.eval_fobas summary breadth n)
| None ->  
 (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.eval_fos summary n)
 | None -> Accumulator_with_optional_anticipator.get_from_low_hashtbl ~with_anticipation pt2) 
) in 
Bulk_result.extend_with_opt pre_res adj ;;
;;   

let generic_access ~with_anticipation pt = 
   Option.unpack(generic_access_opt ~with_anticipation pt) ;;   

exception Bad_access_during_inspection of qualified_point ;;
exception Undecided of qualified_point ;;

let inspect_qualified_point ~with_anticipation qp =
   let (Q(pt,constraints,_)) = qp in 
   match generic_access_opt ~with_anticipation pt with 
    None -> raise(Bad_access_during_inspection(qp))
    | Some (BR(_,M(reps,qpoints))) ->
      let test = Constraint.satisfied_by_individual constraints in 
      if List.exists test reps 
      then true 
      else   
       let temp1 = List.filter test reps
       and temp2 = List.filter (
        fun qp2 -> 
          (Qualified_point.insert_several_constraints constraints qp2)<>None
       ) qpoints in 
      if (temp1,temp2) = ([],[])
      then false   
      else raise(Undecided(qp)) ;; 

let action_on_mold_to_action_on_bulk_result_opt
   on_mold =
   let on_bulk_result = (
     fun (BR(reps,mold)) -> BR(reps,on_mold mold)
   ) in 
   (
    function None -> None | Some bres -> Some (on_bulk_result bres)
   ) ;;

let clean_mold ~with_anticipation (M(reps,qpoints)) =
    M(reps,List.filter (inspect_qualified_point ~with_anticipation) qpoints) ;;


exception Bad_access_during_singleton_recognition of qualified_point ;;

let recognize_singleton_mold ~with_anticipation mold =
   let (M(reps,qpoints)) = mold in 
   let temp1 = Image.image ( fun qp -> 
    let (Q(pt,constraints,extension)) = qp in 
    match generic_access_opt ~with_anticipation pt with 
    None -> raise(Bad_access_during_singleton_recognition(qp))
    | Some (BR(_,M(reps2,qpoints2))) ->
       (reps2,constraints,extension,qpoints2)
   ) qpoints in 
   if List.exists (fun (_,_,_,qpoints2)->qpoints2<>[]) temp1 
   then mold 
   else  
   let temp2 = Image.image (
    fun (reps2,constraints,extension,qpoints2)->
       let ttemp3 = List.filter (Constraint.satisfied_by_individual constraints) reps2 in 
       Image.image (i_merge extension) ttemp3
   ) temp1 in
   let whole = il_fold_merge temp2 in 
   if List.length whole = 1 
   then M(whole,[])
   else mold ;;  

let improve_mold ~with_anticipation fd =
   let fd1 = clean_mold ~with_anticipation fd in 
   let fd2 = recognize_singleton_mold ~with_anticipation fd1 in 
   fd2 ;;

let improve_bulk_result ~with_anticipation (BR(reps,mold)) =
  BR(reps,improve_mold ~with_anticipation mold) ;;

let improve_bulk_result_opt ~with_anticipation  = function 
   None -> None 
   |Some bres -> Some ( improve_bulk_result ~with_anticipation bres) ;;
      

(* The following function should only be used 
  on a point whose decomposability has already been checked ;
  otherwise the call to ancestors_for_hook will raise an exception   
*)

let unexceptional_try_hook_quickly ~with_anticipation pt hook = 
   let (AI ancestors) = ancestors_for_hook pt hook in  
   let ancestors_with_their_images = Image.image (
      fun (pt2,adj)  -> 
        let bres1_opt = generic_access_opt ~with_anticipation pt2 in 
        (pt2,
        (bres1_opt,adj,Bulk_result.extend_with_opt bres1_opt adj))
    ) ancestors in  
  let (failures,successes) = List.partition (
          fun (_,(_,_,opt)) -> opt = None
  ) ancestors_with_their_images in 
  let missing_data = Image.image fst failures in 
  if missing_data <> [] then (missing_data,None) else 
  let args = Image.image (fun (pt3,(_,adj,opt))->(Q(pt3,[],adj),Option.unpack opt)) successes in 
  let bres_opt = Bulk_result.apply_hook pt hook args in 
  ([],improve_bulk_result_opt ~with_anticipation bres_opt) ;;  

exception Try_hook_quickly_exn of point * hook * qualified_point ;;

let try_hook_quickly ~with_anticipation pt hook = 
   try unexceptional_try_hook_quickly ~with_anticipation pt hook with  
    Undecided(qp) -> raise(Try_hook_quickly_exn(pt,hook,qp)) ;;


exception Compute_from_below_exn of point ;;  

let compute_from_below ~with_anticipation pt hook =
   let (missing_data,result_opt) = 
     try_hook_quickly ~with_anticipation pt hook in 
   match  result_opt with 
   None ->raise(Compute_from_below_exn(pt)) 
   | Some result -> result ;; 

let low_add pt hook =
   let res = compute_from_below ~with_anticipation:false pt hook in  
   let _ = Accumulator_with_optional_anticipator.add_to_low_hashtbl  
             ~with_anticipation:false pt res in 
   res ;;

let med_add (width,breadth,scrappers) summary = 
  Hashtbl.replace medium_hashtbl (width,breadth,scrappers) summary ;;

let rose_add (width,breadth) summary = 
    Hashtbl.replace rose_hashtbl (width,breadth) summary ;;  
 
let find_remote_stumbling_block_or_immediate_working_hook 
~with_anticipation pt =      
    match generic_access_opt ~with_anticipation pt with 
    Some old_answer -> ([],None) 
  | None ->
   let (width,breadth,n,scrappers) = Point.unveil pt in     
   if breadth=0 
   then let (missing_data0,result_opt0) = 
        try_hook_quickly ~with_anticipation pt Jump in 
        if result_opt0<>None
        then ([],Some Jump)
        else (missing_data0,None)    
   else      
   let (missing_data1,result_opt1) = 
    try_hook_quickly ~with_anticipation pt Passive_repeat in 
   if result_opt1<>None then ([], Some Passive_repeat) else  
   if missing_data1<>[] then (missing_data1,None) else  
   let (missing_data2,result_opt2) = 
    try_hook_quickly ~with_anticipation pt Boundary_increment in 
   if result_opt2<>None then ([], Some Boundary_increment) else  
   if missing_data2<>[] then (missing_data2,None) else  
   let (missing_data3,result_opt3) = 
    try_hook_quickly ~with_anticipation pt Fork in 
   if result_opt3<>None then ([], Some Fork) else  
    (missing_data3,None) ;;
 
    
  
exception Pusher_exn ;;

let rec pusher_for_recursive_computation to_be_treated= 
    match to_be_treated with 
    [] -> raise(Pusher_exn)
    | pt :: others -> 
       (match Bulgarian.decompose pt with 
        None -> others
        |Some(pt2,adj) ->
       let (missing_data,opt_res) =
      find_remote_stumbling_block_or_immediate_working_hook 
      ~with_anticipation:true pt2 in 
      match opt_res with 
       Some hook ->
           let res = compute_from_below ~with_anticipation:true pt2 hook in  
           let _ = Accumulator_with_optional_anticipator.add_to_low_hashtbl 
           ~with_anticipation:true pt2 res in 
           others
       | None -> 
         if missing_data = [] 
         then others 
         else missing_data @ (pt::others) 
       );;      
         
let rec born_to_fail_for_recursive_computation walker=
  born_to_fail_for_recursive_computation
  (pusher_for_recursive_computation walker)  ;;     

let  needed_subcomputations_for_several_computations uples = 
  try born_to_fail_for_recursive_computation uples with 
  Pusher_exn -> !(  Accumulator_with_optional_anticipator.low_anticipator) ;; 

let needed_subcomputations_for_single_computation pt = 
  needed_subcomputations_for_several_computations [pt] ;; 

let access = generic_access ~with_anticipation:true ;;  

let compute_all_recursively pt = 
  let needed_carrier = needed_subcomputations_for_single_computation pt in 
  let answer = access pt in 
  (answer,needed_carrier) 
;;  

let force_compute pt = fst(compute_all_recursively pt) ;;

let rec all_representatives p =
    let (BR(anc_info,M(reps,qpoints))) = access p in 
    let temp1 = Image.image (
         fun (Q(pt,constraints,extension)) -> 
           let ttemp2 = all_representatives pt in 
           let ttemp3 = List.filter (Constraint.satisfied_by_individual constraints) ttemp2 in 
           Image.image (i_merge extension) ttemp3
    ) qpoints in 
    il_fold_merge (reps::temp1) ;;

(*    
rose_add (1,[]) (Usual_fobas(Parametrized_Example.bresf2));;
med_add (2,0,[]) (Usual_fos(Parametrized_Example.bresf1)) ;; 
*)

(*    


rose_add (2,[]) (Usual_fobas(Parametrized_Example.brf5));;
*)



(*
#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;

let g1 = needed_subcomputations_for_single_computation (P(4,0,8,[])) ;;

let g2 = needed_subcomputations_for_single_computation (P(3,1,7,[])) ;;



(  Accumulator_with_optional_anticipator.low_anticipator:=[]) ;;
let current_width = 2 
and current_strappers = [] ;;
let tg b n = force_compute (P(current_width,b,n,current_strappers)) ;;
let tt n = tg (n-4) n;;
let uu n = tg (n-3) n;;
let tu n = let tv=tt n and uv=uu n in (tv=uv,(tt n,uu n));;
let parf1 n =
    let (BR(opt,PR(reps,M(offshoot,qpoints)))) = tt n in 
    qpoints;;

let check_bresf1 = 
   let temp1 = Int_range.scale (fun n->
    let bres = force_compute (P(1,n,n,[])) in 
    (n,bres,Parametrized_Example.bresf1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;;     


module Parametrized_Example = struct 
include Parametrized_Example ;;
end ;;

(uu 7)= Bulk_result.append_right (force_compute (P (1, 4, 6, []))) [7] ;;
(uu 8)= Bulk_result.append_right (force_compute (P (1, 5, 7, []))) [8] ;;

Bulgarian.decompose (P(1,5,8,[])) ;;
(uu 8)=
  BR
   (Some(Passive_repeat,
      AI[(pf1(6), [7; 8]);]),
   PR ([sf1(8)],
    M ([],[Q (pf1(4), [], [6; 7; 8]); qpf1(8)]))) ;;

(uu 9)=
    BR
     (Some(Passive_repeat,
        AI[(pf1(7), [8; 9]);]),
     PR ([sf1(9)],
      M ([sf1(9)],[]))) ;;



open Parametrized_Example ;;

let pf1 x = P (1, x-2, x, []) ;; 
let qpf1 n = Q (pf1 (n-3), [], [n-1; n]) ;;
let qpf2 n = Q (pf1 (n-2), [], [n]) ;;
let qpf3 n = Q (pf1 (n-1), [], []) ;;

let pt0 = P(2,2,6,[]) ;;
let bad1 = force_compute pt0 ;;
let bad2 = needed_subcomputations_for_several_computations [pt0] ;;
let bad3 = pusher_for_recursive_computation [pt0] ;;
let (pt,others) = Listennou.ht [pt0] ;;
let (Some(pt2,adj)) = Bulgarian.decompose pt ;;
let bad4 =
      find_remote_stumbling_block_or_immediate_working_hook 
      ~with_anticipation:true pt2 ;;


let hook = Passive_repeat ;;      
let bad5 = unexceptional_try_hook_quickly  
  ~with_anticipation:true pt0 hook ;;
let (AI ancestors) = ancestors_for_hook pt0 hook ;; 
let ancestors_with_their_images = Image.image (
       fun (pt2,adj)  -> 
         let bres1_opt = generic_access_opt ~with_anticipation:true pt2 in 
         (pt2,
         (bres1_opt,adj,Bulk_result.extend_with_opt bres1_opt adj))
     ) ancestors ;;
    
    in  
   let (failures,successes) = List.partition (
           fun (_,(_,_,opt)) -> opt = None
   ) ancestors_with_their_images in 
   let missing_data = Image.image fst failures in 
   if missing_data <> [] then (missing_data,None) else 
   let args = Image.image (fun (pt3,(_,adj,opt))->(Q(pt3,[],adj),Option.unpack opt)) successes in 
   let bres_opt = Bulk_result.apply_hook pt hook args in 
   ([],improve_bulk_result_opt ~with_anticipation bres_opt) ;; 


*)

