(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)

open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz_types ;; 
open Sz_preliminaries_for_stab ;;


let cil_order = ((fun (C x) (C y)->il_order x y) : constraint_t Total_ordering_t.t) ;;

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
  let new_z =  Finite_int_set.of_pair (n,new_scrappers) in 
  let new_max = List.hd(List.rev new_z) in 
  (new_max,List.filter (fun t->t<new_max) new_scrappers) ;;


(*

remove_one_element (10,[3;7;8;9]) 10 ;;

*)



module Constraint = struct  

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
  let whole = Finite_int_set.of_pair (n,scrappers) in 
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
  
exception Insert_several_constraints_carefully_exn of constraint_t list * mold ;;

let insert_several_constraints_carefully extra_constraints old_mold =
   let new_mold = insert_several_constraints extra_constraints old_mold in 
   let (M(new_reps,new_qpoints)) = new_mold in     
    if new_qpoints = [] 
    then None 
    else
    if new_reps = []
    then raise(Insert_several_constraints_carefully_exn(extra_constraints,old_mold))
    else Some new_mold ;;          

let expand_qpoint (Q(_,constraints,extension)) mold =
  extend_with (insert_several_constraints constraints mold) extension ;; 

let replace_in_qpoint_list (pivot,mold) qpoint_list =
  let pivot_found = ref false 
  and (M(expanded_reps,expanded_qpoints)) = expand_qpoint pivot mold in 
  let temp1 = Image.image (
     fun qpoint ->
       if qpoint = pivot 
       then let _ = (pivot_found:=true) in 
             expanded_qpoints
       else [qpoint]      
  ) qpoint_list in 
  let new_reps = (if !pivot_found then expanded_reps else []) in 
  (new_reps,List.flatten temp1) ;;

let compute_full_replacement (pivot,active_mold) passive_mold =
  let (M(old_reps,old_qpoints)) = passive_mold in 
  let (new_reps,final_qpoints) = replace_in_qpoint_list (pivot,active_mold)  old_qpoints in 
  M(old_reps@new_reps,final_qpoints) ;;
  
exception Bad_minimal_insertion of qualified_point * mold * solution list ;;  

let do_minimal_insertion_in_qpoint_list (pivot,mold,chosen_insertions) qpoint_list =
  let pivot_found = ref false 
  and (M(expanded_reps,expanded_qpoints)) = expand_qpoint pivot mold in 
  let insertion_is_allowed = il_is_included_in chosen_insertions expanded_reps 
  and replacement_for_pivot = (
     if (chosen_insertions = expanded_reps) && (expanded_qpoints=[])
     then None 
     else Some pivot
  ) in 
  let final_qpoints = Option.filter_and_unpack (
     fun qpoint ->
       if qpoint = pivot 
       then let _ = (pivot_found:=true) in 
            (if not(insertion_is_allowed)
             then raise(Bad_minimal_insertion(pivot,mold,chosen_insertions)) 
             else replacement_for_pivot
            )
       else Some qpoint      
  ) qpoint_list in 
  let new_reps = (if !pivot_found then chosen_insertions else []) in 
  (new_reps,final_qpoints) ;;

let compute_minimal_insertion (pivot,active_mold,chosen_insertions) passive_mold =
  let (M(old_reps,old_qpoints)) = passive_mold in 
  let (new_reps,final_qpoints) = do_minimal_insertion_in_qpoint_list 
          (pivot,active_mold,chosen_insertions)  old_qpoints in 
  M(old_reps@new_reps,final_qpoints) ;;


let insert_several_constraints_opt extra_constraints mold = 
   let new_mold = insert_several_constraints extra_constraints mold in 
   if new_mold = M([],[])
   then None 
   else Some new_mold ;;   

let apply_passive_repeat  pt mold =
   let (width,b,_,_) = Point.unveil pt in 
   insert_several_constraints_opt [C[b;b+width;b+2*width]] mold;;

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

  
  
let nondecomposed_ancestors_for_hook pt hook = 
    let (width,breadth,n,scrappers) = Point.unveil pt in  
    match hook with 
    Passive_repeat -> [P(width,breadth-1,n,scrappers)]     
   | Fork ->     
       Int_range.scale (fun k->
          let (m,scr) = remove_one_element  (n,scrappers)  (breadth+k*width) in 
          P(width,breadth-1,m,scr)
        ) 0 2 
   | Jump -> [P(width-1,n-2*(width-1),n,scrappers)] ;;

exception Already_decomposable_point of point ;; 

let ancestors_for_hook pt0 hook = 
  match Simplest_reduction.decompose pt0 with 
  None -> raise(Already_decomposable_point(pt0))
  |Some(pt1,adj1) ->
  let temp1 = nondecomposed_ancestors_for_hook pt1 hook in 
  (AI(Image.image (fun pt2-> 
    match Simplest_reduction.decompose pt2 with 
     None -> (pt2,adj1)
    |Some(pt3,adj3) -> (pt3,i_merge adj1 adj3)
  ) temp1))  ;; 

module Deprecated_bulk_result = struct 

    let common_length (DBR(_,pres)) = Mold.common_length pres ;;
    let partial (DBR(_,pres)) = pres ;;  
    let make ancestor_info pres = DBR(ancestor_info,pres) ;;
    let singleton ancestry z = DBR(ancestry,Mold.singleton z)  ;;
    
    let extend_with (DBR(ancestry_opt,pres)) extension = 
       let new_ancestry_opt = (
          match ancestry_opt with 
            None -> None 
           |Some(hook,anc_info) -> Some(hook,Ancestry_info.extend_with anc_info extension)
       ) in 
      DBR(
        new_ancestry_opt,
        Mold.extend_with pres extension
      );;
    
    let append_right (DBR(ancestry_opt,pres)) extension = 
        let new_ancestry_opt = (
           match ancestry_opt with 
             None -> None 
            |Some(hook,anc_info) -> Some(hook,Ancestry_info.append_right anc_info extension)
        ) in 
       DBR(
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
       |Some new_mold ->
            let anc_info = Some(hook,ancestors_for_hook pt hook) in  
            Some(DBR(anc_info,new_mold));;
      
    let compute_full_replacement (DBR(anc_info,mold)) replacement_data =
      DBR(anc_info,Mold.compute_full_replacement replacement_data mold) ;; 

    let apply_several_replacements bres replacements = 
        List.fold_left compute_full_replacement bres replacements ;;  

        let compute_minimal_insertion (DBR(anc_info,mold)) minins_data =
          DBR(anc_info,Mold.compute_minimal_insertion minins_data mold) ;;     

    let apply_several_minimal_insertions bres minins_data = 
          List.fold_left compute_minimal_insertion bres minins_data ;;    

    end ;;  
    

module Bulk_result = struct     

let atomic_case pt = BR (Atomic,M([Point.enumerate_supporting_set pt],[])) ;; 

let jump_from_atom pt = BR (Jump_from_atom(pt),M([Point.enumerate_supporting_set pt],[])) ;;

let extend_with pt (BR(old_sr,mold)) extension = 
 let new_sr = (if extension <> []
 then Decomposable(pt,extension)
 else old_sr) in
 BR(new_sr,Mold.extend_with mold extension);;

let extend_with_opt pt bres_opt extension = match bres_opt with 
      None -> None 
      |Some bres -> Some (extend_with pt bres extension) ;;    

let impose_one_more_constraint_opt pt cstr (BR(sr,mold)) =
    match Mold.insert_several_constraints_opt [cstr] mold with 
     None -> None
    | Some new_mold -> Some(BR(Contraction_surface(pt,cstr),new_mold)) ;;
     

end ;;  
    
    module Parametrized = struct 

      let deprecated_eval_fos fos n =
        match fos with 
           Dusual_fos f -> f n ;; 
     
     let deprecated_eval_fobas fobas breadth n = 
       match fobas with 
        Dusual_fobas f -> f breadth n ;;  

      
      let eval_fos fos n =
         match fos with 
            Usual_fos f -> f n ;; 
      
      let eval_fobas fobas breadth n = 
        match fobas with 
         Usual_fobas f -> f breadth n ;;  
  
      
    
    end ;;   
    
    
module Parametrized_Example = struct 
    
let sf1 n = List.filter (fun t->List.mem(t mod 3)[1;2]) (Int_range.range 1 n) ;;
let sf2 n = List.filter (fun t->List.mem(t mod 3)[0;1]) (Int_range.range 1 n) ;;  

(*
let check_sf1 = 
  let temp1 = Int_range.scale (fun n->
   let (DBR(opt,M(reps,_))) = force_compute (P(2,0,n,[])) in 
   (n,reps,[Parametrized_Example.sf1 n])) 1 40 in 
  List.filter (fun (n,a,b)->a<>b) temp1 ;; 
*)  

let vp1 = P (1, 0, 3, [1]) ;; 
let vp2 = P (1, 0, 3, [2]) ;; 

let pf1 x = P (1, x-2, x, []) ;; 
let pf2 x = P (2, x-5, x, []) ;; 

let aif1 n =
   match List.assoc_opt n [1,None;2,None;
    3, Some(Fork,AI[(vp1, []); (vp2, []); (pf1 2, [])])
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
    let (DBR(opt,_)) = force_compute (P(2,0,n,[])) in 
    (n,opt,Parametrized_Example.aif1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 

*)

let aif2 n =
  match List.assoc_opt n [1,None;2,None;
  3, Some(Fork,AI[(vp1, []); (vp2, []); (pf1 2, [])]);
   4, Some (Passive_repeat,AI[(pf1 3, [4])]);
   5, Some (Passive_repeat,AI[(pf1 5, [])])
  ] with 
  Some answer -> answer 
  | None -> Some(Passive_repeat,AI[(pf2(n), [])]) ;;
   
(*
  
let check_aif2 = 
  let temp1 = Int_range.scale (fun n->
   let (DBR(opt,_)) = force_compute (P(3,0,n,[])) in 
   (n,opt,Parametrized_Example.aif2 n)) 1 40 in 
  List.filter (fun (n,a,b)->a<>b) temp1 ;; 

*)


let vqp1 = Q (vp1, [], []) ;;
let vqp2 = Q (vp2, [], []) ;;

let qpf1 n = Q (pf1 (n-3), [], [n-1; n]) ;;
let qpf2 n = Q (pf1 (n-2), [], [n]) ;;
let qpf3 n = Q (pf1 (n-1), [], []) ;;

let moldf1 n =
  match List.assoc_opt n [
    1,M([[1]],[]);3,M([sf1(3)],[vqp1; vqp2;qpf3(3)]);
    4,M([sf1(4);[1;3;4]],[qpf2(4)])
  ]  with Some answer -> answer | None ->
  (match (n mod 3) with 
    0 -> M([sf1(n)],[qpf1(n); qpf2(n)  ;qpf3(n)]) 
  | 1 -> M([sf1(n);sf2(n)],[qpf1(n); qpf2(n)])
  | 2 -> M([sf1(n)],[])
  |_ ->failwith("impossible remainder by 3")) ;;  


(*
let check_moldf1 = 
   let temp1 = Int_range.scale (fun n->
    let (DBR(opt,mold)) = force_compute (P(2,0,n,[])) in 
    (n,mold,Parametrized_Example.moldf1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 
*)    

let bresf1 n = DBR(aif1(n),moldf1(n)) ;;
  
(*
let check_bresf1 = 
   let temp1 = Int_range.scale (fun n->
    let bres = force_compute (P(2,0,n,[])) in 
    (n,bres,Parametrized_Example.bresf1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 
*)    

let bresf2 breadth n = 
  if breadth = 0 then Deprecated_bulk_result.singleton None (Int_range.range 1 n) else
  if n <= breadth + 2 then bresf1 n else 
  Deprecated_bulk_result.append_right (bresf1(breadth+2)) (Int_range.range (breadth+3) n)  ;;

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

let deprecated_low_hashtbl = Hashtbl.create 50 ;;
let deprecated_low_anticipator = ref [] ;; 
let deprecated_get_from_low_hashtbl ~with_anticipation pt =
      if not(with_anticipation)
      then  Hashtbl.find_opt deprecated_low_hashtbl pt 
      else
          match List.assoc_opt pt (!deprecated_low_anticipator) with 
          Some anticiped_answer -> Some anticiped_answer 
          | None -> Hashtbl.find_opt deprecated_low_hashtbl pt  ;;
  
let deprecated_add_to_low_hashtbl  ~with_anticipation pt vaal=
    if not(with_anticipation)
    then   Hashtbl.replace deprecated_low_hashtbl pt vaal
    else deprecated_low_anticipator := (pt,vaal) :: (!deprecated_low_anticipator)  ;;



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

let deprecated_rose_hashtbl = Hashtbl.create 50 ;;
let deprecated_medium_hashtbl = Hashtbl.create 50 ;;



let deprecated_generic_access_opt  ~with_anticipation pt = 
 match Simplest_reduction.decompose pt with 
 None -> Some (Deprecated_bulk_result.singleton None (Point.enumerate_supporting_set pt))
 | Some(pt2,adj) ->
let (width,breadth,n,scrappers) = Point.unveil pt2 in 
let pre_res=(
match Hashtbl.find_opt deprecated_rose_hashtbl (width,scrappers) with 
Some summary -> Some (Parametrized.deprecated_eval_fobas summary breadth n)
| None ->  
 (match Hashtbl.find_opt deprecated_medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.deprecated_eval_fos summary n)
 | None -> Accumulator_with_optional_anticipator.deprecated_get_from_low_hashtbl ~with_anticipation pt2) 
) in 
Deprecated_bulk_result.extend_with_opt pre_res adj ;;


let generic_access_opt  ~with_anticipation pt = 
 match Simplest_reduction.decompose pt with 
 None -> Some (Bulk_result.atomic_case pt)
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
Bulk_result.extend_with_opt pt2 pre_res adj ;;


let generic_access ~with_anticipation pt = 
   Option.unpack(deprecated_generic_access_opt ~with_anticipation pt) ;;   

exception Bad_access_during_inspection of qualified_point ;;
exception Undecided of qualified_point ;;

let inspect_qualified_point ~with_anticipation qp =
   let (Q(pt,constraints,_)) = qp in 
   match deprecated_generic_access_opt ~with_anticipation pt with 
    None -> raise(Bad_access_during_inspection(qp))
    | Some (DBR(_,M(reps,qpoints))) ->
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
     fun (DBR(reps,mold)) -> DBR(reps,on_mold mold)
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
    match deprecated_generic_access_opt ~with_anticipation pt with 
    None -> raise(Bad_access_during_singleton_recognition(qp))
    | Some (DBR(_,M(reps2,qpoints2))) ->
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

let improve_bulk_result ~with_anticipation (DBR(reps,mold)) =
  DBR(reps,improve_mold ~with_anticipation mold) ;;

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
        let bres1_opt = deprecated_generic_access_opt ~with_anticipation pt2 in 
        (pt2,
        (bres1_opt,adj,Deprecated_bulk_result.extend_with_opt bres1_opt adj))
    ) ancestors in  
  let (failures,successes) = List.partition (
          fun (_,(_,_,opt)) -> opt = None
  ) ancestors_with_their_images in 
  let missing_data = Image.image fst failures in 
  if missing_data <> [] then (missing_data,None) else 
  let args = Image.image (fun (pt3,(_,adj,opt))->(Q(pt3,[],adj),Option.unpack opt)) successes in 
  let bres_opt = Deprecated_bulk_result.apply_hook pt hook args in 
  ([],improve_bulk_result_opt ~with_anticipation bres_opt) ;;  

exception Try_hook_quickly_exn of point * hook * qualified_point ;;

let memorizer_for_try_hook_quickly = ref None ;;

let try_hook_quickly ~with_anticipation pt hook = 
   try unexceptional_try_hook_quickly ~with_anticipation pt hook with  
    Undecided(qp) -> 
      let _ = (memorizer_for_try_hook_quickly:=Some(pt,hook,qp)) in 
      raise(Try_hook_quickly_exn(pt,hook,qp)) ;;

let enhancement_data = ref [
] ;;

let add_enhancement_data pair =
   (
    Accumulator_with_optional_anticipator.deprecated_low_anticipator:=[];
    enhancement_data := (!enhancement_data)@[pair]) ;; 

let test1_for_enhancement (P(w,b,n,s)) = None ;; 


let omega_enhancements pt = 
  match test1_for_enhancement pt with 
    (Some q) ->    
      Some [(Q (P (1, 3*q-4, 3*q-2, []), [], [3*q; 3*q+1]), 
       [Parametrized_Example.sf2 (3*q+1)])]     
  | None -> None ;;

let get_enhancements_opt pt = 
   match omega_enhancements pt with 
     Some result -> Some result 
     | None -> List.assoc_opt pt (!enhancement_data) ;;   

exception Access_error_during_enhancement of point * point ;; 

let enhance_first_time_result ~with_anticipation pt result = 
  match get_enhancements_opt pt  with 
  None -> result
  | (Some pivots) -> 
     let temp1 = Image.image (
       fun (qp,chosen_reps) -> let (Q(pt2,_,_)) = qp in 
         try (qp,generic_access ~with_anticipation pt2,chosen_reps) with 
         _ -> raise (Access_error_during_enhancement(pt,pt2))
     ) pivots in  
    let minimal_insertions = Image.image 
      (fun (qp,DBR(_,mold),chosen_reps)->(qp,mold,chosen_reps)) temp1 in 
     Deprecated_bulk_result.apply_several_minimal_insertions result minimal_insertions;; 

exception Compute_from_below_exn of point ;;  

let compute_from_below ~with_anticipation pt hook =
   let (missing_data,result_opt) = 
     try_hook_quickly ~with_anticipation pt hook in 
   match  result_opt with 
   None ->raise(Compute_from_below_exn(pt)) 
   | Some result -> enhance_first_time_result ~with_anticipation pt result ;; 


let low_add pt hook =
   let res = compute_from_below ~with_anticipation:false pt hook in  
   let _ = Accumulator_with_optional_anticipator.deprecated_add_to_low_hashtbl  
             ~with_anticipation:false pt res in 
   res ;;

let  deprecated_med_add (width,breadth,scrappers) summary = 
  Hashtbl.replace deprecated_medium_hashtbl (width,breadth,scrappers) summary ;;

let  deprecated_rose_add (width,breadth) summary = 
    Hashtbl.replace deprecated_rose_hashtbl (width,breadth) summary ;;  
 
(*    
let med_add (width,breadth,scrappers) summary = 
    Hashtbl.replace medium_hashtbl (width,breadth,scrappers) summary ;;
    
let rose_add (width,breadth) summary = 
    Hashtbl.replace rose_hashtbl (width,breadth) summary ;; 
*)

   
let partial_superificial_result_in_jump_case  pt_after_jump =
  let (width,breadth,n,scrappers) = Point.unveil pt_after_jump in 
  let pt_before_jump = P(width-1,n-2*(width-1),n,scrappers) in  
  match Simplest_reduction.decompose pt_before_jump with 
  None -> ([],Some(Jump_from_atom(pt_before_jump)))
  | Some (pt2,adj2) -> ([],Some(Jump_surface(pt2,adj2))) ;; 
    

let compute_superficial_result_partially ~with_anticipation pt =  
  let (width,breadth,n,scrappers) = Point.unveil pt in 
  let opt = Simplest_reduction.decompose pt in 
  if ((width,breadth)=(1,0))||(opt=None)
  then ([],Some Atomic)
  else 
  let (pt2,adj2) = Option.unpack opt in 
  if adj2<>[]
  then ([],Some(Decomposable(pt2,adj2)))
  else     
  if breadth = 0
  then partial_superificial_result_in_jump_case pt   
  else
  let (width2,breadth2,n2,scrappers2) = Point.unveil pt2 in 
  let _ = assert(breadth2>0) in 
  let front_constraint = C [width2;width2+breadth2;width2+2*breadth2] 
  and preceding_point = P(width2,breadth2-1,n2,scrappers2) in 
  match generic_access_opt  ~with_anticipation preceding_point with 
    None -> ([preceding_point],None)
   |Some bres ->
       (match Bulk_result.impose_one_more_constraint_opt preceding_point front_constraint bres  with 
       None -> let tooths = Int_range.scale (fun k->
                let (m,scr) = remove_one_element  (n2,scrappers2)  (breadth2+k*width2) in 
                let pt3 = P(width2,breadth2-1,m,scr) in 
                match Simplest_reduction.decompose(pt3) with 
                 None -> (* TODO : create an empty point variant *)
                          let z = Point.enumerate_supporting_set pt3 in   
                          (P(1,0,0,[]),z)
                 |Some(pt4,adj4) -> (pt4,adj4) 
               ) 0 2  in 
              ([],Some(Fork_surface tooths))
      |Some bres2 -> ([],Some(Contraction_surface(preceding_point,front_constraint)))) ;; 


exception Bad_contraction of point * constraint_t ;; 

let rec compute_bulk_result_partially ~with_anticipation pt =  
   let partial_res1 = compute_superficial_result_partially ~with_anticipation pt in 
   match snd partial_res1 with 
    None -> (fst partial_res1,None) 
   |Some sr ->(match sr with 
     Atomic -> ([],Some(Bulk_result.atomic_case pt)) 
   | Decomposable(pt2,adj2) -> 
       let partial_res2 = compute_bulk_result_partially ~with_anticipation pt2 in 
       (
        match snd partial_res2 with 
        None -> (fst partial_res2,None) 
       |Some br2 -> ([],Some (Bulk_result.extend_with pt2 br2 adj2))
       )
   | Jump_from_atom (pt3) -> ([],Some(Bulk_result.jump_from_atom pt))
   | Jump_surface(pt4,adj4) ->
    let partial_res3 = compute_bulk_result_partially ~with_anticipation pt4 in 
    (
     match snd partial_res3 with 
     None -> (fst partial_res3,None) 
    |Some br3 -> ([],Some (Bulk_result.extend_with pt4 br3 adj4))
    )
   | Contraction_surface (pt5,cstr) ->
    let partial_res4 = compute_bulk_result_partially ~with_anticipation pt5 in 
    (
     match snd partial_res4 with 
     None -> (fst partial_res4,None) 
    |Some br4 -> 
      match Bulk_result.impose_one_more_constraint_opt pt5 cstr br4 with 
        None -> raise(Bad_contraction(pt5,cstr))
        |Some new_br4 ->([],Some new_br4)
    ) 
   | Fork_surface cases ->
      let (last_pt,last_adj) = List.nth cases 2 in 
      let partial_res5 = compute_bulk_result_partially ~with_anticipation last_pt in 
    (
     match snd partial_res5 with 
     None -> (fst partial_res5,None) 
    |Some br5 -> 
       let (BR(_,M(reps,_))) = br5 in 
       let new_mold = M(reps,Image.image (
        fun (pt6,adj6)-> Q(pt6,[],adj6)
     ) cases) in 
      ([],Some (BR(Fork_surface cases,new_mold)))
    )
   ) ;; 

let from_partial_to_full f_partial ~with_anticipation pts0 =
  let rec main = (
     fun pts -> match pts with 
       [] -> Image.image (fun pt->Option.unpack(snd(f_partial ~with_anticipation pt))) pts0 
       | pt1 :: other_pts ->
         let partial_res1 = f_partial ~with_anticipation pt1 in 
         match snd partial_res1 with 
          None -> main((fst partial_res1)@pts)
         |Some _ -> main other_pts 
  ) in 
  main ;;

let generic_compute_bulk_results ~with_anticipation pts0 =
  from_partial_to_full compute_bulk_result_partially ~with_anticipation pts0 ;;



let find_remote_stumbling_block_or_immediate_working_hook 
~with_anticipation pt =      
    match deprecated_generic_access_opt ~with_anticipation pt with 
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
   let (missing_data3,result_opt3) = 
    try_hook_quickly ~with_anticipation pt Fork in 
   if result_opt3<>None then ([], Some Fork) else  
    (missing_data3,None) ;;
 
    
  
exception Pusher_exn ;;

let rec pusher_for_recursive_computation to_be_treated= 
    match to_be_treated with 
    [] -> raise(Pusher_exn)
    | pt :: others -> 
       (match Simplest_reduction.decompose pt with 
        None -> others
        |Some(pt2,adj) ->
       let (missing_data,opt_res) =
      find_remote_stumbling_block_or_immediate_working_hook 
      ~with_anticipation:true pt2 in 
      match opt_res with 
       Some hook ->
           let res = compute_from_below ~with_anticipation:true pt2 hook in  
           let _ = Accumulator_with_optional_anticipator.deprecated_add_to_low_hashtbl 
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
  Pusher_exn -> !(  Accumulator_with_optional_anticipator.deprecated_low_anticipator) ;; 

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
    let (DBR(anc_info,M(reps,qpoints))) = access p in 
    let temp1 = Image.image (
         fun (Q(pt,constraints,extension)) -> 
           let ttemp2 = all_representatives pt in 
           let ttemp3 = List.filter (Constraint.satisfied_by_individual constraints) ttemp2 in 
           Image.image (i_merge extension) ttemp3
    ) qpoints in 
    il_fold_merge (reps::temp1) ;;

let all_representatives_for_qpoint (Q(pt,constraints,extension)) =
   let temp1 = all_representatives pt in 
   let temp2 = List.filter (Constraint.satisfied_by_individual constraints) temp1 in 
   Image.image (i_merge extension) temp2 ;; 

let zoom (Q(pt,constraints,extension)) = 
   let (DBR(_,M(_,qpoints))) = force_compute pt in 
   Image.image (
     fun qp3 ->
       let ttemp1 = all_representatives_for_qpoint qp3 in 
       (qp3,List.filter 
       (Constraint.satisfied_by_individual constraints) ttemp1)
   ) qpoints ;; 

let usual_zoom () =   
   let (pt,hook,qp) = Option.unpack(!memorizer_for_try_hook_quickly) in 
   let (Q(pt1,_,_)) = qp
   and temp1 = zoom qp in 
   let (qp1,sols1) = Listennou.force_find (fun (_,sols)->sols<>[]) temp1  in  
   ((pt1,[qp1,sols1]),temp1);;   

(*    
rose_add (1,[]) (Usual_fobas(Parametrized_Example.bresf2));;
med_add (2,0,[]) (Usual_fos(Parametrized_Example.bresf1)) ;; 
*)

(*    


rose_add (2,[]) (Usual_fobas(Parametrized_Example.brf5));;
*)




