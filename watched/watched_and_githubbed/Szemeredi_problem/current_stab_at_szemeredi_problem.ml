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

let singleton z = M([z],[])  ;;


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

(*
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
    

   
let check_aif1 = 
   let temp1 = Int_range.scale (fun n->
    let (DBR(opt,_)) = force_compute (P(2,0,n,[])) in 
    (n,opt,Parametrized_Example.aif1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 

*)

(*
let aif2 n =
  match List.assoc_opt n [1,None;2,None;
  3, Some(Fork,AI[(vp1, []); (vp2, []); (pf1 2, [])]);
   4, Some (Passive_repeat,AI[(pf1 3, [4])]);
   5, Some (Passive_repeat,AI[(pf1 5, [])])
  ] with 
  Some answer -> answer 
  | None -> Some(Passive_repeat,AI[(pf2(n), [])]) ;;
   

  
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

(*
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



let check_moldf1 = 
   let temp1 = Int_range.scale (fun n->
    let (DBR(opt,mold)) = force_compute (P(2,0,n,[])) in 
    (n,mold,Parametrized_Example.moldf1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 
*)    

(*
let bresf1 n = DBR(aif1(n),moldf1(n)) ;;
  

let check_bresf1 = 
   let temp1 = Int_range.scale (fun n->
    let bres = force_compute (P(2,0,n,[])) in 
    (n,bres,Parametrized_Example.bresf1 n)) 1 40 in 
   List.filter (fun (n,a,b)->a<>b) temp1 ;; 
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



(*
let low_add pt hook =
   let res = compute_from_below ~with_anticipation:false pt hook in  
   let _ = Accumulator_with_optional_anticipator.add_to_low_hashtbl  
             ~with_anticipation:false pt res in 
   res ;;
*)


    
let med_add (width,breadth,scrappers) summary = 
    Hashtbl.replace medium_hashtbl (width,breadth,scrappers) summary ;;
    
let rose_add (width,breadth) summary = 
    Hashtbl.replace rose_hashtbl (width,breadth) summary ;; 


   
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

let compute_bulk_results pts0 =
  from_partial_to_full 
     compute_bulk_result_partially 
       ~with_anticipation:true pts0 ;;

let compute_bulk_result pt = compute_bulk_results [pt] ;; 



