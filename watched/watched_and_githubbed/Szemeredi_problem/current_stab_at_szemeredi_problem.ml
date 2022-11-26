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
 

let insert_several_constraints new_constraints (Q(pt,old_constraints,extension)) =
  let n = Point.size pt and scrappers = Point.scrappers pt in 
  match Constraint.insert_several (n,scrappers) (old_constraints,extension) new_constraints 
  with
    None -> None 
   |(Some final_constraints) ->  Some((Q(pt,final_constraints,extension))) ;; 


end ;;  

module Mold = struct 

(* it is assumed that compatibility has already been checked *)   
let extend_with (M(reps,qpoints)) extension =
  M(Image.image (i_merge extension) reps,
  Image.image (fun qpoint->Qualified_point.extend_with qpoint extension) qpoints
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


end ;;

  
 

module Bulk_result = struct     

let atomic_case pt = BR (Atomic,M([Point.enumerate_supporting_set pt],[])) ;; 

let extend_with pt (BR(old_sr,mold)) extension = 
 let new_sr = (if extension <> []
 then Decomposable(pt,extension)
 else old_sr) in
 BR(new_sr,Mold.extend_with mold extension);;

let extend_with_opt pt bres_opt extension = match bres_opt with 
      None -> None 
      |Some bres -> Some (extend_with pt bres extension) ;;    

let impose_one_more_constraint_opt pt cstr (BR(sr,mold)) =
    match Mold.insert_several_constraints_carefully [cstr] mold with 
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
      

let low_hashtbl = Hashtbl.create 50 ;;
  
let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;


let generic_access_opt  pt = 
 let (pt2,adj) = Simplest_reduction.decompose pt in 
 if pt2 = Empty_point 
 then Some (Bulk_result.atomic_case pt)
 else
 let (width,breadth,n,scrappers) = Point.unveil pt2 in 
  let pre_res=(
  match Hashtbl.find_opt rose_hashtbl (width,scrappers) with 
  Some summary -> Some (Parametrized.eval_fobas summary breadth n)
| None ->  
 (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.eval_fos summary n)
 | None -> Hashtbl.find_opt low_hashtbl pt2 )
) in 
Bulk_result.extend_with_opt pt2 pre_res adj ;;

let med_add (width,breadth,scrappers) summary = 
    Hashtbl.replace medium_hashtbl (width,breadth,scrappers) summary ;;
    
let rose_add (width,breadth) summary = 
    Hashtbl.replace rose_hashtbl (width,breadth) summary ;; 

let this_path = 
  home^
     "/Teuliou/OCaml/skeptical_duck/watched/watched_and_githubbed/"^
     "Szemeredi_problem/" ;; 

let ap_for_head =
    Absolute_path.of_string(this_path^
     "current_head_position_for_szemeredi_problem.ml") ;; 

let ap_for_this_file =
      Absolute_path.of_string(this_path^
       "current_stab_at_szemeredi_problem.ml") ;; 

let update_head () = 
    let new_text = Io.read_whole_file ap_for_this_file in 
    Replace_inside.overwrite_between_markers_inside_file 
     (Overwriter.of_string new_text) 
        (
          "(* Reproduced stab starts here *)",
          "(* Reproduced stab ends here *)"
        ) ap_for_head ;; 
   
let partial_superificial_result_in_jump_case  pt_after_jump =
  let (width,breadth,n,scrappers) = Point.unveil pt_after_jump in 
  let pt_before_jump = P(width-1,n-2*(width-1),n,scrappers) in  
  let (pt2,adj2) = Simplest_reduction.decompose pt_before_jump in 
  ([],Some(Decomposable(pt2,adj2))) ;; 
    

let compute_superficial_result_partially pt =  
  if pt = Empty_point then ([],Some Atomic) else
  let (width,breadth,n,scrappers) = Point.unveil pt in 
  let (pt2,adj2) = Simplest_reduction.decompose pt in 
  if ((width,breadth)=(1,0))||(pt2=Empty_point)
  then ([],Some Atomic)
  else 
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
  match generic_access_opt  preceding_point with 
    None -> ([preceding_point],None)
   |Some bres ->
       (match Bulk_result.impose_one_more_constraint_opt preceding_point front_constraint bres  with 
       None -> let tooths = Int_range.scale (fun k->
                let (m,scr) = remove_one_element  (n2,scrappers2)  (breadth2+k*width2) in 
                let pt3 = P(width2,breadth2-1,m,scr) in 
                Simplest_reduction.decompose(pt3) 
               ) 0 2  in 
              ([],Some(Fork_surface tooths))
      |Some bres2 -> ([],Some(Contraction_surface(preceding_point,front_constraint)))) ;; 


exception Bad_contraction of point * constraint_t ;; 

let rec compute_bulk_result_partially pt =  
  if pt = Empty_point then ([],Some(Bulk_result.atomic_case pt)) else 
   let partial_res1 = compute_superficial_result_partially pt in 
   match snd partial_res1 with 
    None -> (fst partial_res1,None) 
   |Some sr ->(match sr with 
     Atomic -> ([],Some(Bulk_result.atomic_case pt)) 
   | Decomposable(pt2,adj2) -> 
       let partial_res2 = compute_bulk_result_partially pt2 in 
       (
        match snd partial_res2 with 
        None -> (fst partial_res2,None) 
       |Some br2 -> ([],Some (Bulk_result.extend_with pt2 br2 adj2))
       )
   | Contraction_surface (pt5,cstr) ->
    let partial_res4 = compute_bulk_result_partially pt5 in 
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
      let partial_res5 = compute_bulk_result_partially last_pt in 
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

let compute_bulk_result_partially_with_helper pt helper =
   match List.assoc_opt pt helper with 
   Some answer -> ([],Some answer) 
   | None ->  compute_bulk_result_partially pt ;; 
   
let add_if_necessary (a,b) assoc_list = 
  if List.mem_assoc a assoc_list 
  then assoc_list 
  else (a,b) :: assoc_list ;;   

exception Pusher_stop ;;

let pusher_for_bulk_result_computation  
   (treated,to_be_treated) = match to_be_treated with 
         [] -> raise Pusher_stop
         | pt1 :: other_pts ->
           let partial_res1 = compute_bulk_result_partially pt1 in 
           match snd partial_res1 with 
            None -> (treated,(fst partial_res1)@to_be_treated)
           |Some answer -> (add_if_necessary (pt1,answer) treated,other_pts) ;;

let rec compute_bulk_results walker =
    let (treated,to_be_treated) = walker in 
    match to_be_treated with 
    [] -> treated
    | _ -> compute_bulk_results (pusher_for_bulk_result_computation walker) ;;


let compute_bulk_result pt = compute_bulk_results ([],[pt]) ;; 
