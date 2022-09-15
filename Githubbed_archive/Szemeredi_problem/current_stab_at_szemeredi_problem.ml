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

module Sycomore_list = struct 
  
        let extend_with sycom extension = match sycom with 
        Singleton(l) -> Singleton(i_merge l extension)
        | Breakpoint_with_extensions(Q(pt,old_constraints,extension2)) -> 
          Breakpoint_with_extensions(Q(pt,old_constraints,i_merge extension extension2))   ;;    


         let insert_several_constraints extra_constraints sycom =
            match sycom with 
             (Singleton l) -> if Constraint.satisfied_by_individual extra_constraints l 
              then Some(Singleton l)
              else None 
             | _ -> Some sycom ;; 
      
         let extract_singleton_opt = function 
           (Singleton l) -> Some l | _ -> None ;;

        exception Extract_qualified_point_exn of sycomore_list ;; 

         let extract_qualified_point sycom = match sycom with 
           (Breakpoint_with_extensions(qp)) -> qp 
           | _ -> raise(Extract_qualified_point_exn(sycom)) ;;  

end ;;  

module Forced_data = struct 

(* it is assumed that compatibility has already been checked *)   
let extend_with (FD(offshoots,qpoints)) extension =
  FD(Image.image (i_merge extension) offshoots,
  Image.image (fun qpoint->Qualified_point.extend_with qpoint extension) qpoints
  ) ;;  


let insert_several_constraints extra_constraints (FD(offshoots,qpoints)) = 
  FD(List.filter (Constraint.satisfied_by_individual extra_constraints) offshoots,
     Option.filter_and_unpack (
      Qualified_point.insert_several_constraints extra_constraints
     ) qpoints) ;; 
    

end ;;

 

module Bulk_result = struct 

let common_length (BR(sycom,representatives,forced_data)) =
    List.length(List.hd representatives);;


let extend_with (BR(sycom,representatives,forced_data)) extension =
    BR(Sycomore_list.extend_with sycom extension,
      Image.image (i_merge extension) representatives,
      Forced_data.extend_with forced_data extension
    );;

let insert_several_constraints extra_constraints 
  (BR(sycom,representatives,forced_data)) = 
   match Sycomore_list.insert_several_constraints extra_constraints sycom with 
   None -> None 
  | Some new_sycom -> 
     let new_forced_data = Forced_data.insert_several_constraints extra_constraints forced_data in 
     if new_forced_data = FD([],[])
     then None 
     else   
     Some(BR(new_sycom,
      List.filter (Constraint.satisfied_by_individual extra_constraints) representatives,
      Forced_data.insert_several_constraints extra_constraints forced_data
     )) ;;
    
let apply_passive_repeat  pt bres =
    let (width,b,_,_) = Point.unveil pt in 
    insert_several_constraints [C[b;b+width;b+2*width]] bres ;;
  
let apply_boundary_increment pt bres = 
    let (width,breadth,n,_) = Point.unveil pt in 
    let new_constraints = Constraint.extra_constraints_from_boundary_increment width breadth n in 
    match insert_several_constraints new_constraints bres with 
     None -> None 
    |Some new_bres -> Some(extend_with new_bres [n]) ;;

    
let apply_fork pt ll =
   let (_,temp1) = Max.maximize_it_with_care common_length  ll in  
   let new_representatives = (fun (BR(_,r,_))->r) (List.hd(List.rev(temp1))) in 
   let temp2 = Image.image (fun ebr -> let (BR(sycom,_,_)) = ebr in 
        (ebr,Sycomore_list.extract_singleton_opt sycom ) ) temp1 in 
   let (temp3,temp4) = List.partition (fun (_,opt_singleton)->opt_singleton=None) temp2 in 
   let new_forced_data = FD(
    il_sort(Image.image (fun (_,opt_singleton)->Option.unpack opt_singleton) temp4),
    Image.image (fun (ebr,_)->
      let (BR(sycom,_,_)) = ebr in 
         Sycomore_list.extract_qualified_point sycom 
    ) temp3 
   ) in 
   BR(Breakpoint_with_extensions(Q(pt,[],[])),new_representatives,new_forced_data) ;; 


end ;;  


module Parametrized = struct 


    let eval_fw1 (FW1 l) n =
      let (m,final_case) = List.hd(List.rev l) in 
      if n < m 
      then List.assoc n l 
      else Bulk_result.extend_with final_case (Int_range.range (m+1) n) ;;   
  
  let eval_fos fos n =
     match fos with 
       Width_one fw1 -> eval_fw1 fw1 n 
      | Usual_fos f -> f n ;; 
  
  let eval_foscras foscras scrappers n = 
    match foscras with 
     Usual_foscras f -> f scrappers n ;;  

   

end ;;   


module Parametrized_Example = struct 


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
    match Private.find_meaningful_obstruction (old_width,old_breadth) domain with 
      None -> None 
      | Some (width,breadth) -> Some ((width,breadth),Private.detach (width,breadth) domain);;  
  
  
  end ;;  
  
  module Bulgarian = struct 
  
    let decompose pt =
        let (old_width,old_breadth,n,scrappers) = Point.unveil pt in 
        let domain = concretize (n,scrappers) in 
         match Bulgarian_for_nonparametrized_sets.decompose (old_width,old_breadth) domain with 
         None -> None 
         | Some ((new_width,new_breadth),(new_domain,adjustment)) -> 
            let (new_n,new_scrappers) = abstractize new_domain in 
          Some (P(new_width,new_breadth,new_n,new_scrappers),adjustment);;
      
  (*
     
  let check1 = (decompose (P(1,4,6,[])) =  Some (P (1, 4, 6, []), [])) ;;
  let check2 = (decompose (P(1,3,6,[])) =  Some (P (1, 3, 5, []), [6])) ;;
  
  *)
  
  end ;;   
  


let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;

let polish_fork ~with_anticipation pt unpolished_result =
    Some unpolished_result ;; 

let apply_hook_naively ~with_anticipation pt hook ll =  
  match hook with 
  Passive_repeat -> Bulk_result.apply_passive_repeat pt (List.hd ll)
| Boundary_increment -> Bulk_result.apply_boundary_increment pt (List.hd ll)
| Fork ->  polish_fork ~with_anticipation pt (Bulk_result.apply_fork pt ll)
| Jump -> Some(List.hd ll);; 

exception Apply_hook_exn of point * hook_in_knowledge ;;


let apply_hook ~with_anticipation pt hook ll = 
   match apply_hook_naively ~with_anticipation pt hook ll with 
   None -> None 
    |Some bres ->
       let (BR(_,reps,_)) = bres in 
       if reps = []
       then raise (Apply_hook_exn(pt,hook)) 
       else Some bres ;;



  
let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;       

let nonbulgarian_getter  ~with_anticipation pt = 
let (width,breadth,n,scrappers) = Point.unveil pt in 
let z = concretize (n,scrappers) in 
if ((width,breadth)=(1,0))||(test_for_admissiblity width breadth z) 
then Some (BR(Singleton z,[z],FD([z],[]))) 
else 
match Hashtbl.find_opt rose_hashtbl (width,breadth) with 
Some summary -> Some (Parametrized.eval_foscras summary scrappers n)
| None ->  
 (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.eval_fos summary n)
 | None -> Accumulator_with_optional_anticipator.get_from_low_hashtbl ~with_anticipation pt) ;;   


let bulgarian_getter ~with_anticipation pt = 
  match Bulgarian.decompose pt  with 
  None -> nonbulgarian_getter  ~with_anticipation pt
  |Some(pt2,adj) -> 
    (match nonbulgarian_getter  ~with_anticipation pt2 with
      None -> None 
      |Some bres -> Some(Bulk_result.extend_with bres adj)
    );;

let low_getter = Accumulator_with_optional_anticipator.get_from_low_hashtbl 
  ~with_anticipation:false ;;    
let access = bulgarian_getter ~with_anticipation:false ;;   

let descendants_for_hook pt hook = 
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


let try_hook_quickly ~with_anticipation pt hook =  
   (* 
   let nonbulgarian_descendants = descendants_for_hook pt hook in  
   let descendants = Image.image Bulgarian.decompose nonbulgarian_descendants in 
   *)
   let descendants = descendants_for_hook pt hook in  
   let descendants_with_their_images = Image.image (
      fun pt  -> (pt,bulgarian_getter ~with_anticipation pt)
    ) descendants in  
  let (failures,successes) = List.partition (
          fun (_,opt) -> opt = None
  ) descendants_with_their_images in 
  let missing_data = Image.image fst failures in 
  if missing_data <> [] then (missing_data,None) else 
  let args = Image.image (fun (_,opt)->Option.unpack opt) successes in 
  ([],apply_hook ~with_anticipation pt hook args) ;;  


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
   let hg_enhanced_getter = bulgarian_getter ~with_anticipation in     
   match hg_enhanced_getter pt with 
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
       let (missing_data,opt_res) =
      find_remote_stumbling_block_or_immediate_working_hook 
      ~with_anticipation:true pt in 
      match opt_res with 
       Some hook ->
           let res = compute_from_below ~with_anticipation:true pt hook in  
           let _ = Accumulator_with_optional_anticipator.add_to_low_hashtbl 
           ~with_anticipation:true pt res in 
           others
       | None -> 
         if missing_data = [] 
         then others 
         else missing_data @ (pt::others) 
      ;;      
         
let rec born_to_fail_for_recursive_computation walker=
  born_to_fail_for_recursive_computation
  (pusher_for_recursive_computation walker)  ;;     

let  needed_subcomputations_for_several_computations uples = 
  let _ = (  Accumulator_with_optional_anticipator.low_anticipator:=[]) in  
  try born_to_fail_for_recursive_computation uples with 
  Pusher_exn -> !(  Accumulator_with_optional_anticipator.low_anticipator) ;; 

let needed_subcomputations_for_single_computation pt = 
  needed_subcomputations_for_several_computations [pt] ;; 


let compute_recursively width breadth (n,scrappers) = 
  let uple = P(width,breadth,n,scrappers) in 
  let needed_carrier = needed_subcomputations_for_single_computation uple in 
  let answer = List.assoc_opt uple needed_carrier in 
  (answer,needed_carrier) 
;;  




let feel_new_line (width,breadth,scrappers) =
  let temp1 = Int_range.scale 
    (fun n->P(width,breadth,n,scrappers)) 1  (width+2*breadth) in 
  let temp2 = needed_subcomputations_for_several_computations temp1 in 
  let temp3 = Image.image (fun (P(w,b,n,s),_)->(w,b,s)) temp2 in 
  Ordered.sort t_order temp3 ;; 

let exhaust_new_line (width,breadth,scrappers) = 
    let temp1 = Int_range.scale 
      (fun n->P(width,breadth,n,scrappers)) 1  30 in 
    let carrier = needed_subcomputations_for_several_computations temp1 in 
    let temp2 = Image.image (fun pt-> 
      let mutilated_carrier = List.filter (
        fun p->fst(p)<>pt
      ) carrier in 
      let _ = ( Accumulator_with_optional_anticipator.low_anticipator :=mutilated_carrier) in 
      let (_,hook_opt) = find_remote_stumbling_block_or_immediate_working_hook ~with_anticipation:true pt in 
      (Point.size pt,hook_opt)
    ) temp1 in 
    let selector = (fun l->Option.filter_and_unpack  (fun (n,pair_opt)->match pair_opt with 
      None -> None |Some pair ->Some(n,pair)) l) in 
    let temp3 = selector temp2 in 
    let temp4 = Int_range.scale (fun n-> 
       let pt2 = P(width,breadth,n,scrappers) in 
       let _ = (  Accumulator_with_optional_anticipator.low_anticipator:=carrier) in 
      (n, bulgarian_getter ~with_anticipation:true pt2 ))  1 30  in 
    let temp5 = selector temp4 in 
    (temp3,temp5) ;;   


(*

#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;
let g1 = needed_subcomputations_for_single_computation (P(4,0,8,[])) ;;

*)



(*
let pt1 = P(1,4,6,[]) ;;       
let g1 = needed_subcomputations_for_single_computation (pt1) ;;
let g2 = needed_subcomputations_for_several_computations [pt1] ;;
let state0 = [pt1] ;; 
Accumulator_with_optional_anticipator.low_anticipator:=[] ;;
let ff = Memoized.small pusher_for_recursive_computation state0 ;;
let bad1 = pusher_for_recursive_computation state0 ;;
let (pt,others) = Listennou.ht state0 ;; 
let bad2 =
      find_remote_stumbling_block_or_immediate_working_hook 
      ~with_anticipation:true pt ;;
let hg_enhanced_getter = bulgarian_getter ~with_anticipation:true ;; 
let bad3 = hg_enhanced_getter pt ;; 
let bad4 = bulgarian_getter ~with_anticipation:true pt ;; 
let bad5 = Bulgarian.decompose pt ;; 

(* let pt= P(1,3,6,[]) ;; *)
let (old_width,old_breadth,n,scrappers) = Point.unveil pt ;;
let domain = concretize (n,scrappers) ;;
let bad6 = Bulgarian_for_nonparametrized_sets.decompose (old_width,old_breadth) domain ;; 
module Pri = Bulgarian_for_nonparametrized_sets.Private ;; 
let opt1 = Pri.find_meaningful_obstruction (old_width,old_breadth) domain ;;  
let (width,breadth) = Option.unpack opt1 ;; 
let bad2 = Pri.detach width domain ;; 
let bad3 = Pri.iterator_for_detachment (width,domain,[],List.rev domain) ;;

let x= 6 ;;
let bad4 = Pri.test_for_detachability width domain x ;; 
let test_for_detachability width domain x = 
  let idx_range = Int_range.range 1 (min (width)((x-1)/2))  in 
    List.for_all (fun w->not(i_is_included_in [x-2*w;x-w] domain)) idx_range ;;
      



let h1 = try_hook_quickly ~with_anticipation:false 
       (P(1,1,3,[])) Fork ;; 


*)


