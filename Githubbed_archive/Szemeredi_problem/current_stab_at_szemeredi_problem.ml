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

let insert_new n (old_constraints,extension) (C new_constraint)= 
  let remaining_constraint = i_setminus new_constraint extension in 
  if remaining_constraint = [] 
  then None 
  else 
  if List.exists (fun t->t>n) remaining_constraint 
  then Some (old_constraints)    
  else Some (merge_constraints [C new_constraint] old_constraints) ;;  
   

end ;;  


module Point = struct 
    
  let width (P(w,b,n,s)) = w ;;
  let breadth (P(w,b,n,s)) = b ;;
  let size (P(w,b,n,s)) = n ;;
  let scrappers (P(w,b,n,s)) = s ;;
  let unveil (P(w,b,n,s)) = (w,b,n,s) ;;
  
  let remaining_part_of_constraint pt extension (C new_constraint) = 
    let n = size pt in 
    let (below,above) = List.partition (fun t->t<=n) new_constraint in 
    if not(i_is_included_in above extension)
    then None 
    else if i_intersects below (scrappers pt) 
         then None 
         else Some (C below) ;;

  
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


let representatives_hashtbl = Hashtbl.create 50 ;;
let representatives_anticipator = ref [] ;; 

let get_representatives_opt ~with_anticipation pt =
    if not(with_anticipation)
    then  Hashtbl.find_opt representatives_hashtbl pt 
    else
        match List.assoc_opt pt (!representatives_anticipator) with 
        Some anticiped_answer -> Some anticiped_answer 
        | None -> Hashtbl.find_opt representatives_hashtbl pt  ;;

exception Get_representatives_exn of point ;;        

let get_representatives ~with_anticipation pt = 
    match get_representatives_opt ~with_anticipation pt  with 
     Some answer -> answer 
     | None -> raise (Get_representatives_exn(pt)) ;;

let add_to_representatives_hashtbl ~with_anticipation pt vaal=
  if not(with_anticipation)
  then   Hashtbl.replace representatives_hashtbl pt vaal
  else representatives_anticipator := (pt,vaal) :: (!representatives_anticipator)  ;;
  

let forced_data_hashtbl = Hashtbl.create 50 ;;
let forced_data_anticipator = ref [] ;; 

let get_forced_data_opt ~with_anticipation pt =
    if not(with_anticipation)
    then  Hashtbl.find_opt forced_data_hashtbl pt 
    else
        match List.assoc_opt pt (!forced_data_anticipator) with 
        Some anticiped_answer -> Some anticiped_answer 
        | None -> Hashtbl.find_opt forced_data_hashtbl pt  ;;


exception Get_forced_data_exn of point ;;        

let get_forced_data ~with_anticipation pt = 
    match get_forced_data_opt ~with_anticipation pt  with 
      Some answer -> answer 
    | None -> raise (Get_forced_data_exn(pt)) ;;
        

let add_to_forced_data_hashtbl ~with_anticipation pt vaal=
  if not(with_anticipation)
  then   Hashtbl.replace forced_data_hashtbl pt vaal
  else forced_data_anticipator := (pt,vaal) :: (!forced_data_anticipator)  ;;
  

end ;;   



(* dummy and empty for now, to be filled later *)  
let test_forced_data_for_possible_obstruction new_constraints () = false;;



exception Test_for_possible_refinement_exn of point ;;

let test_for_possible_refinement ~with_anticipation pt new_constraints= 
   if List.exists (Constraint.satisfied_by_individual new_constraints) 
       (Accumulator_with_optional_anticipator.get_representatives ~with_anticipation pt) 
   then true 
   else   
   if test_forced_data_for_possible_obstruction new_constraints 
    (Accumulator_with_optional_anticipator.get_forced_data ~with_anticipation pt)
   then false 
   else raise (Test_for_possible_refinement_exn(pt)) ;; 

let common_length_for_bare_point ~with_anticipation pt = 
   List.length (List.hd (Accumulator_with_optional_anticipator.get_representatives ~with_anticipation pt)) ;; 

module Qualified_point = struct 

let extend_with qp extension =  
  match qp with 
  Q(pt,old_constraints,extension2) -> 
  Q(pt,old_constraints,i_merge extension extension2)   ;;

end ;;  

module Sycomore_list = struct 
  
      let get_representatives ~with_anticipation = function 
         Singleton(l) -> [l]
         | Breakpoint_with_extensions(Q(pt,_,extension)) -> Image.image (i_merge extension) 
            (Accumulator_with_optional_anticipator.get_representatives ~with_anticipation pt)  ;;       
      
      
      let refinement_opt ~with_anticipation new_constraints = function 
        Singleton(l) ->  if Constraint.satisfied_by_individual new_constraints l 
                       then Some(Singleton l)
                       else None 
        | Breakpoint_with_extensions(Q(pt,old_constraints,extension)) ->
          let cleaned_constraints = Option.filter_and_unpack (
            Point.remaining_part_of_constraint pt extension 
          )  new_constraints in 
          let final_constraints = Constraint.merge_constraints cleaned_constraints old_constraints in 
          if test_for_possible_refinement ~with_anticipation pt final_constraints 
          then  Some(Breakpoint_with_extensions(Q(pt,final_constraints,extension)))  
          else  None   ;;
      
        exception Remove_fixed_part_on_all of sycomore_list * (int list) ;; 
      
        let remove_fixed_part_on_all to_be_removed sycom_l= match sycom_l with 
          Singleton(l) -> Singleton(i_setminus l to_be_removed)
          | Breakpoint_with_extensions(Q(pt,old_constraints,extension)) -> 
             let n =  Point.size pt in 
             if List.exists (fun t->t<=n) to_be_removed 
             then raise(Remove_fixed_part_on_all(sycom_l,to_be_removed))
             else Breakpoint_with_extensions(Q(pt,old_constraints,extension)) ;;         
      
        
      
        let common_length ~with_anticipation = function 
        Singleton(l) -> List.length l
        | Breakpoint_with_extensions(Q(pt,old_constraints,extension)) -> 
           (common_length_for_bare_point ~with_anticipation pt)+List.length extension   ;;
      
        let extend_with sycom extension = match sycom with 
        Singleton(l) -> Singleton(i_merge l extension)
        | Breakpoint_with_extensions(Q(pt,old_constraints,extension2)) -> 
          Breakpoint_with_extensions(Q(pt,old_constraints,i_merge extension extension2))   ;;
      
      
         let enforce_boundary_increment n = function 
              (Singleton l) -> Singleton(l@[n])
              | Breakpoint_with_extensions(Q(pt2,old_constraints,extension))  -> 
                Breakpoint_with_extensions(Q(pt2,old_constraints,i_insert n extension)) ;;
      
         let apply_fork pt ll = Some(Breakpoint_with_extensions(Q(pt,[],[]))) ;; 
      
      
end ;;  

module Forced_data = struct 

(* it is assumed that compatibility has already been checked *)   
let extend_with (FD(offshoots,qpoints)) extension =
  FD(Image.image (i_merge extension) offshoots,
  Image.image (fun qpoint->Qualified_point.extend_with qpoint extension) qpoints
  ) ;;  

(*  
let refinement_opt extra_constraints (FD(offshoots,qpoints)) =
*)    

end ;;

module Bulk_result = struct 

let extend_with (BR(sycom)) extension =
    BR(Sycomore_list.extend_with sycom extension);;

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
  
let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;
  
let apply_passive_repeat_on_bulk_result ~with_anticipation pt (BR(sycom)) =
  let (width,b,_,_) = Point.unveil pt in 
   match Sycomore_list.refinement_opt ~with_anticipation [C[b;b+width;b+2*width]] sycom with 
   None -> None 
   |Some new_sycom -> Some(BR(new_sycom)) ;;  

let apply_boundary_increment_on_bulk_result ~with_anticipation pt (BR(sycom)) = 
  let (width,breadth,n,_) = Point.unveil pt in 
  let new_constraints = Constraint.extra_constraints_from_boundary_increment width breadth n in 
  match Sycomore_list.refinement_opt ~with_anticipation new_constraints sycom with 
   None -> None 
  |Some new_sycom -> Some(BR(Sycomore_list.enforce_boundary_increment n new_sycom)) ;;

let apply_fork_on_bulk_result pt ll = 
     match Sycomore_list.apply_fork pt (Image.image (fun (BR syc)->syc) ll) with 
      None -> None 
      | Some new_sycom -> Some(BR(new_sycom)) ;;  

let apply_hook_on_bulk_result ~with_anticipation pt hook ll =  
    match hook with 
   Passive_repeat -> apply_passive_repeat_on_bulk_result ~with_anticipation pt (List.hd ll)
 | Boundary_increment -> apply_boundary_increment_on_bulk_result ~with_anticipation pt (List.hd ll)
 | Fork ->  apply_fork_on_bulk_result pt ll 
 | Jump -> Some(List.hd ll);;



module Hungarian = struct 

module Breadth_reduction = struct 

let test_for_automatic_treatment width scrappers b = 
  List.exists(fun t->List.mem t scrappers) 
  [b;b+width;b+2*width] ;;

let rec effective_breadth (width,scrappers,b) =
   if (test_for_automatic_treatment width scrappers b)&&(b>0) 
   then effective_breadth (width,scrappers,b-1)
   else b ;;

end ;;  

let normalized_adjust adj =
    if adj = [] 
    then Leave_unchanged 
    else Adjust adj ;;


let short_adjust old_result adjustment =
      match adjustment with 
      Leave_unchanged -> old_result 
      |Adjust extra -> Sycomore_list.remove_fixed_part_on_all extra old_result;;
  
let adjust result_opt adjustment=
    match result_opt with 
    None -> None 
    | Some old_result ->  
    Some(short_adjust old_result adjustment) ;;   

let merge_adjustments adj1 adj2 =
  (
    match adj1 with 
      Leave_unchanged -> adj2 
     |Adjust content1 -> 
      (
      match adj2 with 
      Leave_unchanged -> adj1 
     |Adjust content2 -> Adjust(i_merge content1 content2) 
      
  )
  ) ;;

let first_step_in_decomposition (width,breadth,scrappers) =
     let bound = breadth+2*width in 
     let (below,above) = (if width<>1
      then (scrappers,[])
      else List.partition (fun t->t<=bound) scrappers) in 
     ((width,Breadth_reduction.effective_breadth (width,scrappers,breadth),below),
       normalized_adjust above) ;;     

let pusher_for_decomposition (triple,adj)= 
      let (triple2,adj2) = first_step_in_decomposition triple in 
      (triple2,merge_adjustments adj adj2) ;;

let rec iterator_for_decomposition  pair =
   let next_pair = pusher_for_decomposition pair in 
   if next_pair = pair 
   then pair 
   else iterator_for_decomposition next_pair ;;

let decompose triple =  iterator_for_decomposition (triple,Leave_unchanged) ;;  

(* Example : decompose (1,18,[8;11;14;17;20]);; *)

end ;;  


  
let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;       

let nonhungarian_getter  ~with_anticipation pt = 
let (width,breadth,n,scrappers) = Point.unveil pt in 
let z = concretize (n,scrappers) in 
if ((width,breadth)=(1,0))||(test_for_admissiblity width breadth z) 
then Some (BR(Singleton z)) 
else 
match Hashtbl.find_opt rose_hashtbl (width,breadth) with 
Some summary -> Some (Parametrized.eval_foscras summary scrappers n)
| None ->  
 (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.eval_fos summary n)
 | None -> Accumulator_with_optional_anticipator.get_from_low_hashtbl ~with_anticipation pt) ;;   

let hungarian_adjust_bulk_result bres_opt adj =
  match bres_opt with 
   None -> None 
   |Some (BR(sycom)) -> Some(BR(Hungarian.short_adjust sycom adj)) ;;


   

let hungarian_getter ~with_anticipation pt = 
  let (width,breadth,n,scrappers) = Point.unveil pt in  
  let ((width2,breadth2,scrappers2),adj) = 
    Hungarian.decompose (width,breadth,scrappers) in 
  let pt2 = P(width2,breadth2,n,scrappers2) in   
  let res_opt = nonhungarian_getter  ~with_anticipation pt2 in  
    hungarian_adjust_bulk_result res_opt adj;;

let low_getter = Accumulator_with_optional_anticipator.get_from_low_hashtbl 
  ~with_anticipation:false ;;    
let access = hungarian_getter ~with_anticipation:false ;;   

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
   let nh_enhanced_getter = nonhungarian_getter ~with_anticipation in 
   let descendants = descendants_for_hook pt hook in  
   let hungarian_descendants = Image.image (
      fun pt1  ->
        let (w,b,m,s) = Point.unveil pt1 in  
        let ((w2,b2,s2),adj) =  Hungarian.decompose (w,b,s) in 
        (P(w2,b2,m,s2),adj)
    ) descendants in 
   let temp1 = Image.image (fun (pt3,adj)->
      (pt3,hungarian_adjust_bulk_result (nh_enhanced_getter pt3) adj)
  ) hungarian_descendants in   
  let (failures,successes) = List.partition (
          fun (_,opt) -> opt = None
  ) temp1 in 
  let missing_data = Image.image fst failures in 
  if missing_data <> [] then (missing_data,None) else 
  let args = Image.image (fun (_,opt)->Option.unpack opt) successes in 
  ([],apply_hook_on_bulk_result ~with_anticipation pt hook args) ;;  


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
   let hg_enhanced_getter = hungarian_getter ~with_anticipation in     
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
      (n, hungarian_getter ~with_anticipation:true pt2 ))  1 30  in 
    let temp5 = selector temp4 in 
    (temp3,temp5) ;;   


(*

#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;
let g1 = needed_subcomputations_for_single_computation (P(4,0,8,[])) ;;

*)