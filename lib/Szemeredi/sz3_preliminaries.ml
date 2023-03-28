(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type breadth = Sz3_types.breadth = B of int ;;
type size = Sz3_types.size = S of int ;;

type point = Sz3_types.point 
  =Empty_point | P of int * int list * breadth * size ;;
type constraint_t = Sz3_types.constraint_t = C of int list ;; 
type extension_data =  int list ;;
type qualified_point = Sz3_types.qualified_point = Q of point * constraint_t list * extension_data ;;
type solution = int list ;;
type mold = Sz3_types.mold = M of solution list * qualified_point list ;;
type superficial_result = Sz3_types.superficial_result = 
     Atomic
   | Decomposable of point * extension_data
   | Contraction of point * constraint_t
   | Fork of (point * extension_data) list ;; 
type bulk_result = Sz3_types.bulk_result = BR of superficial_result * mold ;; 
type half = Sz3_types.half = Lower_half | Upper_half ;;
type kind_of_missing_part = Sz3_types.kind_of_missing_part = KMP of int ;; 
type index_of_missing_data = Sz3_types.index_of_missing_data = IMD of int ;;
type visualization_result = Sz3_types.visualization_result = 
   VR1 of ((breadth * size) * superficial_result) list 
  |VR2 of ((breadth * size) * solution list) list 
  |VR3 of ((breadth * size) * int) list 
  |VR4 of ((breadth * size) * point) list 
  |VR5 of ((breadth * size) * constraint_t list) list 
  |VR6 of ((breadth * size) * extension_data) list ;; 
type check_entry = Sz3_types.check_entry = 
  CE1 of ( breadth -> size -> superficial_result) 
 |CE2 of ( breadth -> size -> solution list) 
 |CE3 of ( breadth -> size -> int) 
 |CE4 of ( breadth -> size -> point) 
 |CE5 of ( breadth -> size -> constraint_t list) 
 |CE6 of ( breadth -> size -> extension_data);;  
type check_result = Sz3_types.check_result =
  CR1 of ((breadth * size) * superficial_result * superficial_result) list 
 |CR2 of ((breadth * size) * solution list * solution list) list 
 |CR3 of ((breadth * size) * int * int) list 
 |CR4 of ((breadth * size) * point * point) list 
 |CR5 of ((breadth * size) * constraint_t list * constraint_t list) list 
 |CR6 of ((breadth * size) * extension_data * extension_data) list ;;    
     


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


module Parameter_pair_for_obstruction = struct 

  let predecessor max_in_set (width,breadth) = 
    if breadth < 1 
    then (if width < 2 then None else Some(width-1,max_in_set-2*(width-1)) )  
    else (Some(width,breadth-1)) ;;
    
  let check_for_meaningful_obstruction (width,breadth) domain =
     if breadth < 1 
     then false 
     else Ordered.is_included_in 
           Total_ordering.for_integers 
         [breadth;breadth+width;breadth+2*width] domain ;;  
  
end ;;  

module Finite_int_set = struct 

  let of_pair ( S n,scrappers) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let to_pair domain =
       if domain = [] then (S 0,[]) else 
       let n = List.hd(List.rev domain) in 
       (S n,i_setminus (Int_range.range 1 n) domain) ;;   
  
end ;;    

module Point = struct 
  
   exception Empty_point_cannot_be_unveiled ;; 
  let unveil =function 
   Empty_point -> raise(Empty_point_cannot_be_unveiled) 
   |P(w,s,b,n) ->  (w,s,b,n) ;; 
  let width p = let (w,_,_,_) = unveil p in w ;; 
  let scrappers p = let (_,s,_,_) = unveil p in s ;;   
  let breadth p = let (_,_,b,_) = unveil p in b ;;  
  let size p = let (_,_,n,_) = unveil p in n ;;   
  let enumerate_supporting_set = function
     Empty_point -> []
    |P(_w,s,_b, n) -> Finite_int_set.of_pair (n,s) ;; 
  let is_in_upper_half p= 
    let (w,_,B(b),S(n)) = unveil p in 
    n >= b + 2*w ;; 


end ;;  

  
module Simplest_reduction = struct 

  module For_nonparametrized_sets = struct 

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


  let decompose pt =
      let (old_width,scrappers,B old_breadth,n) = Point.unveil pt in 
      let domain = Finite_int_set.of_pair (n,scrappers) in 
      match For_nonparametrized_sets.decompose (old_width,old_breadth) domain with
      None -> (Empty_point,domain)
    | (Some((new_width,new_breadth),(new_domain,adjustment))) -> 
       let (new_n,new_scrappers) = Finite_int_set.to_pair new_domain in 
        (P(new_width,new_scrappers,B new_breadth,new_n),adjustment);;
    
(*
   
let check1 = (decompose (P(1,4,6,[])) =  (P (1, 4, 6, []), [])) ;;
let check2 = (decompose (P(1,3,6,[])) =  (P (1, 3, 5, []), [6])) ;;

*)

end ;;  


module Warehouse = struct 
  
  exception Bad_kmp_index of int ;; 

  let access_named_hashtbl (error_msg,hashtbl) pt = 
    let (width,scrappers,breadth,size) = Point.unveil pt in 
    match Hashtbl.find_opt hashtbl (width,scrappers) with 
    None -> (None,Some error_msg) 
    | Some f -> (Some(f breadth size),None) ;; 

  let index_for_missing_data = ref (IMD(0)) ;; 

  let access_parametrized_named_hashtbl (error_msg,hashtbl) k pt = 
    let (width,scrappers,breadth,size) = Point.unveil pt in 
    match Hashtbl.find_opt hashtbl (width,scrappers,k) with 
    None -> let _ = (index_for_missing_data:=k) in 
             (None,Some error_msg) 
    | Some f -> (Some(f breadth size),None) ;; 
  
    
  let superficial_result_lower_half_idx = KMP 1 ;;  
  let hashtbl1 = ((Hashtbl.create 50) : (int * int list, breadth -> size -> superficial_result) Hashtbl.t) ;;
  let pair_for_superficial_result_lower_half = (superficial_result_lower_half_idx,hashtbl1) ;;
  let superficial_result_upper_half_idx = KMP 2 ;;  
  let hashtbl2 = ((Hashtbl.create 50) : (int * int list, breadth -> size -> superficial_result) Hashtbl.t) ;;
  let pair_for_superficial_result_upper_half = (superficial_result_upper_half_idx,hashtbl2) ;;
  let solution_list_lower_half_idx = KMP 3 ;; 
  let hashtbl3 = ((Hashtbl.create 50): (int * int list, breadth -> size -> solution list) Hashtbl.t) ;;
  let pair_for_solution_list_lower_half = (solution_list_lower_half_idx,hashtbl3) ;;
  let solution_list_upper_half_idx = KMP 4 ;; 
  let hashtbl4 = ((Hashtbl.create 50): (int * int list, breadth -> size -> solution list) Hashtbl.t) ;;
  let pair_for_solution_list_upper_half = (solution_list_upper_half_idx,hashtbl4) ;; 
  let qpl_length_lower_half_idx = KMP 5 ;;  
  let hashtbl5 = ((Hashtbl.create 50): (int * int list, breadth -> size -> int) Hashtbl.t) ;;
  let pair_for_qpl_length_lower_half = (qpl_length_lower_half_idx,hashtbl5) ;;
  let qpl_length_upper_half_idx = KMP 6 ;;  
  let hashtbl6 = ((Hashtbl.create 50): (int * int list, breadth -> size -> int) Hashtbl.t) ;;
  let pair_for_qpl_length_upper_half = (qpl_length_upper_half_idx,hashtbl6) ;;
  let qpe_core_lower_half_idx = KMP 7 ;;  
  let hashtbl7 = ((Hashtbl.create 50): (int * int list * index_of_missing_data, breadth -> size -> point) Hashtbl.t) ;;
  let pair_for_qpe_core_lower_half = (qpe_core_lower_half_idx,hashtbl7) ;;
  let qpe_core_upper_half_idx = KMP 8 ;;  
  let hashtbl8 = ((Hashtbl.create 50): (int * int list * index_of_missing_data, breadth -> size -> point) Hashtbl.t) ;;
  let pair_for_qpe_core_upper_half = (qpe_core_upper_half_idx,hashtbl8) ;; 
  let qpe_constraints_lower_half_idx = KMP 9 ;;  
  let hashtbl9 = ((Hashtbl.create 50): (int * int list * index_of_missing_data, breadth -> size -> constraint_t list) Hashtbl.t) ;;
  let pair_for_qpe_constraints_lower_half = (qpe_constraints_lower_half_idx,hashtbl9) ;;
  let qpe_constraints_upper_half_idx = KMP 10 ;;  
  let hashtbl10 = ((Hashtbl.create 50): (int * int list * index_of_missing_data, breadth -> size -> constraint_t list) Hashtbl.t) ;;
  let pair_for_qpe_constraints_upper_half = (qpe_constraints_upper_half_idx,hashtbl10) ;; 
  let qpe_extension_lower_half_idx = KMP 11 ;;  
  let hashtbl11 = ((Hashtbl.create 50): (int * int list * index_of_missing_data, breadth -> size -> int list) Hashtbl.t) ;;
  let pair_for_qpe_extension_lower_half = (qpe_extension_lower_half_idx,hashtbl11) ;;
  let qpe_extension_upper_half_idx = KMP 12 ;;  
  let hashtbl12 = ((Hashtbl.create 50): (int * int list * index_of_missing_data, breadth -> size -> int list) Hashtbl.t) ;;
  let pair_for_qpe_extension_upper_half = (qpe_extension_upper_half_idx,hashtbl12) ;; 

  let read_missing_part (KMP i) = 
      match List.assoc_opt (KMP i)  
      [
        (fst pair_for_superficial_result_lower_half),"superficial_result_lower_half";
        (fst pair_for_superficial_result_upper_half),"superficial_result_upper_half";
        (fst pair_for_solution_list_lower_half),"solution_list_lower_half";
        (fst pair_for_solution_list_upper_half),"solution_list_upper_half";
        (fst pair_for_qpl_length_lower_half),"qpl_length_lower_half";
        (fst pair_for_qpl_length_upper_half),"qpl_length_upper_half";
        (fst pair_for_qpe_core_lower_half),"qpe_core_lower_half";
        (fst pair_for_qpe_core_upper_half),"qpe_core_upper_half";
        (fst pair_for_qpe_constraints_lower_half),"qpe_constraints_lower_half";
        (fst pair_for_qpe_constraints_upper_half),"qpe_constraints_upper_half";
        (fst pair_for_qpe_extension_lower_half),"qpe_extension_lower_half";
        (fst pair_for_qpe_extension_upper_half),"qpe_extension_upper_half";
      ] with 
      Some answer -> answer 
      |None -> raise(Bad_kmp_index(i)) ;; 

  let qualified_point_core half k pt = match half with 
    Lower_half -> access_parametrized_named_hashtbl pair_for_qpe_core_lower_half k pt 
   |Upper_half -> access_parametrized_named_hashtbl pair_for_qpe_core_upper_half k pt;;

  let qualified_point_constraints half k pt = match half with 
   Lower_half -> access_parametrized_named_hashtbl pair_for_qpe_constraints_lower_half k pt 
  |Upper_half -> access_parametrized_named_hashtbl pair_for_qpe_constraints_upper_half k pt;;


  let qualified_point_extension half k pt = match half with 
    Lower_half -> access_parametrized_named_hashtbl pair_for_qpe_extension_lower_half k pt 
   |Upper_half -> access_parametrized_named_hashtbl pair_for_qpe_extension_upper_half k pt;;



  let qpl_length half pt = match half with 
    Lower_half -> access_named_hashtbl pair_for_qpl_length_lower_half pt 
   |Upper_half -> access_named_hashtbl pair_for_qpl_length_upper_half pt;;

  let qualified_point_element half k pt = 
    let (good_opt1,bad_opt1) = qualified_point_core half k pt in 
    if bad_opt1<>None then (None,bad_opt1) else 
    let (good_opt2,bad_opt2) = qualified_point_constraints half k pt in 
    if bad_opt2<>None then (None,bad_opt2) else   
    let (good_opt3,bad_opt3) = qualified_point_extension half k pt in 
    if bad_opt3<>None then (None,bad_opt3) else     
    let core_r = Option.get good_opt1 
    and constraints_r = Option.get good_opt2 
    and extension_r = Option.get good_opt3 in 
    (Some(Q(core_r,constraints_r,extension_r)),None) ;;   
  
  let superficial_result half pt = match half with 
     Lower_half -> access_named_hashtbl pair_for_superficial_result_lower_half pt 
    |Upper_half -> access_named_hashtbl pair_for_superficial_result_upper_half pt;;
     
  let solution_list half pt = match half with 
    Lower_half -> access_named_hashtbl pair_for_solution_list_lower_half pt 
   |Upper_half -> access_named_hashtbl pair_for_solution_list_upper_half pt;;
  
   
  let qualified_point_list half pt =
    let (good_opt1,bad_opt1) =qpl_length half pt in 
    if bad_opt1<>None then (None,bad_opt1) else 
  let length_r = Option.get good_opt1 in 
  let eltwise_results = Int_range.scale (
          fun k-> qualified_point_element half (IMD k) pt
  )  1 length_r in  
  let bad_ones = List.filter (
        fun (good_opt,_bad_opt) -> good_opt = None
  ) eltwise_results in 
  if bad_ones <> [] 
  then (None,snd(List.hd bad_ones)) 
  else 
  let final_result = Image.image (fun (good_opt,_bad_opt) ->Option.get good_opt) eltwise_results in 
  (Some final_result,None) ;;  


  let bulk_result half pt = 
     let (good_opt1,bad_opt1) = superficial_result half pt in 
     if bad_opt1<>None then (None,bad_opt1) else 
     let (good_opt2,bad_opt2) = solution_list half pt in 
     if bad_opt2<>None then (None,bad_opt2) else  
     let (good_opt3,bad_opt3) = qualified_point_list half pt in 
     if bad_opt3<>None then (None,bad_opt3) else  
     let superficial_result_r = Option.get good_opt1 
     and solution_list_r = Option.get good_opt2 
     and qualified_point_list_r = Option.get good_opt3  in     
     (Some(BR(superficial_result_r,M(solution_list_r,qualified_point_list_r))),None)
   ;;   


  let nonhalved_bulk_result pt = 
    if Point.is_in_upper_half pt 
    then bulk_result Upper_half pt 
    else bulk_result Lower_half pt ;;    

  let try_precomputed_results pt = fst(nonhalved_bulk_result pt) ;;

end ;;  

module Fill_Warehouse = struct 

let simplest_example n = List.filter (fun j->(j mod 3)<>0) (Int_range.range 1 n) ;; 
let simplest_list n = [simplest_example n] ;; 

let f_1_empty_set_superficial_result_lower_half (B _b) (S n) =
    match List.assoc_opt n 
       [1,Atomic;
        2,Atomic;
        3,Fork [(Empty_point, [2; 3]); (Empty_point, [1; 3]); (Empty_point, [1; 2])]]  with    
      Some answer -> answer 
   | None ->
    (match (n mod 3) with 
    0 -> Fork([( P(1,[],B(n-5),S(n-3)),[n-1;n] );
               ( P(1,[],B(n-4),S(n-2)),[n] );
               ( P(1,[],B(n-3),S(n-1)),[] )])
   |1-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] )  
   |2-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] ) 
   |_-> failwith("bad reminder by 3")) ;;       
  
(*
  
  Verify.check 
   (fst(Warehouse.pair_for_superficial_result_lower_half))
   (1,[],IMD 0) (CE1 f_1_empty_set_superficial_result_lower_half) ;; 
  
*)

Hashtbl.add 
  (snd(Warehouse.pair_for_superficial_result_lower_half)) (1,[]) 
    f_1_empty_set_superficial_result_lower_half;;


end ;;   

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
  
  let insert_new domain (old_constraints,extension) (C new_constraint)= 
    let remaining_constraint = i_setminus new_constraint extension in 
    if remaining_constraint = [] 
    then None 
    else 
    if (i_setminus remaining_constraint domain)<>[] 
    then Some (old_constraints)    
    else Some (merge_constraints [C remaining_constraint] old_constraints) ;;  
     
  let insert_several  domain (old_constraints,extension) new_constraints =
     let rec tempf = (
        fun (constraints_walker,to_be_treated) ->
           match to_be_treated with 
           [] -> Some constraints_walker 
           | new_constraint :: others ->  
          (match  insert_new domain (constraints_walker,extension) new_constraint with    
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
      (match Constraint.insert_several (Point.enumerate_supporting_set pt) (old_constraints,extension) new_constraints 
      with
        None -> None 
       |(Some final_constraints) ->  Some((Q(pt,final_constraints,extension)))) ;; 
    
    
end ;;  



module Mold = struct 

  type t = Sz3_types.mold = M of solution list * qualified_point list ;;

  (* it is assumed that compatibility has already been checked *)   
  let extend_with (M(reps,qpoints)) extension =
    M(Image.image (i_merge extension) reps,
    Image.image (fun qpoint->Qualified_point.extend_with qpoint extension) qpoints
    ) ;;  
  
  let insert_several_constraints extra_constraints (M(reps,qpoints)) = 
    M(List.filter (Constraint.satisfied_by_individual extra_constraints) reps,
       List.filter_map (
        Qualified_point.insert_several_constraints extra_constraints
       ) qpoints) ;; 
    
  exception Insert_several_constraints_carefully_exn of constraint_t list * t ;;
  
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

  type t = Sz3_types.bulk_result = BR of superficial_result * mold ;;

  let atomic_case pt = BR (Atomic,M([Point.enumerate_supporting_set pt],[])) ;; 
  
  let is_not_atomic (BR(sr,_)) = sr <> Atomic ;; 
  
  let superficial_part (BR(sr,_)) = sr ;; 
  let solution_list (BR(_,md)) = let (M(reps,_qpoints)) = md in reps ;; 
  let qualified_points (BR(_,md)) = let (M(_reps,qpoints)) = md in qpoints ;; 
  let mold (BR(_,md)) = md ;; 
  
  let extend_with pt (BR(old_sr,mold)) extension = 
   let new_sr = (if extension <> []
   then Decomposable(pt,extension)
   else old_sr) in
   BR(new_sr,Mold.extend_with mold extension);;
  
  let extend_with_opt pt bres_opt extension = match bres_opt with 
        None -> None 
        |Some bres -> Some (extend_with pt bres extension) ;;    
  
  let impose_one_more_constraint_opt pt cstr (BR(_sr,mold)) =
      match Mold.insert_several_constraints_carefully [cstr] mold with 
       None -> None
      | Some new_mold -> Some(BR(Contraction(pt,cstr),new_mold)) ;;
       
  
end ;;      
  
module Untamed = struct 
  
  let cil_order = ((fun (C x) (C y)->il_order x y) : constraint_t Total_ordering_t.t) ;;

  let test_for_admissibility_up_to_max_with max_width z =
    if max_width<1 then true else 
    Sz_preliminaries.test_for_admissibility (Sz_max_width_t.MW (max_width)) z ;;
  
  let test_for_admissiblity width breadth z =
     (test_for_admissibility_up_to_max_with (width-1) z)
     &&
     (List.for_all (fun t->
      not(i_is_included_in [t;t+width;t+2*width] z)) (Int_range.range 1 breadth))  ;;
  
  let remove_one_element (S n,scrappers) k=
    let new_scrappers = i_insert k scrappers in 
    if k <> n then (S n,new_scrappers) else 
    let new_z =  Finite_int_set.of_pair (S n,new_scrappers) in 
    let new_max = List.hd(List.rev new_z) in 
    (S new_max,List.filter (fun t->t<new_max) new_scrappers) ;;
  
  
  (*
  
  remove_one_element (10,[3;7;8;9]) 10 ;;
  
  *)
  
  
  
  
  
  let low_hashtbl = Hashtbl.create 50 ;;
    
  let access_opt  pt = 
   let (pt2,adj) = Simplest_reduction.decompose pt in 
   if pt2 = Empty_point 
   then Some (Bulk_result.atomic_case pt)
   else
   let pre_res=Warehouse.try_precomputed_results pt2 in 
   Bulk_result.extend_with_opt pt2 pre_res adj ;;
     
  let superificial_result_in_jump_case  pt_after_jump =
    let (width,scrappers,B _breadth, S n) = Point.unveil pt_after_jump in 
    let pt_before_jump = P(width-1,scrappers,B(n-2*(width-1)),S n) in  
    let (pt2,adj2) = Simplest_reduction.decompose pt_before_jump in 
    ([],Some(Decomposable(pt2,adj2))) ;; 
      
  let access_with_helper_opt pt helper =
      match List.assoc_opt pt helper with 
      Some answer -> Some answer
      | None ->  access_opt pt ;;
  
  let compute_superficial_result_partially pt helper =  
    if pt = Empty_point then ([],Some Atomic) else
    let (width,_scrappers,B breadth,_n) = Point.unveil pt in 
    let (pt2,adj2) = Simplest_reduction.decompose pt in 
    if ((width,breadth)=(1,0))||(pt2=Empty_point)
    then ([],Some Atomic)
    else 
    if adj2<>[]
    then ([],Some(Decomposable(pt2,adj2)))
    else     
    if breadth = 0
    then superificial_result_in_jump_case pt   
    else
    let (width2,scrappers2,B breadth2,n2) = Point.unveil pt2 in   
    let _ = assert(breadth2>0) in 
    let front_constraint = C [breadth2;breadth2+width2;breadth2+2*width2] 
    and preceding_point = P(width2,scrappers2,B(breadth2-1),n2) in 
    match access_with_helper_opt  preceding_point helper with 
      None -> ([preceding_point],None)
     |Some bres ->
         (match Bulk_result.impose_one_more_constraint_opt preceding_point front_constraint bres  with 
         None -> let tooths = Int_range.scale (fun k->
                  let (m,scr) = remove_one_element  (n2,scrappers2)  (breadth2+k*width2) in 
                  let pt3 = P(width2,scr,B(breadth2-1),m) in 
                  Simplest_reduction.decompose(pt3) 
                 ) 0 2  in 
                ([],Some(Fork tooths))
        |Some _bres2 -> ([],Some(Contraction(preceding_point,front_constraint)))) ;; 
  
  let fork_case_in_bulk_result_computation old_f cases helper = 
        let (last_pt,last_adj) = List.nth cases 2 in 
        let partial_res5 = old_f last_pt helper in 
        match snd partial_res5 with 
        None -> (fst partial_res5,None) 
       |Some br5 -> 
         let (BR(_,M(reps,_))) = Bulk_result.extend_with last_pt br5 last_adj in 
         let new_mold = M(reps,Image.image (
          fun (pt6,adj6)-> Q(pt6,[],adj6)
        ) cases) in 
        ([],Some (BR(Fork cases,new_mold))) ;; 
  
  exception Bad_contraction of point * constraint_t ;; 
  
  let rec compute_bulk_result_partially pt helper=  
    if pt = Empty_point then ([],Some(Bulk_result.atomic_case pt)) else 
     let partial_res1 = compute_superficial_result_partially pt helper in 
     match snd partial_res1 with 
      None -> (fst partial_res1,None) 
     |Some sr ->(match sr with 
       Atomic -> ([],Some(Bulk_result.atomic_case pt)) 
     | Decomposable(pt2,adj2) -> 
         let partial_res2 = compute_bulk_result_partially pt2 helper in 
         (
          match snd partial_res2 with 
          None -> (fst partial_res2,None) 
         |Some br2 -> ([],Some (Bulk_result.extend_with pt2 br2 adj2))
         )
     | Contraction (pt5,cstr) ->
      let partial_res4 = compute_bulk_result_partially pt5 helper in 
      (
       match snd partial_res4 with 
       None -> (fst partial_res4,None) 
      |Some br4 -> 
        match Bulk_result.impose_one_more_constraint_opt pt5 cstr br4 with 
          None -> raise(Bad_contraction(pt5,cstr))
          |Some new_br4 ->([],Some new_br4)
      ) 
     | Fork cases ->
      fork_case_in_bulk_result_computation compute_bulk_result_partially cases helper
     ) ;; 
  
     
  let add_if_necessary (a,b) assoc_list = 
    if List.mem_assoc a assoc_list 
    then assoc_list 
    else (a,b) :: assoc_list ;;   
  
  exception Pusher_stop ;;
  
  let pusher_for_needed_subcomputations  
     (treated,to_be_treated) = match to_be_treated with 
           [] -> raise Pusher_stop
           | pt1 :: other_pts ->
             let partial_res1 = compute_bulk_result_partially pt1 treated in 
             match snd partial_res1 with 
              None -> (treated,(fst partial_res1)@to_be_treated)
             |Some answer -> (add_if_necessary (pt1,answer) treated,other_pts) ;;
  
  let rec needed_subcomputations walker =
      let (treated,to_be_treated) = walker in 
      let subcomps =( match to_be_treated with 
      [] -> treated
      | _ -> needed_subcomputations (pusher_for_needed_subcomputations walker) ) in 
      let new_subcomps = List.filter (
      fun (_,bres) -> Bulk_result.is_not_atomic bres
     ) subcomps in 
     let _ = List.iter (fun (pt2,bres)->
      Hashtbl.replace low_hashtbl pt2 bres ) new_subcomps in 
      subcomps ;;
  
  let compute_bulk_result pt =
     let subcomps = needed_subcomputations ([],[pt]) in 
     List.assoc pt subcomps ;;   
    
end ;;  


module Pretty_printer = struct 

module Private = struct 

let for_int_list l = "["^(String.concat ";" (Image.image string_of_int l))^"]" ;;

let for_constraint (C l) = "C"^(for_int_list l);;

let for_solution_list l = "["^(String.concat ";" (Image.image for_int_list l))^"]" ;;

let for_point = function 
    Empty_point -> "Empty_point" 
  | P(w,s,B b,S n) -> "P("^(string_of_int w)^","^(for_int_list s)^",B("^(string_of_int b)^"),S("^(string_of_int n)^"))";;

let for_fork_element 
(pt,ext_data) = "( "^(for_point pt)^","^(for_int_list ext_data)^" )" ;; 
  

let for_superficial_result = function
    Atomic -> "Atomic"
  | Decomposable (pt,ext_data) -> "Decomposable( "^(for_point pt)^","^(for_int_list ext_data)^" )"
  | Contraction (pt,cstr) -> "Contraction( "^(for_point pt)^","^(for_constraint cstr)^" )"
  | Fork (l) -> "Fork(["^(String.concat ";" (Image.image for_fork_element l))^"])" ;;


let for_vr1_element ((B b,S n),sr) = 
    "((B "^(string_of_int b)^",S "^(string_of_int n)^"),"^(for_superficial_result sr)^")" ;;  

let for_vr2_element ((B b,S n),sl) = 
    "((B "^(string_of_int b)^",S "^(string_of_int n)^"),"^(for_solution_list sl)^")" ;; 


let for_visualization_result = function 
   VR1(l)->"VR1[\n"^(String.concat ";\n" (Image.image for_vr1_element l))^"\n]"
  |VR2(l)->"VR2[\n"^(String.concat ";\n" (Image.image for_vr2_element l))^"\n]"
  |VR3(_l)->"..."
  |VR4(_l)->"..."
  |VR5(_l)->"..."
  |VR6(_l)->"..." ;;

end ;; 

let for_visualization_result = Private.for_visualization_result ;;

end ;;   

module Verify = struct 
  
exception Bad_kmp_index of int ;; 

module Private = struct 

let bound = 40 ;; 

let breadths = Int_range.scale (fun b->B b) 0 bound ;;
let sizes = Int_range.scale (fun n->S n) 1 bound ;;

let whole_range = Cartesian.product breadths sizes ;; 

let global_verification_for_single_pair (width,scrappers) =
    let rec tempf = (fun l->match l with 
      [] -> None 
      | (b,n) :: others ->
         (
          let (good_opt,bad_opt) = Warehouse.nonhalved_bulk_result (P(width,scrappers,b,n)) in 
          if good_opt = None 
          then Some(Option.get bad_opt,(b,n))
          else tempf others     
         )
    ) in
    tempf whole_range ;; 

let rec global_verification = function 
    [] -> None 
   | ws :: others -> 
      match global_verification_for_single_pair ws with 
       Some (i,(b,n)) -> Some (i,P(fst ws,snd ws,b,n))   
      | None -> global_verification others ;; 


let halves = Memoized.make (fun (width,scrappers) -> List.partition (fun 
  (breadth,size) -> let p = P(width,scrappers,breadth,size) in 
     Point.is_in_upper_half p
) whole_range );;

let upper_range (width,scrappers) = fst(halves (width,scrappers)) ;; 
let lower_range (width,scrappers) = snd(halves (width,scrappers)) ;; 

let linear_upper_range (w,_scr,d) =
      Int_range.scale (fun b->(B(b),S(b+2*w-1+d))) (max(2-2*w-d) 0) (bound-2*w+d+1) ;;
let linear_lower_range (w,_scr,d) =
      Int_range.scale (fun n->(B(n-2*w+d),S(n))) (max(2*w-d) 1) (bound-2*w+d) ;;      

let bivariate_selector f g l= 
  let temp1 = Image.image (fun x->let (b,n)=x in (x,f b n,g b n)) l in 
  List.filter (fun (_,y1,y2)->y1<>y2) temp1 ;;

let upper_selector (width,scrappers) f g = bivariate_selector f g (upper_range (width,scrappers)) ;;  
let lower_selector (width,scrappers) f g = bivariate_selector f g (lower_range (width,scrappers)) ;;  

let original1 (width,scrappers) b n =
  Bulk_result.superficial_part( Untamed.compute_bulk_result (P(width,scrappers,b,n))) ;;
let original2 = original1 ;;
let original3 (width,scrappers) b n =
  Bulk_result.solution_list( Untamed.compute_bulk_result (P(width,scrappers,b,n))) ;;
let original4 = original3 ;;
let original5 (width,scrappers) b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  List.length ql;;
let original6 = original5 ;;
let original7 (width,scrappers,IMD ql_idx) b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  let (Q(pt,_ql_constraints,_extension)) = List.nth ql (ql_idx-1) in 
  pt ;;
let original8 = original7 ;;
let original9 (width,scrappers,IMD ql_idx) b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  let (Q(_pt,ql_constraints,_extension)) = List.nth ql (ql_idx-1) in 
  ql_constraints ;;
let original10 = original9 ;;       
let original11 (width,scrappers,IMD ql_idx) b n =
  let ql = Bulk_result.qualified_points( Untamed.compute_bulk_result (P(width,scrappers,b,n))) in 
  let (Q(_pt,_ql_constraints,extension)) = List.nth ql (ql_idx-1) in 
  extension ;;
let original12 = original11 ;; 

let check1 pair g = lower_selector pair (original1 pair) g  ;; 
let check2 pair g = upper_selector pair (original2 pair) g  ;; 
let check3 pair g = lower_selector pair (original3 pair) g  ;; 
let check4 pair g = upper_selector pair (original4 pair) g  ;; 
let check5 pair g = lower_selector pair (original5 pair) g  ;; 
let check6 pair g = upper_selector pair (original6 pair) g  ;; 
let check7  (w,s,i) g = lower_selector (w,s) (original7  (w,s,i)) g  ;; 
let check8  (w,s,i) g = upper_selector (w,s) (original8  (w,s,i)) g  ;; 
let check9  (w,s,i) g = lower_selector (w,s) (original9  (w,s,i)) g  ;; 
let check10 (w,s,i) g = upper_selector (w,s) (original10 (w,s,i)) g  ;; 
let check11 (w,s,i) g = lower_selector (w,s) (original11 (w,s,i)) g  ;; 
let check12 (w,s,i) g = upper_selector (w,s) (original12 (w,s,i)) g  ;; 

let visualize1 (w,scr) d = VR1(Image.image (
   fun (b,n) -> ((b,n),original1 (w,scr) b n)
) (linear_lower_range (w,scr,d))) ;;
let visualize2 (w,scr) d = VR1(Image.image (
   fun (b,n) -> ((b,n),original2 (w,scr) b n)
) (linear_upper_range (w,scr,d))) ;;
let visualize3 (w,scr) d = VR2(Image.image (
   fun (b,n) -> ((b,n),original3 (w,scr) b n)
) (linear_lower_range (w,scr,d))) ;;
let visualize4 (w,scr) d = VR2(Image.image (
   fun (b,n) -> ((b,n),original4 (w,scr) b n)
) (linear_upper_range (w,scr,d))) ;;
let visualize5 (w,scr) d = VR3(Image.image (
   fun (b,n) -> ((b,n),original5 (w,scr) b n)
) (linear_lower_range (w,scr,d))) ;;
let visualize6 (w,scr) d = VR3(Image.image (
   fun (b,n) -> ((b,n),original6 (w,scr) b n)
) (linear_upper_range (w,scr,d))) ;;
let visualize7 (w,s,i) d = VR4(Image.image (
   fun (b,n) -> ((b,n),original7 (w,s,i) b n)
) (linear_lower_range (w,s,d))) ;;
let visualize8 (w,s,i) d = VR4(Image.image (
   fun (b,n) -> ((b,n),original8 (w,s,i) b n)
) (linear_upper_range (w,s,d))) ;;
let visualize9 (w,s,i) d = VR5(Image.image (
   fun (b,n) -> ((b,n),original9 (w,s,i) b n)
) (linear_lower_range (w,s,d))) ;;
let visualize10 (w,s,i) d = VR5(Image.image (
   fun (b,n) -> ((b,n),original10 (w,s,i) b n)
) (linear_upper_range (w,s,d))) ;;
let visualize11 (w,s,i) d = VR6(Image.image (
   fun (b,n) -> ((b,n),original11 (w,s,i) b n)
) (linear_lower_range (w,s,d))) ;;
let visualize12 (w,s,i) d = VR6(Image.image (
   fun (b,n) -> ((b,n),original12 (w,s,i) b n)
) (linear_upper_range (w,s,d))) ;;

let unwrap_check_entry = function 
   CE1(f1)->(Some f1,None,None,None,None,None)
  |CE2(f2)->(None,Some f2,None,None,None,None)
  |CE3(f3)->(None,None,Some f3,None,None,None)
  |CE4(f4)->(None,None,None,Some f4,None,None)
  |CE5(f5)->(None,None,None,None,Some f5,None)
  |CE6(f6)->(None,None,None,None,None,Some f6) ;; 

let unwrap_check_entry1 f = let (opt1,_opt2,_opt3,_opt4,_opt5,_opt6) = unwrap_check_entry f in Option.get opt1 ;;
let unwrap_check_entry2 f = let (_opt1,opt2,_opt3,_opt4,_opt5,_opt6) = unwrap_check_entry f in Option.get opt2 ;;
let unwrap_check_entry3 f = let (_opt1,_opt2,opt3,_opt4,_opt5,_opt6) = unwrap_check_entry f in Option.get opt3 ;;
let unwrap_check_entry4 f = let (_opt1,_opt2,_opt3,opt4,_opt5,_opt6) = unwrap_check_entry f in Option.get opt4 ;;
let unwrap_check_entry5 f = let (_opt1,_opt2,_opt3,_opt4,opt5,_opt6) = unwrap_check_entry f in Option.get opt5 ;;
let unwrap_check_entry6 f = let (_opt1,_opt2,_opt3,_opt4,_opt5,opt6) = unwrap_check_entry f in Option.get opt6 ;;



let check (KMP i_kmp) (w,s,i) tagged_f = match i_kmp with 
    1 -> CR1(check1 (w,s) (unwrap_check_entry1 tagged_f))
   |2 -> CR1(check2 (w,s) (unwrap_check_entry1 tagged_f))
   |3 -> CR2(check3 (w,s) (unwrap_check_entry2 tagged_f))
   |4 -> CR2(check4 (w,s) (unwrap_check_entry2 tagged_f)) 
   |5 -> CR3(check5 (w,s) (unwrap_check_entry3 tagged_f))
   |6 -> CR3(check6 (w,s) (unwrap_check_entry3 tagged_f))
   |7  -> CR4(check7  (w,s,i) (unwrap_check_entry4 tagged_f)) 
   |8  -> CR4(check8  (w,s,i) (unwrap_check_entry4 tagged_f)) 
   |9  -> CR5(check9  (w,s,i) (unwrap_check_entry5 tagged_f))
   |10 -> CR5(check10 (w,s,i) (unwrap_check_entry5 tagged_f))
   |11 -> CR6(check11 (w,s,i) (unwrap_check_entry6 tagged_f))
   |12 -> CR6(check12 (w,s,i) (unwrap_check_entry6 tagged_f))
   |_ -> raise(Bad_kmp_index(i_kmp));; 

let visualize (KMP i_kmp) (w,s,i) d = match i_kmp with 
    1 -> visualize1 (w,s) d 
   |2 -> visualize2 (w,s) d 
   |3 -> visualize3 (w,s) d 
   |4 -> visualize4 (w,s) d 
   |5 -> visualize5 (w,s) d 
   |6 -> visualize6 (w,s) d 
   |7  -> visualize7  (w,s,i) d 
   |8  -> visualize8  (w,s,i) d 
   |9  -> visualize9  (w,s,i) d 
   |10 -> visualize10 (w,s,i) d 
   |11 -> visualize11 (w,s,i) d 
   |12 -> visualize12 (w,s,i) d 
   |_ -> raise(Bad_kmp_index(i_kmp));; 

end ;;

let check = Private.check ;; 
let global_verification = Private.global_verification ;; 
let visualize = Private.visualize ;;

end ;;  



module Overall = struct 

let goal = [(1,[]);(2,[]);(3,[]);(4,[]);(5,[])] ;; 

let ref_for_status = ref None ;; 

let get_status () = match (!ref_for_status) with 
   Some status -> status 
   | None -> 
      let opt =  Verify.global_verification goal 
      and idx = (!(Warehouse.index_for_missing_data)) in 
      let (kmp,pt) = Option.get opt in 
      let answer = (kmp,Warehouse.read_missing_part kmp,idx,pt) in 
      let _ = (ref_for_status := Some answer) in 
      answer ;;

let next_look d =
    let (kmp,_,idx,pt) = get_status () in 
    let (w,s,_,_) = Point.unveil pt in 
    let msg1 = "Current goal is ("^(string_of_int w)^
                ",["^(String.concat "," (Image.image string_of_int s))^"])"  in 
    let _ = (print_string("\n\n"^msg1^"\n\n");flush stdout) in 
    let answer = Verify.visualize kmp (w,s,idx) d in 
    let msg2 = "\n\n\n"^(Pretty_printer.for_visualization_result answer)^"\n\n\n" in  
    let _ = (print_string("\n\n\n"^msg2^"\n\n\n");flush stdout) in 
    (fun ()->answer) ;;  


let check tagged_f = 
  let (kmp,_,idx,pt) = get_status () in 
  let (w,s,_,_) = Point.unveil pt in 
  let msg1 = "Current goal is ("^(string_of_int w)^
              ",["^(String.concat "," (Image.image string_of_int s))^"])"  in 
  let _ = (print_string("\n\n"^msg1^"\n\n");flush stdout) in 
  Verify.check kmp (w,s,idx) tagged_f ;; 

end ;; 
