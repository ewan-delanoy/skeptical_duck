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


module Qualified_point = struct 

let extend_with qp extension =  
  match qp with 
  Q(pt,old_constraints,extension2) -> 
  Q(pt,old_constraints,i_merge extension extension2)   ;;

let fragile_extend_with qp extension =  
    match qp with 
    Q(pt,old_constraints,extension2) -> 
    Q(pt,old_constraints,extension2@extension)   ;;  

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

        let fragile_extend_with sycom extension = match sycom with 
        Singleton(l) -> Singleton(l@extension)
        | Breakpoint_with_extensions(Q(pt,old_constraints,extension2)) -> 
          Breakpoint_with_extensions(Q(pt,old_constraints,extension2@extension))   ;;    



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

let fragile_extend_with (FD(offshoots,qpoints)) extension =
    FD(Image.image (fun x->x@extension) offshoots,
    Image.image (fun qpoint->Qualified_point.fragile_extend_with qpoint extension) qpoints
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

let fragile_extend_with (BR(sycom,representatives,forced_data)) extension =
  BR(Sycomore_list.fragile_extend_with sycom extension,
        Image.image (fun x->x@extension) representatives,
        Forced_data.fragile_extend_with forced_data extension
  );;    

let extend_with_opt bres_opt extension = match bres_opt with 
  None -> None 
  |Some bres -> Some (extend_with bres extension) ;;     

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
    
let easy_polish bres = 
  let (BR(sycom,representatives,forced_data)) = bres in 
  let (FD(direct,indirect)) = forced_data in 
  if (indirect = [])&&(List.length(direct)=1)
  then let new_sycom = Singleton (List.hd direct) in 
       BR(new_sycom,representatives,forced_data)     
  else bres ;;      

let easy_polish_opt = function 
   None -> None 
   | Some bres -> Some(easy_polish bres) ;;   

let apply_passive_repeat  pt bres =
    let (width,b,_,_) = Point.unveil pt in 
    easy_polish_opt(insert_several_constraints [C[b;b+width;b+2*width]] bres);;
  
let apply_boundary_increment pt bres = 
    let (width,breadth,n,_) = Point.unveil pt in 
    let new_constraints = Constraint.extra_constraints_from_boundary_increment width breadth n in 
    match insert_several_constraints new_constraints bres with 
     None -> None 
    |Some new_bres -> Some(easy_polish(extend_with new_bres [n])) ;;

    
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
   easy_polish(
   BR(Breakpoint_with_extensions(Q(pt,[],[])),new_representatives,new_forced_data)) ;; 

let singleton z = BR(Singleton z,[z],FD([z],[]))  ;;

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
  
  let eval_fobas fobas breadth n = 
    match fobas with 
     Usual_fobas f -> f breadth n ;;  

   

end ;;   


module Parametrized_Example = struct 

let sf1 n = List.filter (fun t->List.mem(t mod 3)[1;2]) (Int_range.range 1 n) ;;


let ptf1 n =  let q = (n/3) in  P (1, 3*q-5, 3*q-3, []) ;;
let ptf2 n =  let q = (n/3) in  P (1, 3*q-2, 3*q, []) ;;
let qpf1 n (a,b) = Q (ptf1 n, a, b) ;; 
let bwef1 n =  Breakpoint_with_extensions (Q (ptf2 n, [], [])) ;;
let bwef2 n =  let q = (n/3) in  Breakpoint_with_extensions (Q (ptf2 n, [], [3*q+1])) ;;
let level2 = Int_range.scale (fun j->C[j;j+2;j+4]) ;; 
let clf1 q = C[3*q-5;3*q-3] :: (Int_range.scale (fun j->C[j;j+2;j+4]) 1 (3*q-8)) ;;
let clf2 q = 
   let rest = (
    if q<3 then [] else   
    Image.image (fun j->C[j;j+2;j+4]) ((Int_range.range 1 (3*q-9))@[3*q-7])
   ) in 
    C[3*q-4] :: rest ;;
let clf3 n = Int_range.scale (fun j->C[j;j+2;j+4]) 1 n  ;;
let clf4 q = if q<3 then [] else C[3*q-6;3*q-4] :: (Int_range.scale (fun j->C[j;j+2;j+4]) 1 (3*q-9)) @ [C[3*q-7;3*q-5;3*q-3]];;

let clf5 breadth n = 
  let q = (n/3) in  
  match (breadth mod 3) with 
   0 ->  (
          if n=breadth+6 
          then clf3(breadth-1)
          else clf3(breadth)
         )  
   |1 -> (
          if n=breadth+5 
          then clf1(q)
          else clf3(breadth)
         ) 
     
   |2 -> (
          if n=breadth+5 
          then clf1(q)
          else clf3(breadth)
         )
   | _ -> failwith("impossible remainder") ;;    

let clf6 breadth n = 
  let q = (n/3) in  
  match (breadth mod 3) with 
   0 ->  (
          if n=breadth+6 
          then clf4(q)
          else clf3(breadth) 
         )  
   |1 -> (
          if n=breadth+5 
          then clf4(q)
          else clf3(breadth) 
         ) 
     
   |2 -> (
          if n=breadth+5 
          then clf4(q)
          else clf3(breadth) 
         )
   | _ -> failwith("impossible remainder") ;;    

let clf7 breadth n = 
  let q = (n/3) in  
  match (breadth mod 3) with 
   0 ->  (
          if n=breadth+7 
          then clf4(q)
          else clf3(breadth)
         )  
   |1 -> (
          if n=breadth+6 
          then clf4(q)
          else clf3(breadth)
         ) 
     
   |2 -> (
          if n=breadth+5 
          then clf2(q)
          else clf3(breadth)
         )
   | _ -> failwith("impossible remainder") ;;    


let v1 = Bulk_result.singleton [1] ;;

let v2 = BR (Breakpoint_with_extensions (Q (P (1, 1, 3, []), [], [])), [[1; 2]],
FD ([[1; 2]; [1; 3]; [2; 3]], [])) ;;

let v3 = BR (Breakpoint_with_extensions (Q (P (1, 1, 3, []), [], [4])), [[1; 2; 4]],
FD ([[1; 2; 4]; [1; 3; 4]], [])) ;;

let v4 = BR (Breakpoint_with_extensions (Q (P (1, 4, 6, []), [], [])), [[1; 2; 4; 5]],
FD ([[1; 2; 4; 5]],
 [Q (P (1, 1, 3, []), [C [1; 3]], [5; 6]);
  Q (P (1, 1, 3, []), [C [2]], [4; 6])])) ;;

let v5 =  BR (Breakpoint_with_extensions (Q (P (1, 4, 6, []), [], [7])),
[[1; 2; 4; 5; 7]],
FD ([[1; 2; 4; 5; 7]], [Q (P (1, 1, 3, []), [C [2]], [4; 6; 7])])) ;; 

let brf1 n =
    let main = sf1 n in 
    let q = (n/3) in 
    let qp1 = qpf1 n in 
    match List.assoc_opt n [1,v1;3,v2;4,v3] with
    Some answer -> answer | None ->
    (match (n mod 3) with 
     0 ->  
      let bwe1 = bwef1 n  in 
      if n=3 then BR (bwe1,[main],FD([[1;2]; [1;3]; [2;3]],[])) 
             else BR (bwe1,[main],FD ([main],[qp1([],[3*q-1; 3*q]);qp1([],[3*q-2; 3*q])]))
    |1 ->  
      let bwe2 = bwef2 n in 
      if n=1 then BR (Singleton main,[main],FD ([main],[])) else
      if n=4 then BR (bwe2,[main],FD ([main;[1;3;4]],[]))  else     
                  BR (bwe2,[main],FD ([main],[qp1([],[3*q-2; 3*q;3*q+1])]))   
    |2 ->  BR (Singleton(main),[main],FD ([main],[]))   
    |_ -> failwith("impossible remainder")) ;; 

(*

To test brf1  : 

open Parametrized_Example ;;
let u1 = needed_subcomputations_for_several_computations 
  (Int_range.scale(fun n->P(2,0,n,[])) 1 30) ;; 
let u2 = Int_range.scale (
  fun n->let p = P(2,0,n,[]) in 
  (n,access  p,brf1 n)
) 1 30;;
let u3 = List.filter (
  fun (n,sol1,sol2)->sol1<>sol2
) u2;;
*)



let brf2 breadth n = 
   if breadth = 0 then Bulk_result.singleton (Int_range.range 1 n) else 
   if n<=(breadth+2) 
   then brf1 n
   else Bulk_result.fragile_extend_with (brf1 (breadth+2)) (Int_range.range (breadth+3) n) ;; 

(*

To test brf2 : 

open Parametrized_Example ;;
let u1 = Cartesian.product (Int_range.range 0 15)(Int_range.range 1 30) ;;
let u2 = Image.image (fun (breadth,n)->P(1,breadth,n,[])) u1 ;;
let u3 = needed_subcomputations_for_several_computations u2 ;; 
let u4 = Image.image (
  fun (breadth,n)->let p = P(1,breadth,n,[]) in 
  ((breadth,n),access p,brf2 breadth n)
) u1;;
let u5 = List.filter (
  fun (pair,sol1,sol2)->sol1<>sol2
) u4;;
*)


let brf3 n =
    let main = sf1 n in 
    let q = (n/3) in 
    let qp1 = qpf1 n in 
    let cl1 = clf1 q
    and cl2 = clf2 q in 
    if n<=5 then brf1 n else
    match List.assoc_opt n [6,v4;7,v5] with Some answer -> answer | None ->
    (  
    match (n mod 3) with 
     0 ->  
      let bwe1 = bwef1 n  in 
      BR (bwe1,[main],FD ([main],[qp1(cl1,[3*q-1; 3*q]);qp1(cl2,[3*q-2; 3*q])]))
    |1 ->  
      let bwe2 = bwef2 n in 
          BR (bwe2,[main],FD ([main],[qp1(cl2,[3*q-2; 3*q;3*q+1])]))   
    |2 ->  BR (Singleton(main),[main],FD ([main],[]))   
    |_ -> failwith("impossible remainder") 
  );; 

(*

To test brf3  : 

open Parametrized_Example ;;
let v1 = needed_subcomputations_for_several_computations 
  (Int_range.scale(fun n->P(3,0,n,[])) 1 30) ;; 
let v2 = Int_range.scale (
  fun n->let p = P(3,0,n,[]) in 
  (n,access p,brf3 n)
) 1 30;;
let v3 = List.filter (
  fun (n,sol1,sol2)->sol1<>sol2
) v2;;
*)

let brf4 breadth n = 
  let q = (n/3) 
  and p1 = ptf1 n
  and main = sf1 n  in 
  match (n mod 3) with 
   0 ->  
    let bwe1 = bwef1 n  in
    BR (bwe1,[main],FD ([main],
      [Q(p1,clf5 breadth n,[3*q-1; 3*q]); 
       Q(p1,clf6 breadth n,[3*q-2; 3*q])])) 
  |1 -> 
    let bwe2 = bwef2 n in
    BR (bwe2,[main],FD ([main],[Q(p1,clf7 breadth n,[3*q-2; 3*q; 3*q+1])])) 
  |2-> BR (Singleton (main),[main],FD ([main],[]))     
  | _ -> failwith("impossible remainder") ;;  


let brf5 breadth n = 
   if breadth = 0 then brf1 n  else 
   if n<=(breadth+4) 
   then brf3 n
   else brf4 breadth n ;; 

(*

To test brf5 : 

open Parametrized_Example ;;
let u1 = Cartesian.product (Int_range.range 0 15)(Int_range.range 1 30) ;;
let u2 = Image.image (fun (breadth,n)->P(2,breadth,n,[])) u1 ;;
let u3 = needed_subcomputations_for_several_computations u2 ;; 
let u4 = Image.image (
  fun (breadth,n)->let p = P(2,breadth,n,[]) in 
  ((breadth,n),access p,brf5 breadth n)
) u1;;
let u5 = List.filter (
  fun (pair,sol1,sol2)->sol1<>sol2
) u4;;


*)
    
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
    match Private.find_meaningful_obstruction (old_width,old_breadth) domain with 
      None -> ((old_width,old_breadth),([],domain)) 
      | Some (width,breadth) -> ((width,breadth),Private.detach (width,breadth) domain);;  
  
  
  end ;;  
  
  module Bulgarian = struct 
  
    let decompose pt =
        let (old_width,old_breadth,n,scrappers) = Point.unveil pt in 
        let domain = concretize (n,scrappers) in 
         let ((new_width,new_breadth),(new_domain,adjustment)) =
         Bulgarian_for_nonparametrized_sets.decompose (old_width,old_breadth) domain in 
         let (new_n,new_scrappers) = abstractize new_domain in 
          (P(new_width,new_breadth,new_n,new_scrappers),adjustment);;
      
  (*
     
  let check1 = (decompose (P(1,4,6,[])) =  (P (1, 4, 6, []), [])) ;;
  let check2 = (decompose (P(1,3,6,[])) =  (P (1, 3, 5, []), [6])) ;;
  
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

let nonbulgarian_getter  ~with_anticipation pt = 
let (width,breadth,n,scrappers) = Point.unveil pt in 
let z = concretize (n,scrappers) in 
if ((List.length z)<3)||
    ((width,breadth)=(1,0))||
    (test_for_admissiblity width breadth z) 
then Some (Bulk_result.singleton z) 
else 
match Hashtbl.find_opt rose_hashtbl (width,scrappers) with 
Some summary -> Some (Parametrized.eval_fobas summary breadth n)
| None ->  
 (match Hashtbl.find_opt medium_hashtbl (width,breadth,scrappers) with 
   Some summary -> Some (Parametrized.eval_fos summary n)
 | None -> Accumulator_with_optional_anticipator.get_from_low_hashtbl ~with_anticipation pt) ;;   

let bulgarian_getter ~with_anticipation pt = 
  let (pt2,adj)= Bulgarian.decompose pt  in 
  Bulk_result.extend_with_opt ( nonbulgarian_getter  ~with_anticipation pt2) adj ;;

   
let generic_access ~with_anticipation pt = 
   Option.unpack(bulgarian_getter ~with_anticipation pt) ;;   



let polish_fork ~with_anticipation pt unpolished_result =
    Some unpolished_result ;; 

let apply_hook_naively ~with_anticipation pt hook ll =  
  match hook with 
  Passive_repeat -> Bulk_result.apply_passive_repeat pt (List.hd ll)
| Boundary_increment -> Bulk_result.apply_boundary_increment pt (List.hd ll)
| Fork ->  polish_fork ~with_anticipation pt (Bulk_result.apply_fork pt ll)
| Jump -> Some(List.hd ll);; 

exception Apply_hook_exn of (bulk_result list) * hook * point ;;


let apply_hook ~with_anticipation pt hook args = 
   match apply_hook_naively ~with_anticipation pt hook args with 
   None -> None 
    |Some bres ->
       let (BR(_,reps,_)) = bres in 
       if reps = []
       then raise (Apply_hook_exn(args,hook,pt)) 
       else Some bres ;;



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
   let nonbulgarian_descendants = descendants_for_hook pt hook in  
   let descendants = Image.image Bulgarian.decompose nonbulgarian_descendants in 
   let descendants_with_their_images = Image.image (
      fun (pt2,adj)  -> (pt2,Bulk_result.extend_with_opt (nonbulgarian_getter ~with_anticipation pt2) adj)
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
    
let hook_and_descendants ~with_anticipation pt =  
    let (_,result_opt) = find_remote_stumbling_block_or_immediate_working_hook 
    ~with_anticipation pt in 
    let hook = Option.unpack result_opt in 
    let nonbulgarian_descendants = descendants_for_hook pt hook in  
    (hook,Image.image Bulgarian.decompose nonbulgarian_descendants) ;; 
    


let find_remote_stumbling_block_or_immediate_working_hook_after_a_direct_try 
~with_anticipation pt =      
   match bulgarian_getter ~with_anticipation pt with 
    Some old_answer -> ([],None) 
    | None ->
      find_remote_stumbling_block_or_immediate_working_hook ~with_anticipation pt ;;
    
  
exception Pusher_exn ;;

let rec pusher_for_recursive_computation to_be_treated= 
    match to_be_treated with 
    [] -> raise(Pusher_exn)
    | pt :: others -> 
       let (pt2,adj) = Bulgarian.decompose pt in 
       let (missing_data,opt_res) =
      find_remote_stumbling_block_or_immediate_working_hook_after_a_direct_try 
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


let exhaust_new_line (width,breadth,scrappers) = 
    let temp1 = Int_range.scale 
      (fun n->P(width,breadth,n,scrappers)) 1  60 in 
    let carrier = needed_subcomputations_for_several_computations temp1 in 
    let temp2 = Image.image (fun pt-> 
      let mutilated_carrier = List.filter (
        fun p->fst(p)<>pt
      ) carrier in 
      let _ = ( Accumulator_with_optional_anticipator.low_anticipator :=mutilated_carrier) in 
      let (_,hook_opt) = find_remote_stumbling_block_or_immediate_working_hook_after_a_direct_try 
          ~with_anticipation:true pt in 
      (Point.size pt,hook_opt)
    ) temp1 in 
    let selector = (fun l->Option.filter_and_unpack  (fun (n,pair_opt)->match pair_opt with 
      None -> None |Some pair ->Some(n,pair)) l) in 
    let temp3 = selector temp2 in 
    let temp4 = Int_range.scale (fun n-> 
       let pt2 = P(width,breadth,n,scrappers) in 
       let _ = (  Accumulator_with_optional_anticipator.low_anticipator:=carrier) in 
      (n, bulgarian_getter ~with_anticipation:true pt2 ))  1 50  in 
    let temp5 = selector temp4 in 
    (temp3,temp5) ;;   

let access = generic_access ~with_anticipation:true ;;     
let see p = (hook_and_descendants ~with_anticipation:true p,access p);;
let rec all_representatives p =
    let (BR(sycom,reps,forced_data)) = access p in 
    let (FD(offshoots,qpoints)) = forced_data in 
    let temp1 = Image.image (
         fun (Q(pt,constraints,extension)) -> 
           let ttemp2 = all_representatives pt in 
           let ttemp3 = List.filter (Constraint.satisfied_by_individual constraints) ttemp2 in 
           Image.image (i_merge extension) ttemp3
    ) qpoints in 
    il_fold_merge (offshoots::temp1) ;;

rose_add (1,[]) (Usual_fobas(Parametrized_Example.brf2));;
med_add (2,0,[]) (Usual_fos(Parametrized_Example.brf1)) ;; 
rose_add (2,[]) (Usual_fobas(Parametrized_Example.brf5));;



(*

#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;

let g1 = needed_subcomputations_for_single_computation (P(4,0,8,[])) ;;

let g2 = needed_subcomputations_for_single_computation (P(3,1,7,[])) ;;


open Parametrized_Example ;; 

let current_width = 2 
and current_breadth = 40 
and current_strappers = [] ;;
let (_,small_accu) = exhaust_new_line (current_width,current_breadth,current_strappers) ;;

let tg b n = access ~with_anticipation:true (P(current_width,b,n,current_strappers)) ;;

let current_width = 2 
and current_breadth = 40 
and current_strappers = [] ;;
let (_,small_accu) = exhaust_new_line (current_width,current_breadth,current_strappers) ;;
let tf n = 
  let p = P(current_width,current_breadth,n,current_strappers) in 
  (hook_and_descendants ~with_anticipation:true p,List.assoc n small_accu);;

let check_g2 = List.filter (
  fun (n,bres)->bres <> Parametrized_Example.brf1 n
) g2 ;;

*)

