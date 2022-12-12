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

(* it is assumed that compatibility has already been checked *)   
let extend_with (M(reps,qpoints)) extension =
  M(Image.image (i_merge extension) reps,
  Image.image (fun qpoint->Qualified_point.extend_with qpoint extension) qpoints
  ) ;;  

let insert_several_constraints extra_constraints (M(reps,qpoints)) = 
  M(List.filter (Constraint.satisfied_by_individual extra_constraints) reps,
     More_option.filter_and_unpack (
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

let is_not_atomic (BR(sr,_)) = sr <> Atomic ;; 

let superficial_part (BR(sr,_)) = sr ;; 
let mold (BR(_,md)) = md ;; 

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
    | Some new_mold -> Some(BR(Contraction(pt,cstr),new_mold)) ;;
     

end ;;  
    
module Parametrized = struct 
      
      let eval_fos fos n =
         match fos with 
            Usual_fos f -> f n ;; 
      
      let eval_fobas fobas breadth n = 
        match fobas with 
         Usual_fobas f -> f breadth n ;;  
  
      
    
    end ;;   

module Example = struct     
(* Points *)    
let ep = Empty_point ;;
let vp1 n = P (1, n-2, n, []) ;;
let vp2 n = P (1, n-3, n, []) ;;    
let vp3 n = P (2, n-5, n, []) ;;  

(* Constraints *)
let vcstr1 n = C [n-2; n-1; n] ;; 
let vcstr2 n = C [n-4; n-2; n] ;; 

(* Set of constraints *)

let helper1_for_constraints_lists l r=
   (Image.image (fun x->C x) l) 
   @ (Int_range.scale (fun t->C[t;t+2;t+4]) 1 r) ;; 

let vcl1 n = 
  if n<6 then [] else 
  let m = (if n<8 then 0 else n-8) in 
  helper1_for_constraints_lists [[n-5;n-3]] m ;;

(*  
let check_vcl1 = 
    let bound = 30 in 
    let temp1 = Int_range.scale (
        fun k->
        let (M(_,qpoints)) = Bulk_result.mold(compute_bulk_result (P(3,0,k,[]))) in     
        let res = Option.map (fun (Q(_,l_cstr,_)) -> l_cstr) 
          (List.nth_opt qpoints 0) in 
           
        (k,res,Example.vcl1 k)
      ) 1 bound in 
    List.filter (fun (p,x,y)->(x<>None)&&(x<>Some y)) temp1 ;; 
*)


let vcl2 n = 
  if n<5 then [] else
  helper1_for_constraints_lists [[n-4;n-2]] (n-7) ;;

(*  
let check_vcl2 = 
    let bound = 30 in 
    let temp1 = Int_range.scale (
        fun k->
        let (M(_,qpoints)) = Bulk_result.mold(compute_bulk_result (P(3,0,k,[]))) in     
        let res = Option.map (fun (Q(_,l_cstr,_)) -> l_cstr) 
          (List.nth_opt qpoints 1) in 
        (k,res,Example.vcl2 k)
      ) 1 bound in 
    List.filter (fun (p,x,y)->(x<>None)&&(x<>Some y)) temp1 ;; 
*)    

let vcl3 n = 
  if n<4 then [] else
  helper1_for_constraints_lists [] (n-5) ;;

(*
let check_vcl3 = 
    let bound = 30 in 
    let temp1 = Int_range.scale (
        fun k->
        let (M(_,qpoints)) = Bulk_result.mold(compute_bulk_result (P(3,0,k,[]))) in     
        let res = Option.map (fun (Q(_,l_cstr,_)) -> l_cstr) 
          (List.nth_opt qpoints 2) in 
        (k,res,Example.vcl3 k)
      ) 1 bound in 
    List.filter (fun (p,x,y)->(x<>None)&&(x<>Some y)) temp1 ;; 
*)



(* Sets of integers *)
let vso1 n = List.filter (fun t->List.mem(t mod 3)[1;2]) (Int_range.range 1 n) ;;

(*
   
let check_vso1 = 
    let bound = 30 in 
    let temp1 = Int_range.scale (
        fun k->
        let (M(sols,_)) = Bulk_result.mold(compute_bulk_result (P(3,0,k,[]))) in     
        (k,sols,[Example.vso1 k])
      ) 1 bound in 
    List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)

let vvso1 d n =
  match (n-d) mod 3 with 
  0 -> (vso1(n-(d-2)))@(Int_range.range (n-(d-3)) n)
 |1 -> if d = 3 then vso1 n else
       (vso1(n-(d-4)))@(Int_range.range (n-(d-5)) n)
 |2 -> (vso1(n-(d-3)))@(Int_range.range (n-(d-4)) n)
 |_ -> failwith("Impossible remainder by 3") ;;

(*
   
let check_vvso1 = 
  let bound = 30 in 
  let all_pairs = Cartesian.square (Int_range.range 3 bound) in 
  let concerned_pairs = List.filter (fun (d,k)->d<=k) all_pairs in 
  let temp1 = Image.image (
      fun (d,k)->
      let (M(sols,_)) = Bulk_result.mold(compute_bulk_result (P(1,k-d,k,[]))) in     
      ((d,k),sols,[Example.vvso1 d k])
    ) concerned_pairs in 
  List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)

let vvso2 b n = 
  if b=0 
  then Int_range.range 1 n
  else     
  if b<=n-3
  then vvso1 (n-b) n 
  else vso1 n;;

(*
   
let check_vvso2 = 
  let bound = 30 in 
  let all_pairs = Cartesian.product 
      (Int_range.range 0 bound) (Int_range.range 1 bound) in 
  let temp1 = Image.image (
      fun (b,n)->
      let (M(sols,_)) = Bulk_result.mold(compute_bulk_result (P(1,b,n,[]))) in     
      ((b,n),sols,[Example.vvso2 b n])
    ) all_pairs in 
  List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)


(* Superficial results *)  
let vvsu1 d n= 
  if n = d then Atomic else 
  Decomposable(P(1,n-d,n-(d-2),[]),Int_range.range (n-(d-3)) n);; 

(*
   
let check_vvsu1 = 
  let bound = 30 in 
  let all_pairs = Cartesian.square (Int_range.range 4 bound) in 
  let concerned_pairs = List.filter (fun (d,k)->d<=k) all_pairs in 
  let temp1 = Image.image (
      fun (d,k)->((d,k),
      Bulk_result.superficial_part(compute_bulk_result (P(1,k-d,k,[]))),
      Example.vvsu1 d k)
    ) concerned_pairs in 
  List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)

let vsu1 n= 
  match n with 
  1 | 2 -> Atomic 
  | 3 -> Fork [(ep, [2;3]);(ep, [1;3]);(ep, [1;2])]
  | _ -> 
  (match n mod 3 with 
  0 -> Fork
  [(vp1(n-3), [n-1; n]);
   (vp1(n-2), [n]);
   (vp1(n-1), [])]
 |1|2 ->  Contraction (vp2(n), vcstr1 n)
 |_ -> failwith("Impossible remainder by 3")) ;; 

(*
   
let check_vsu1 = 
   let temp1 = Int_range.scale (
     fun k->(k,
     Bulk_result.superficial_part(compute_bulk_result (P(2,0,k,[]))),
     Example.vsu1 k)
   ) 1 30 in 
   List.filter (fun (n,x,y)->x<>y) temp1 ;; 

*)

let vsu2 n= 
  match n with 
  1 | 2 -> Atomic 
  | 3 -> Fork [(ep, [2;3]);(ep, [1;3]);(ep, [1;2])]
  | 4 -> Contraction (vp2 4, C[2;3;4])
  | _ ->  Contraction (vp3(n), vcstr2 n) ;; 


(*   
let check_vsu2 = 
   let temp1 = Int_range.scale (
     fun k->(k,
     Bulk_result.superficial_part(compute_bulk_result (P(3,0,k,[]))),
     Example.vsu2 k)
   ) 1 30 in 
   List.filter (fun (n,x,y)->x<>y) temp1 ;; 
*)

let vvsu2 b n = 
  if b=0 
  then Atomic
  else     
  if b<=n-3
  then vvsu1 (n-b) n 
  else vsu1 n;;

(*
   
let check_vvsu2 = 
  let bound = 30 in 
  let all_pairs = Cartesian.product 
      (Int_range.range 0 bound) (Int_range.range 1 bound) in 
  let temp1 = Image.image (
      fun (b,n)->
      let sr = Bulk_result.superficial_part(compute_bulk_result (P(1,b,n,[]))) in     
      ((b,n),sr,Example.vvsu2 b n)
    ) all_pairs in 
  List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)


(* Qualified points *)

let vvq1 d n = Q (vp1(n-d), [], Int_range.range (n-(d-2)) n) ;; 

let vq0_1 n = Q (ep,[],[1;2]@(Int_range.range 4 n)) ;;
let vq0_2 n = Q (ep,[],[1;3]@(Int_range.range 4 n)) ;;
let vq0_3 n = Q (ep,[],[2;3]@(Int_range.range 4 n)) ;;

let vq1_1 n = 
  match n with 
   3 -> Q(ep,[],[2;3]) 
  |4 -> Q(ep,[],[1;3;4]) 
  |5 -> Q(ep,[],[1;2;4;5])
  |_ ->  Q(vp1(n-3),vcl1 n,[n-1;n]) ;;

(*  
let check_vq1_1 = 
    let bound = 30 in 
    let temp1 = Int_range.scale (
        fun k->
        let (M(_,qpoints)) = Bulk_result.mold(compute_bulk_result (P(3,0,k,[]))) in     
        let res = 
          (List.nth_opt qpoints 0) in 
        (k,res,vq1_1 k)
      ) 1 bound in 
    List.filter (fun (p,x,y)->(x<>None)&&(x<>Some y)) temp1 ;; 
*)

let vq1_2 n = 
  match n with 
   3 -> Q(ep,[],[1;3]) 
  |4 -> Q(ep,[],[1;2;4]) 
  |_ ->  Q(vp1(n-2),vcl2 n,[n]) ;;

(*  
let check_vq1_2 = 
    let bound = 30 in 
    let temp1 = Int_range.scale (
        fun k->
        let (M(_,qpoints)) = Bulk_result.mold(compute_bulk_result (P(3,0,k,[]))) in     
        let res = 
          (List.nth_opt qpoints 1) in 
        (k,res,Example.vq1_2 k)
      ) 1 bound in 
    List.filter (fun (p,x,y)->(x<>None)&&(x<>Some y)) temp1 ;; 
*)

let vq1_3 n = 
  match n with 
   3 -> Q(ep,[],[1;2]) 
  |_ ->  Q(vp1(n-1),vcl3 n,[]) ;;

(*  
let check_vq1_3 = 
    let bound = 30 in 
    let temp1 = Int_range.scale (
        fun k->
        let (M(_,qpoints)) = Bulk_result.mold(compute_bulk_result (P(3,0,k,[]))) in     
        let res = 
          (List.nth_opt qpoints 2) in 
        (k,res,Example.vq1_3 k)
      ) 1 bound in 
    List.filter (fun (p,x,y)->(x<>None)&&(x<>Some y)) temp1 ;; 
*)

(* Lists of qualified points *)

let vvql1 d n =  
  match List.assoc_opt (n-d) [
    0,[];
    1,[vq0_3;vq0_2;vq0_1];
    2,[vq0_2;vq0_1];
    3,[vq0_1]
  ]  with 
  Some (funs)->Image.image (fun f->f n) funs 
 | None ->
 (match List.assoc_opt ((n-d) mod 3) [
  0,[vvq1 (d+1)];
  1,[vvq1 (d+1);vvq1 d;vvq1 (d-1)];
  2,[vvq1 (d+1);vvq1 d];
] with 
Some (funs)->Image.image (fun f->f n) funs 
|None -> failwith("Impossible remainder by 3")) ;;

(*
   
let check_vvql1 = 
  let bound = 30 in 
  let all_pairs = Cartesian.square (Int_range.range 3 bound) in 
  let concerned_pairs = List.filter (fun (d,k)->d<=k) all_pairs in 
  let temp1 = Image.image (
      fun (d,k)->
      let (M(_,ql)) = Bulk_result.mold(compute_bulk_result (P(1,k-d,k,[]))) in   
      ((d,k),ql,Example.vvql1 d k)
    ) concerned_pairs in 
  List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)


let vql1 n =  
  match n with 
 1 | 2 -> [] 
 | 3 -> [vq0_3 3;vq0_2 3;vq0_1 3]
 | 4 -> [vq0_2 4;vq0_1 4]
 | 5 -> [vq0_1 5]
 | _ ->
 (match n mod 3 with 
 0 ->  [vvq1 3 n;vvq1 2 n;vvq1 1 n]
|1 ->  [vvq1 3 n;vvq1 2 n]
|2 ->  [vvq1 3 n]
|_ -> failwith("Impossible remainder by 3")) ;; 

(*
let check_vql1 = 
  let temp1 = Int_range.scale (
    fun k->
    let (M(_,ql)) = Bulk_result.mold(compute_bulk_result (P(2,0,k,[]))) in   
      (k,ql,Example.vql1 k)
  ) 1 30 in 
  List.filter (fun (n,x,y)->x<>y) temp1 ;; 
*)  

let vvql2 b n = 
  if b=0 
  then []
  else     
  if b<=n-3
  then vvql1 (n-b) n 
  else vql1 n;;

(*
   
let check_vvql2 = 
  let bound = 30 in 
  let all_pairs = Cartesian.product 
      (Int_range.range 0 bound) (Int_range.range 1 bound) in 
  let temp1 = Image.image (
      fun (b,n)->
      let (M(_,ql)) = Bulk_result.mold(compute_bulk_result (P(1,b,n,[]))) in     
      ((b,n),ql,Example.vvql2 b n)
    ) all_pairs in 
  List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)

(* Bulk results *)

let vvbr1 b n = BR(vvsu2 b n,M([vvso2 b n],vvql2 b n)) ;;

(*
   
let check_vvbr1 = 
  let bound = 30 in 
  let all_pairs = Cartesian.product 
      (Int_range.range 0 bound) (Int_range.range 1 bound) in 
  let temp1 = Image.image (
      fun (b,n)->
      let br = compute_bulk_result (P(1,b,n,[])) in     
      ((b,n),br,Example.vvbr1 b n)
    ) all_pairs in 
  List.filter (fun (p,x,y)->x<>y) temp1 ;; 

*)


let vbr1 n = BR(vsu1 n,M([vso1(n)],vql1(n))) ;;
(*
let check_vbr1 = 
  let temp1 = Int_range.scale (
    fun k->(k,
    compute_bulk_result (P(2,0,k,[])),
    Example.vbr1 k)
  ) 1 30 in 
  List.filter (fun (n,x,y)->x<>y) temp1 ;; 
*)

end ;;  


module Parametrized_Example = struct 

let width_1_no_scrappers =
    Usual_fobas Example.vvbr1 ;;

end ;;   
      

let low_hashtbl = Hashtbl.create 50 ;;
  
let rose_hashtbl = Hashtbl.create 50 ;;
let medium_hashtbl = Hashtbl.create 50 ;;


let access_opt  pt = 
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
     ~overwriter:new_text 
        (
          "(*"^" Reproduced stab starts here *)",
          "(*"^" Reproduced stab ends here *)"
        ) ap_for_head ;; 
   
let superificial_result_in_jump_case  pt_after_jump =
  let (width,breadth,n,scrappers) = Point.unveil pt_after_jump in 
  let pt_before_jump = P(width-1,n-2*(width-1),n,scrappers) in  
  let (pt2,adj2) = Simplest_reduction.decompose pt_before_jump in 
  ([],Some(Decomposable(pt2,adj2))) ;; 
    
let access_with_helper_opt pt helper =
    match List.assoc_opt pt helper with 
    Some answer -> Some answer
    | None ->  access_opt pt ;;

let compute_superficial_result_partially pt helper =  
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
  then superificial_result_in_jump_case pt   
  else
  let (width2,breadth2,n2,scrappers2) = Point.unveil pt2 in 
  let _ = assert(breadth2>0) in 
  let front_constraint = C [breadth2;breadth2+width2;breadth2+2*width2] 
  and preceding_point = P(width2,breadth2-1,n2,scrappers2) in 
  match access_with_helper_opt  preceding_point helper with 
    None -> ([preceding_point],None)
   |Some bres ->
       (match Bulk_result.impose_one_more_constraint_opt preceding_point front_constraint bres  with 
       None -> let tooths = Int_range.scale (fun k->
                let (m,scr) = remove_one_element  (n2,scrappers2)  (breadth2+k*width2) in 
                let pt3 = P(width2,breadth2-1,m,scr) in 
                Simplest_reduction.decompose(pt3) 
               ) 0 2  in 
              ([],Some(Fork tooths))
      |Some bres2 -> ([],Some(Contraction(preceding_point,front_constraint)))) ;; 

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

rose_add (1,[]) Parametrized_Example.width_1_no_scrappers;;
