(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz3_preliminaries ;;

let see0 = Overall.get_status () ;; 
let see1 = Overall.next_look 1 ;; 

let unvr1 = function (VR1 l)->l | _ -> failwith("aaa") ;; 

let res1 = unvr1(Overall.next_look 1 ()) ;; 
let rf1 (B b,S n) =
  match List.assoc_opt n 
     [
      1,Atomic;
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
   |_-> failwith("bad reminder by 3"));;       
let check_rf1 = List.filter (fun (x,y)->y<> rf1 x) res1 ;;     

let res2 = unvr1(Overall.next_look 2 ()) ;; 
let rf2 (B b,S n) =
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
 |_-> failwith("bad reminder by 3"))
  ;;       
let check_rf2 = List.filter (fun (x,y)->y<> rf2 x) res2 ;;


let res3 = unvr1(Overall.next_look 3 ()) ;; 
let rf3 (B b,S n) =
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
 |_-> failwith("bad reminder by 3"))
  ;;             
let check_rf3 = List.filter (fun (x,y)->y<> rf3 x) res3 ;;

let res4 = unvr1(Overall.next_look 4 ()) ;; 
let rf4 (B b,S n) =
  match List.assoc_opt n 
     [5,Atomic]  with    
    Some answer -> answer 
 | None ->
    Decomposable (P (1, [], B (n-5), S (n-3)), [n-2;n-1;n])  
  ;;       
let check_rf4 = List.filter (fun (x,y)->y<> rf4 x) res4 ;;

let res5 = unvr1(Overall.next_look 5 ()) ;; 
let rf5 (B b,S n) =
  match List.assoc_opt n 
     [6,Atomic]  with    
    Some answer -> answer 
 | None ->
    Decomposable (P (1, [], B (n-6), S (n-4)), Int_range.range (n-3) n)  
  ;;       
let check_rf5 = List.filter (fun (x,y)->y<> rf5 x) res5 ;;

let rfi (B b,S n) =
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
   |_-> failwith("bad reminder by 3"))
    ;;                 

let help_with_check_rfi =
  Image.image ( fun (b,n)->
    let bres = Bulk_result.superficial_part( Untamed.compute_bulk_result (P(1,[],b,n))) in 
    ( (b,n),
      rfi (b,n),
    bres
  ) ) (Verify.Private.lower_range(1,[]));;

let check_rfi = List.filter (fun (x,y1,y2)->y1<>y2)  help_with_check_rfi ;; 

let u1 = Image.image (fun ((B b,S n),_,_)->b-n+2) check_rfi ;; 
let u2 = Multiset.diforchan u1 ;;   
let u3 = List.filter (fun ((B b,S n),_,_)->b-n+2=2) check_rfi ;; 

Bulk_result.superficial_part( Untamed.compute_bulk_result (P(1,[],B 7,S 7))) ;;

let u4 = Verify.Private.lower_range(1,[]) ;; 





let g1 = Overall.check (CE1 f_1_empty_set_kmp1) ;; 
let g2 = (function (CR1 x)->x |_->failwith("aaa")) g1 ;;  

Bulk_result.superficial_part( Untamed.compute_bulk_result (P(1,[],B 0,S 7))) ;;

