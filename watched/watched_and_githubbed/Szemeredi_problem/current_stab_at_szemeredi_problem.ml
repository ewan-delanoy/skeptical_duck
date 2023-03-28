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
     [2,Atomic;
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
     [3,Atomic]  with    
    Some answer -> answer 
 | None ->
    Decomposable (P (1, [], B (n-3), S (n-1)), [n])  
  ;;       
let check_rf2 = List.filter (fun (x,y)->y<> rf2 x) res2 ;;

let res3 = unvr1(Overall.next_look 3 ()) ;; 
let rf3 (B b,S n) =
  match List.assoc_opt n 
     [4,Atomic]  with    
    Some answer -> answer 
 | None ->
    Decomposable (P (1, [], B (n-4), S (n-2)), [n-1;n])  
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
   if b=0 then Atomic else 
   if n=b+2  
   then  (match (n mod 3) with 
    0 -> Fork([( P(1,[],B(n-5),S(n-3)),[n-1;n] );
               ( P(1,[],B(n-4),S(n-2)),[n] );
               ( P(1,[],B(n-3),S(n-1)),[] )])
   |1-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] )  
   |2-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] ) 
   |_-> failwith("bad reminder by 3")) 
    else Decomposable (P (1, [], B (b+1), S (b+2)), Int_range.range (b+3) n)  ;;   

let help_with_check_rfi =
  Image.image ( fun (b,n)->
    let bres = Bulk_result.superficial_part( Untamed.compute_bulk_result (P(1,[],b,n))) in 
    ( (b,n),
      rfi (b,n),
    bres
  ) ) (Verify.Private.lower_range(1,[]));;

let check_rfi = List.filter (fun (x,y1,y2)->y1<>y2)  help_with_check_rfi ;; 

  
