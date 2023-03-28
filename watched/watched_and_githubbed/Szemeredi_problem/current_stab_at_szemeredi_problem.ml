(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz3_preliminaries ;;

let see0 = Overall.get_status () ;; 

let unvr = function (VR3 l)->l | _ -> failwith("aaa") ;; 

let res1 = unvr(Overall.next_look 1 ()) ;; 
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

