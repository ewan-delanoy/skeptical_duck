(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 

(*
open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 
open Unimode ;;



let vz = visualize 1 ;; 
let rf1 (B _b) (S n) = 
  if n<=2 then Atomic else  
  match List.assoc_opt n
  [3,Fork([( Empty_point,[2;3] );( Empty_point,[1;3] );( Empty_point,[1;2] )]);
   4,Contraction( P(1,[],B(1),S(4)),C[2;3;4] )] with 
  Some answer -> answer 
  |None -> Contraction(P(2,[],B(n-5),S(n)),C[n-4;n-2;n] ) ;;
let check_rf1 = partial_check 1 (Superficial_result_ARG rf1) ;; 

let rf2 (B _b) (S n) = 
  if n<=2 then Atomic else  
  match List.assoc_opt n
  [3,Fork([( Empty_point,[2;3] );( Empty_point,[1;3] );( Empty_point,[1;2] )]);
   4,Contraction( P(1,[],B(1),S(4)),C[2;3;4] )] with 
  Some answer -> answer 
  |None -> Contraction(P(2,[],B(n-5),S(n)),C[n-4;n-2;n] ) ;;
let check_rf2 = partial_check 2 (Superficial_result_ARG rf2) ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
  if n<=2 then Atomic else  
    match List.assoc_opt n
    [3,Fork([( Empty_point,[2;3] );( Empty_point,[1;3] );( Empty_point,[1;2] )]);
     4,Contraction( P(1,[],B(1),S(4)),C[2;3;4] )] with 
    Some answer -> answer 
    |None -> Contraction(P(2,[],B(n-5),S(n)),C[n-4;n-2;n] ) ;; 

(* RFI END *)
let check_rfi = Chronometer.it global_check (Superficial_result_ARG rfi) ;; 
*)