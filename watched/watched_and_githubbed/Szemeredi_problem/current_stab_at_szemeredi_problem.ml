(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
open Tools_for_warehouse ;; 
let see0 = Overall.get_status () ;; 
open Unimode ;;


Int_range.scale visualize 1 3;; 
let rf1 (B b) (S _n) = 
   if b=0 then Contraction(P(1,[],B(1),S(4)),C[2;3;4])
          else Contraction( P(2,[],B(b-1),S(b+4)),C[b;b+2;b+4] )
   ;;
let check_rf1 = partial_check 1 (Superficial_result_ARG rf1) ;; 


let rf2 (B b) (S n) = 
  if b=0 then Contraction(P(1,[],B(n-3),S(n)),C[n-2;n-1;n])
  else Contraction( P(2,[],B(b-1),S(n)),C[b;b+2;b+4] )
    ;; 
let check_rf2 = partial_check 2 (Superficial_result_ARG rf2) ;; 


let rf3 (B b) (S n) = 
  if b=0 then Fork([( P(1,[],B(1),S(3)),[5;6] );( P(1,[],B(2),S(4)),[6] );( P(1,[],B(3),S(5)),[] )])
         else Contraction( P(2,[],B(b-1),S(n)),C[b;b+2;b+4] )
    ;; 
let check_rf3 = partial_check 3 (Superficial_result_ARG rf3) ;; 

let rf4 (B b) (S n) = 
  if b=0 then Contraction(P(1,[],B(n-3),S(n)),C[n-2;n-1;n])
  else Contraction( P(2,[],B(b-1),S(n)),C[b;b+2;b+4] )
    ;; 
let check_rf4 = partial_check 4 (Superficial_result_ARG rf4) ;; 

(*
Fork([( P(1,[],B(1),S(3)),[5;6] );( P(1,[],B(2),S(4)),[6] );( P(1,[],B(3),S(5)),[] )])
Fork([( P(1,[],B(4),S(6)),[8;9] );( P(1,[],B(5),S(7)),[9] );( P(1,[],B(6),S(8)),[] )])
Fork([( P(1,[],B(7),S(9)),[11;12] );( P(1,[],B(8),S(10)),[12] );( P(1,[],B(9),S(11)),[] )])
*)


(* RFI BEGIN *)

let rfi (B b) (S n) = 
  if b=0 
  then (
        if (n mod 3)=0 
        then Fork([(P(1,[],B(n-5),S(n-3)),[n-1;n]);
                   (P(1,[],B(n-4),S(n-2)),[n]);
                   (P(1,[],B(n-3),S(n-1)),[])])
        else Contraction(P(1,[],B(n-3),S(n)),C[n-2;n-1;n])
       )
  else Contraction( P(2,[],B(b-1),S(n)),C[b;b+2;b+4] )
    ;;   

(* RFI END *)
let check_rfi = Chronometer.it global_check (Superficial_result_ARG rfi) ;; 
