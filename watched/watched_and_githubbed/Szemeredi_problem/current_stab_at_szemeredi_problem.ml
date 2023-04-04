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

open Qpl_length_upper_half_mode ;; 

let vz1 = visualize 1 ;; 
let rf1 (B b) (S n) = 
  if n=2 then 0 else
  List.assoc (n mod 3) [0,3;1,2;2,1] ;; 
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  if n=3 then 0 else
  List.assoc (n mod 3) [0,1;1,3;2,2] ;; 
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  if n=4 then 0 else
  List.assoc (n mod 3) [0,2;1,1;2,3] ;; 
let check_rf3 = partial_check 3 rf3 ;; 

let vz4 = visualize 4 ;; 
let rf4 (B b) (S n) = 
  if b=0 then 0 else
  List.assoc (b mod 3,n mod 3) [(0,2),1;(1,0),3;(2,1),2;] ;; 
let check_rf4 = partial_check 4 rf4 ;; 

let vz5 = visualize 5 ;; 
let rf5 (B b) (S n) = 
  if b=0 then 0 else
  List.assoc (b mod 3,n mod 3) [(0,0),1;(1,0),3;(2,1),2;] ;; 
let check_rf2 = partial_check 2 rf2 ;; 


(* RFI BEGIN *)

let rfi (B b) (S n) = 
  if b=0 then 0 else
  List.assoc (b mod 3) [0,1;1,3;2,2] ;;  

(* RFI END *)
let check_rfi = global_check rfi ;; 