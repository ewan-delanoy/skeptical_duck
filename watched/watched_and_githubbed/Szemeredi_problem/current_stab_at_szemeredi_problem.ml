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


visualize 1 ;; 
let rf1 (B b) (S _n) = 
   [];;
let check_rf1 = partial_check 1 (Qpe_extension_ARG rf1) ;; 


let rf2 (B b) (S n) = 
  []  ;; 
let check_rf2 = partial_check 2 (Qpe_extension_ARG rf2) ;; 


let rf3 (B b) (S n) = 
  [];;
let check_rf3 = partial_check 2 (Qpe_extension_ARG rf3) ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
  if n=4 then [1;3;4] else 
  if n=5 then [1;2;4;5] else 
  [n-1;n]   ;;

(* RFI END *)
let check_rfi = global_check (Qpe_extension_ARG rfi) ;; 
