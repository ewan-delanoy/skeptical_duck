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


let vz = visualize_by_d ;;
Int_range.scale vz 1 4;; 

let rf1 (B b) (S n) = 
   if n=4 then [1;2;4] else 
   [n] 
  ;;
let check_rf1 = partial_check_by_d 1 (Qpe_extension_ARG rf1) ;; 

let rf2 (B b) (S n) = 
  [n] 
 ;;
let check_rf2 = partial_check_by_d 2 (Qpe_extension_ARG rf2) ;; 


(* RFI BEGIN *)

let rfi (B _b) (S n) = 
  if n=4 then [1;2;4] else 
    [n] 
  ;;
  
(* RFI END *)
let check_rfi = global_check_by_d (Qpe_extension_ARG rfi) ;; 
