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

let rf1 (B _b) (S n) = 
   if n=4 then Empty_point else 
    P(1,[],B(n-4),S(n-2))
;; 
let check_rf1 = partial_check_by_d 1 (Qpe_core_ARG rf1) ;; 

let rf2 (B _b) (S n) = 
   Empty_point 
;; 
let check_rf2 = partial_check_by_d 2 (Qpe_core_ARG rf2) ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
    P(1,[],B(n-3),S(n-1))
  ;; 

(* RFI END *)
let check_rfi = Chronometer.it global_check_by_d (Qpe_core_ARG rfi) ;; 


