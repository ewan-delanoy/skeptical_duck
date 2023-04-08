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
let rf1 (B _b) (S n) = 
   Empty_point 
;; 
let check_rf1 = partial_check 1 (Qpe_core_ARG rf1) ;; 

let rf2 (B _b) (S n) = 
   Empty_point 
;; 
let check_rf2 = partial_check 2 (Qpe_core_ARG rf2) ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
   if n<=5 then Empty_point else
    P(1,[],B(n-5),S(n-3))
  ;; 

(* RFI END *)
let check_rfi = Chronometer.it global_check (Qpe_core_ARG rfi) ;; 