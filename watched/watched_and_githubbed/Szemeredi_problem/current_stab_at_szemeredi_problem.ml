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



let vz = visualize 1 ;; 
let rf1 (B _b) (S n) = 
  if n<=2 then 0 else  3-(n mod 3) ;; 
let check_rf1 = partial_check 1 (Qpl_length_ARG rf1) ;; 

let rf2 (B _b) (S n) = 
  if n<=2 then 0 else  3-(n mod 3) ;; 
let check_rf2 = partial_check 2 (Qpl_length_ARG rf2) ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
  if n<=2 then 0 else  3-(n mod 3) ;; 

(* RFI END *)
let check_rfi = Chronometer.it global_check (Qpl_length_ARG rfi) ;; 
