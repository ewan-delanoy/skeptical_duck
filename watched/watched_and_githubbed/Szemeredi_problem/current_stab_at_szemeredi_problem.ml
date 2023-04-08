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
  simplest_list n ;; 
let check_rf1 = partial_check 1 (Solution_list_ARG rf1) ;; 

let rf2 (B _b) (S n) = 
  simplest_list n ;; 
let check_rf2 = partial_check 2 (Solution_list_ARG rf2) ;; 



(* RFI BEGIN *)

let rfi (B _b) (S n) = 
  simplest_list n ;; 

(* RFI END *)
let check_rfi = Chronometer.it global_check (Solution_list_ARG rfi) ;; 
