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
   []
   ;;
let check_rf1 = partial_check 1 (Solution_list_ARG rf1) ;; 


let rf2 (B b) (S n) = 
  []
    ;; 
let check_rf2 = partial_check 2 (Solution_list_ARG rf2) ;; 


let rf3 (B b) (S n) = 
  []
    ;; 
let check_rf3 = partial_check 3 (Solution_list_ARG rf3) ;; 

let rf4 (B b) (S n) = 
   []
    ;; 
let check_rf4 = partial_check 4 (Solution_list_ARG rf4) ;; 


(* RFI BEGIN *)

let rfi (B _b) (S n) = 
   simplest_list n
    ;;   

(* RFI END *)
let check_rfi = Chronometer.it global_check (Solution_list_ARG rfi) ;; 
