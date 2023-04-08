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
Chronometer.it (Int_range.scale vz 0) 9 ;; 


let rf1 (B b) (S n) = 
  if n=4 then [] else 
  if n=6 then [C[2;4]] else   
  C[n-4;n-2]::(Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-4))
  ;;  
let check_rf1 = partial_check_by_d 1 (Qpe_constraints_ARG rf1) ;; 

let rf2 (B b) (S n) = 
  Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2)
  ;; 
let check_rf2 = partial_check_by_d 2 (Qpe_constraints_ARG rf2) ;; 


let rf3 (B b) (S n) = 
  Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1)
  ;;   
let check_rf3 = partial_check_by_d 3 (Qpe_constraints_ARG rf3) ;; 


(* RFI BEGIN *)

let rfi (B b) (S n) =    
  if n=b+4
  then Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2)         
  else Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1) 
;;

(* RFI END *)
let check_rfi = Chronometer.it global_check_by_d (Qpe_constraints_ARG rfi) ;; 

