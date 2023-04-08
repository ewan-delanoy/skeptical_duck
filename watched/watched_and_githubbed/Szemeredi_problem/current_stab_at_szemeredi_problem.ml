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

let vz = visualize_by_b ;;
Chronometer.it (Int_range.scale vz 0) 9 ;; 



let rf0 (B b) (S _n) = 
  Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1)
  ;;  
let check_rf0 = partial_check_by_b 0 (Qpe_constraints_ARG rf0) ;; 

let rf1 (B b) (S n) = 
  if List.mem n [5;7] then [] else 
  if n=6 then [C[1;3]] else   
  Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1)
  ;;  
let check_rf1 = partial_check_by_b 1 (Qpe_constraints_ARG rf1) ;; 

let rf2 (B b) (S n) = 
  if List.mem n [6;7] then [C[n-5;n-3]] else 
  if n=8 then Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2) else  
  Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1)
  ;; 
let check_rf2 = partial_check_by_b 2 (Qpe_constraints_ARG rf2) ;; 


let rf3 (B b) (S n) = 
  if List.mem n [7;8] then [C[n-5;n-3]] else 
    if n=9 then Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2) else  
    Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1) 
  ;;   
let check_rf3 = partial_check_by_b 3 (Qpe_constraints_ARG rf3) ;; 

let rf4 (B b) (S n) = 
  if n=b+4 then [C[3;5]] else 
  if n=b+5 then [C[4;6];C[1;3;5]] else   
  if n=b+6 then Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2) else 
    Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1) ;;   
let check_rf4 = partial_check_by_b 4 (Qpe_constraints_ARG rf4) ;; 

let rf5 (B b) (S n) = 
  if n=b+4 then [C[4;6];C[1;3;5]] else 
  if n=b+5 then [C[5;7];C[1;3;5];C[2;4;6]] else   
  if n=b+6 then Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2) else 
    Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1) ;;     
let check_rf5 = partial_check_by_b 5 (Qpe_constraints_ARG rf5) ;; 

let rf6 (B b) (S n) = 
  if n=b+4 then C[n-5;n-3]::(Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-5)) else 
  if n=b+5 then C[n-5;n-3]::(Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-4)) else   
  if n=b+6 then Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2) else 
    Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1) ;;   
let check_rf6 = partial_check_by_b 6 (Qpe_constraints_ARG rf6) ;; 

let rf7 = rf6 ;;   
let check_rf7 = partial_check_by_b 7 (Qpe_constraints_ARG rf7) ;; 

(* RFI BEGIN *)

let rfi (B b) (S n) =  
    if n=b+4 then (if b<=1 then [] else C[n-5;n-3]::(Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-5))) else 
    if n=b+5 then (if b=0  then [] else C[n-5;n-3]::(Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-4)) else   
    if n=b+6 then Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2) else 
      Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1) ;;   

(* RFI END *)
let check_rfi = Chronometer.it global_check_by_b (Qpe_constraints_ARG rfi) ;; 

