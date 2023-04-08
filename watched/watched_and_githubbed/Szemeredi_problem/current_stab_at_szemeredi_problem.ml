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


Chronometer.it (Int_range.scale visualize_by_b 0) 9 ;; 



let rf1 (B b) (S _n) = 
  if (b=0)||(b=1) then [] else 
    C[b-1;b+1] :: (Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-5))
  ;;  
let check_rf1 = partial_check 1 (Qpe_constraints_ARG rf1) ;; 


let rf2 (B b) (S _n) = 
  if (b=0) then [] else 
    C[b;b+2] :: (Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-4))
  ;; 
let check_rf2 = partial_check 2 (Qpe_constraints_ARG rf2) ;; 


let rf3 (B b) (S n) = 
  if (b=0)||(b=1) then [] else 
    Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-2) ;;   
let check_rf3 = partial_check 3 (Qpe_constraints_ARG rf3) ;; 

let rf4 (B b) (S n) = 
  if (b=0) then [] else 
    Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1) ;;   
let check_rf4 = partial_check 4 (Qpe_constraints_ARG rf4) ;; 

let rf5  = rf4 ;;   
let check_rf5 = partial_check 5 (Qpe_constraints_ARG rf5) ;; 

let rf6 (B b) (S n) = 
  if n=9 then [] else 
    Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (n-10) ;;   
let check_rf6 = partial_check 6 (Qpe_constraints_ARG rf6) ;; 

let rf7 (B b) (S n) = 
  if b=0 then [] else 
  if b=1 then (
                  if (n=5)||(n=7) then [] else
                  if n=6 then [C[1;3]] else 
                  [C[1;3;5]]   
                ) else  [] ;;   
let check_rf7 = partial_check 7 (Qpe_constraints_ARG rf7) ;; 

(* RFI BEGIN *)

let rfi (B b) (S n) =  
  if b=0  
  then [] 
  else 
  if (b=1)
  then (
         if (n=5)||(n=7) then [] else 
         if n=6 then [C[1;3]] else 
          Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1)
       ) 
  else    
  if (b=2)
      then (
             if (n=6)||(n=7) then [C[n-5;n-3]] else 
             if n=8 then [C[1;3;5]] else 
              Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1)
           ) 
  else      
  if b=3
  then (
          if n<=7 then [] else 
          Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-4))  
  else Int_range.scale (fun j->C[j+1;j+3;j+5]) 0 (b-1)
;;

(* RFI END *)
let check_rfi = Chronometer.it global_check (Qpe_constraints_ARG rfi) ;; 

