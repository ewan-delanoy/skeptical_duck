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


open Qpe_extension_upper_half_mode ;;


let vz1 = visualize 1 ;; 
let rf1 (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 3);2,[1;2]@(Int_range.range 4 4)] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;
let check_rf1 = partial_check 1 rf1 ;; 

let vz2 = visualize 2 ;; 
let rf2 (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 (n-b+1));2,[1;2]@(Int_range.range 4 (n-b+2))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;
let check_rf2 = partial_check 2 rf2 ;; 

let vz3 = visualize 3 ;; 
let rf3 (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 (n-b+1));2,[1;2]@(Int_range.range 4 (n-b+2))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;
let check_rf3 = partial_check 2 rf3 ;; 



(* RFI BEGIN *)

let rfi (B b) (S n) = 
  match List.assoc_opt b
  [1,1::(Int_range.range 3 (n-b+1));2,[1;2]@(Int_range.range 4 (n-b+2))] with 
  Some answer -> answer 
  |None -> Int_range.range (b+2) n ;;  

(* RFI END *)
let check_rfi = global_check rfi ;; 
