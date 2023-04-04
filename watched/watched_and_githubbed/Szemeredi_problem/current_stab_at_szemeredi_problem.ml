(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 


open Sz3_preliminaries ;;
let see0 = Overall.get_status () ;; 
open Tools_for_warehouse ;; 
(* open Solution_list_upper_half_mode ;; *)


(* RFI BEGIN *)

let rfi (B b) (S n) = 
  if List.mem (n-b,n mod 3) [(2,0);(2,1);(2,2);(3,1);(3,2);(4,2)] 
  then simplest_list n  
  else [(simplest_example (b+2))@(Int_range.range (b+3) n)] ;;    

(* RFI END *)
(* let check_rfi = global_check rfi ;; *)


(* let inactive1 = Solution_list_upper_half_mode.global_check rfi ;; *)
let c_data = Solution_list_upper_half_mode.current_data () ;; 
(* let inactive2 = Abstract_solution_list_mode.global_check c_data rfi ;; *)
let (w,s,i,half) = c_data ;;
let temp1 = Image.image (
  fun (b,n) -> ((b,n),Solution_list_seed.original (w,s,i) b n,rfi b n)
  ) (Abstract_solution_list_mode.Private.total_range (w,s,i,half)) ;; 
let temp2 = List.filter (fun (_,y1,y2)->y1<>y2) temp1 ;;
let temp3 = 
  (if temp2=[] 
   then [] 
   else
 snd(Min.minimize_it_with_care (fun (pair,_,_)->
    Range.compute_enumerator_index w pair half) temp2));;
let answer =(temp2,temp3)  ;;  


(* let inactive3 = Side_effects_after_successful_global_check.main (w,s,i,Solution_list_seed.current_component,half) ;; *)


