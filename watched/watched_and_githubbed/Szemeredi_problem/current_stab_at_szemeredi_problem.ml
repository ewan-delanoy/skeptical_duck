(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz3_preliminaries ;;

let ff n = Level1.needed_computations (FIS(n,[])) ;; 

let fis_domain = FIS(3,[]) ;; 
let long1 = Level1.needed_computations fis_domain ;; 
let helper = [] ;; 
let to_be_treated = [fis_domain] ;; 
let long2 = Level1.helper_for_needed_computations (helper,to_be_treated) ;;

let (fis_domain,others) = Listennou.ht to_be_treated ;; 
let (opt_answer,should_remember,to_be_treated_later) = 
   Level1.full_pusher_in_computation helper fis_domain ;;
let (opt_easy_case,opt_cstr_data) =  Level1.partial_pusher_in_computation helper fis_domain ;;   
let (fis_tail,n) = Finite_int_set.tail_and_head fis_domain ;;
       let (opt_easy_case2,_opt_cstr_data2) =  partial_pusher_in_computation helper fis_tail in 
       end_of_full_pusher 
         helper fis_domain fis_tail n 
         opt_easy_case2 opt_cstr_data ;;