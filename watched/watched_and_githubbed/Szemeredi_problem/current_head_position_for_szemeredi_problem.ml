(*

#use "Githubbed_archive/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;

*)

open Skeptical_duck_lib ;; 
open Sz_types ;; 
open Sz_preliminaries_for_stab ;;




(*  
let current_width = 2 
and current_strappers = [] ;;
let tg b n = force_compute (P(current_width,b,n,current_strappers)) ;;
let tt n = tg (n-4) n;;

let (new_addition_to_be_made,all_data) = usual_zoom();;


let uu n = force_compute (P(3,0,n,[])) ;;
let aa n =
   let (BR(opt1,_)) = uu n in 
   let (hook,AI l) = Option.unpack opt1 in 
   l ;;    
let bb n =
    let (BR(opt1,M(sols,qlist))) = uu n in 
    (sols,qlist);;       

let tu n = let tv=tt n and uv=uu n in (tv=uv,(tt n,uu n));;


let g1 = needed_subcomputations_for_single_computation (P(4,0,8,[])) ;;

let g2 = needed_subcomputations_for_single_computation (P(3,1,7,[])) ;;

add_enhancement_data
 (P (1, 5, 7, []),
 [(Q (P (1, 2, 4, []), [], [6; 7]), [[1; 3; 4; 6; 7]])]);;

add_enhancement_data
 (P (1, 8, 10, []),
 [(Q (P (1, 5, 7, []), [], [9; 10]), [[1; 3; 4; 6; 7; 9; 10]])]);; 

add_enhancement_data
 (P (1, 11, 13, []),
   [(Q (P (1, 8, 10, []), [], [12; 13]), [[1; 3; 4; 6; 7; 9; 10; 12; 13]])]);; 

let gg q =    
   (P (1, 3*q-1, 3*q+1, []),
 [(Q (P (1, 3*q-4, 3*q-2, []), [], [3*q; 3*q+1]), [Parametrized_Example.sf2 (3*q+1)])]) ;;


test1_for_enhancement (P (1, 2, 4, []));;

test1_for_enhancement (P (1, 5, 7, []));;

get_enhancements_opt (P (1, 2, 4, []));;

get_enhancements_opt (P (1, 5, 7, []));;
*)
