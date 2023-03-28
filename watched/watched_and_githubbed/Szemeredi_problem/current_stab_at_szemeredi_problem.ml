(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz3_preliminaries ;;

let see0 = Overall.get_status () ;; 

let unvr = function (VR3 l)->l | _ -> failwith("aaa") ;; 

let res1 = unvr(Overall.next_look 1 ()) ;; 
let rf1 (B b,S n) =
  match List.assoc_opt n 
     [1,0;2,0]  with    
    Some answer -> answer 
 | None ->
    (match (n mod 3) with 
    0 -> 3
   |1-> 2   
   |2-> 1  
   |_-> failwith("bad reminder by 3"));;       
let check_rf1 = List.filter (fun (x,y)->y<> rf1 x) res1 ;;     

let res2 = unvr(Overall.next_look 2 ()) ;; 
let rf2 (B b,S n) =
  match List.assoc_opt n 
     [1,0;2,0]  with    
    Some answer -> answer 
 | None ->
    (match (n mod 3) with 
    0 -> 3
   |1-> 2   
   |2-> 1  
   |_-> failwith("bad reminder by 3"));;       
let check_rf2 = List.filter (fun (x,y)->y<> rf2 x) res2 ;;   



let f_1_empty_set_qpl_length_lower_half (B _b) (S n) = match List.assoc_opt n 
[1,0;2,0]  with    
Some answer -> answer 
| None ->
(match (n mod 3) with 
0 -> 3
|1-> 2   
|2-> 1  
|_-> failwith("bad reminder by 3"));; 

    
(*
    
  Verify.check 
    (fst(Warehouse.pair_for_qpl_length_lower_half))
    (1,[],IMD 0) (CE3 f_1_empty_set_qpl_length_lower_half) ;; 
    
*)
    
Hashtbl.add 
  (snd(Warehouse.pair_for_qpl_length_lower_half)) (1,[]) 
    f_1_empty_set_qpl_length_lower_half;;

Lines_in_string.change_indentation_in_interval_in_file ~indent:0 (395,419)
  (Absolute_path.of_string "lib/Szemeredi/sz3_preliminaries.ml");;