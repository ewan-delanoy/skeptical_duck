(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)


open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz3_preliminaries ;;

let see0 = Overall.get_status () ;; 

let unvr = function (VR1 l)->l | _ -> failwith("aaa") ;; 

let res1 = unvr(Overall.next_look 1 ()) ;; 
let rf1 (B b,S n) =
  match List.assoc_opt n 
       [
        2,Atomic;
        3,Fork [(Empty_point, [2; 3]); (Empty_point, [1; 3]); (Empty_point, [1; 2])]]  with    
      Some answer -> answer 
   | None ->
    (match (n mod 3) with 
    0 -> Fork([( P(1,[],B(n-5),S(n-3)),[n-1;n] );
               ( P(1,[],B(n-4),S(n-2)),[n] );
               ( P(1,[],B(n-3),S(n-1)),[] )])
   |1-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] )  
   |2-> Contraction( P(1,[],B(n-3),S(n)),C[n-2;n-1;n] ) 
   |_-> failwith("bad remainder by 3")) ;;         
let check_rf1 = List.filter (fun (x,y)->y<> rf1 x) res1 ;;     


let res2 = unvr(Overall.next_look 2 ()) ;; 
let rf2 (B b,S n) =
  match List.assoc_opt n 
       [
        3,Atomic;
      ]  with    
      Some answer -> answer 
   | None ->
    Decomposable (P (1, [], B (n-3), S (n-1)), [n]) ;;         
let check_rf2 = List.filter (fun (x,y)->y<> rf2 x) res2 ;;  

let res3 = unvr(Overall.next_look 3 ()) ;; 
let rf3 (B b,S n) =
  match List.assoc_opt n 
       [
        4,Atomic;
      ]  with    
      Some answer -> answer 
   | None ->
    Decomposable (P (1, [], B (n-4), S (n-2)), [n-1;n]) ;;         
let check_rf3 = List.filter (fun (x,y)->y<> rf3 x) res3 ;;  
  
let res4 = unvr(Overall.next_look 4 ()) ;; 
let rf4 (B b,S n) =
  match List.assoc_opt n 
       [
        5,Atomic;
      ]  with    
      Some answer -> answer 
   | None ->
    Decomposable (P (1, [], B (b), S (b+2)), Int_range.range (b+3) n) ;;         
let check_rf4 = List.filter (fun (x,y)->y<> rf4 x) res4 ;;  

 
let res5 = unvr(Overall.next_look 5 ()) ;; 
let rf5 (B b,S n) =
  match List.assoc_opt n 
       [
        6,Atomic;
      ]  with    
      Some answer -> answer 
   | None ->
    Decomposable (P (1, [], B (b), S (b+2)), Int_range.range (b+3) n) ;;         
let check_rf5 = List.filter (fun (x,y)->y<> rf5 x) res5 ;;  



let g1 = CR1[] (* Verify.check 
(fst(Warehouse.pair_for_superficial_result_upper_half))
(1,[],IMD 0) (CE1 f_1_empty_set_superficial_result_upper_half) *) ;; 
let g2 =(function (CR1 x)->x |_->failwith("bbb")) g1 ;;
let g3 = Image.image (fun (a,b,c)->a) g2;;
let g4 = Image.image (fun (B b,S n)->n-b-1) g3;;

Lines_in_string.change_indentation_in_interval_in_file ~indent:0 (395,419)
  (Absolute_path.of_string "lib/Szemeredi/sz3_preliminaries.ml");;


(* k=4u: SOLUTION LISTS VERSION *)



let simplest_example = Fill_Warehouse.simplest_example ;;
let simplest_list = Fill_Warehouse.simplest_list ;;


let unvr = function (VR2 l)->l | _ -> failwith("aaa") ;; 
let res2 = unvr(Overall.next_look 2 ()) ;; 
let rf2 (B b,S n) = 
   if ((n mod 3)=0)
   then [simplest_example(n-1)@[n]]
   else simplest_list n ;;         
let check_rf2 = List.filter (fun (x,y)->y<> rf2 x) res2 ;;  

let res3 = unvr(Overall.next_look 3 ()) ;; 
let rf3 (B b,S n) = 
   if ((n mod 3)=0) then [simplest_example(n-1)@[n]] else 
   if ((n mod 3)=1) then [simplest_example(n-5)@(Int_range.range (n-3) n)] else  
   simplest_list n ;;         
let check_rf3 = List.filter (fun (x,y)->y<> rf3 x) res3 ;; 

let res4 = unvr(Overall.next_look 4 ()) ;; 
let rf4 (B b,S n) = 
   if ((n mod 3)=0) then [simplest_example(n-1)@[n]] else 
   if ((n mod 3)=1) then [simplest_example(n-5)@(Int_range.range (n-3) n)] else  
                         [simplest_example(n-6)@(Int_range.range (n-4) n)] ;;         
let check_rf4 = List.filter (fun (x,y)->y<> rf4 x) res4 ;; 

let res5 = unvr(Overall.next_look 5 ()) ;;
let rf5 (B b,S n) = 
  if ((n mod 3)=0) then [simplest_example(n-7)@(Int_range.range (n-5) n)] else 
  if ((n mod 3)=1) then [simplest_example(n-5)@(Int_range.range (n-3) n)] else  
                        [simplest_example(n-6)@(Int_range.range (n-4) n)] ;;         
let check_rf5 = List.filter (fun (x,y)->y<> rf5 x) res5 ;; 
 

let res6 = unvr(Overall.next_look 6 ()) ;;
let rf6 (B b,S n) = 
  if ((n mod 3)=0) then [simplest_example(b)@(Int_range.range (b+2) n)] else 
  if ((n mod 3)=1) then [simplest_example(b-1)@(Int_range.range (b+1) n)] else  
                        [simplest_example(b+1)@(Int_range.range (b+3) n)] ;;         
let check_rf6 = List.filter (fun (x,y)->y<> rf6 x) res6 ;; 

let res7 = unvr(Overall.next_look 7 ()) ;;
let rf7 (B b,S n) = 
  if ((n mod 3)=0) then [simplest_example(b)@(Int_range.range (b+2) n)] else 
  if ((n mod 3)=1) then [simplest_example(b-1)@(Int_range.range (b+1) n)] else  
                        [simplest_example(b+1)@(Int_range.range (b+3) n)] ;;         
let check_rf7 = List.filter (fun (x,y)->y<> rf7 x) res7 ;; 






let f_1_empty_set_solution_list_upper_half (B _b) (S n) = 
  if ((n mod 3)=0)
    then [simplest_example(n-1)@[n]]
    else simplest_list n ;;   

    
(*
    
  Verify.check 
    (fst(Warehouse.pair_for_solution_list_upper_half))
    (1,[],IMD 0) (CE2 f_1_empty_set_solution_list_upper_half) ;; 
    
*)

    
Hashtbl.add 
  (snd(Warehouse.pair_for_solution_list_upper_half)) (1,[]) 
    f_1_empty_set_solution_list_upper_half;;


let g1 = Verify.check 
    (fst(Warehouse.pair_for_solution_list_upper_half))
    (1,[],IMD 0) (CE2 f_1_empty_set_solution_list_upper_half) ;; 
let g2 =(function (CR2 x)->x |_->failwith("bbb")) g1 ;;
let g3 = Image.image (fun (a,b,c)->a) g2;;
let g4 = Image.image (fun (B b,S n)->n-b-1) g3;;


