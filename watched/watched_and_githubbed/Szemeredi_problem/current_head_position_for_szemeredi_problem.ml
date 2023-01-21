(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_head_position_for_szemeredi_problem.ml" ;;

*)


(* Reproduced stab starts here *)(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)

open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz_types ;; 
open Sz_preliminaries_for_stab ;;
open Example ;;

let tf1 b n =
   Bulk_result.superficial_part (compute_bulk_result (P(2,b,n,[]))) ;;

let vvsu3 b n = 
  if b+4>=n then vsu3 n else 
  if b>0 
  then Contraction (P (2, b-1, n, []), C [b; b+2; b+4])
  else 
  (* if we get here, b=0 *)
  if (n mod 3)=0
  then  Fork
      [(P (1, n-5, n-3, []), [n-1; n]);
       (P (1, n-4, n-2, []), [n]);
       (P (1, n-3, n-1, []), [])]    
  else Contraction (P (1, n-3, n, []), C [n-2; n-1; n]);;

let check_vvsu3 = 
    let bound = 30 in 
    let all_pairs = Cartesian.product 
        (Int_range.range 0 bound) (Int_range.range 1 bound) in     
    let temp1 = Image.image (
        fun (b,n)->
        let sr = Bulk_result.superficial_part
         (compute_bulk_result (P(2,b,n,[]))) in     
        ((b,n),(sr,vvsu3 b n))
      ) all_pairs in 
    List.filter (fun (p,(x,y))->x<>y) temp1 ;; 

let u1 = Image.image fst check_vvsu3 ;; 






(* when g=5 *)

let tf2 n = tf1 (n-5+5) n ;;   

let rtf2 n = 
   match List.assoc_opt n
    [ 
    1,Atomic;  
    2,Atomic;  
    3,  Fork
    [(Empty_point, [2; 3]);
     (Empty_point, [1; 3]);
     (Empty_point, [1; 2])] ; 
    4, Contraction (P (1, 1, 4, []), C [n-2; n-1; n])] with 
   Some answer -> answer
   | None -> 
   Contraction (P (2, n-5, n, []), C [n-4; n-2; n]) ;; 

let check_rtf2 =
   let temp1 = Image.image  (fun k->(k,(tf2 k,rtf2 k))) 
     (Int_range.range 1 30) in 
   List.filter (fun (k,(x,y))->y<>x) temp1;;

(* when g=4 *)

let tf2 n = tf1 (n-5+4) n ;;   

let rtf2 n = 
   let b = n-5+4 in 
   match List.assoc_opt n
    [ 
    1,Atomic;  
    2,Atomic;  
    3,  Fork
    [(Empty_point, [2; 3]);
     (Empty_point, [1; 3]);
     (Empty_point, [1; 2])] ; 
    4, Contraction (P (1, 1, 4, []), C [n-2; n-1; n])] with 
   Some answer -> answer
   | None -> 
   Contraction (P (2, n-5, n, []), C [n-4; n-2; n]) ;; 

let check_rtf2 =
   let temp1 = Image.image  (fun k->(k,(tf2 k,rtf2 k))) 
     (Int_range.range 1 30) in 
   List.filter (fun (k,(x,y))->y<>x) temp1;;

(* when g=3 *)

let tf2 n = tf1 (n-5+3) n ;;   

let rtf2 n = 
   let b = n-5+3 in 
   match List.assoc_opt n
    [ 
    2,Atomic;  
    3,  Fork
    [(Empty_point, [2; 3]);
     (Empty_point, [1; 3]);
     (Empty_point, [1; 2])] ; 
    4, Contraction (P (1, 1, 4, []), C [n-2; n-1; n])] with 
   Some answer -> answer
   | None -> 
   Contraction (P (2, n-5, n, []), C [n-4; n-2; n]) ;; 

let check_rtf2 =
   let temp1 = Image.image  (fun k->(k,(tf2 k,rtf2 k))) 
     (Int_range.range 2 30) in 
   List.filter (fun (k,(x,y))->y<>x) temp1;;

(* when g=2 *)

let tf2 n = tf1 (n-5+2) n ;;   

let rtf2 n = 
   let b = n-5+2 in 
   match List.assoc_opt n
    [ 
    3,  Fork
    [(Empty_point, [2; 3]);
     (Empty_point, [1; 3]);
     (Empty_point, [1; 2])] ; 
    4, Contraction (P (1, 1, 4, []), C [n-2; n-1; n])] with 
   Some answer -> answer
   | None -> 
   Contraction (P (2, n-5, n, []), C [n-4; n-2; n]) ;; 

let check_rtf2 =
   let temp1 = Image.image  (fun k->(k,(tf2 k,rtf2 k))) 
     (Int_range.range 3 30) in 
   List.filter (fun (k,(x,y))->y<>x) temp1;;

(* when g=1 *)

let tf2 n = tf1 (n-5+1) n ;;   

let rtf2 n = 
   let b = n-5+1 in 
   match List.assoc_opt n
    [ 4, Contraction (P (1, 1, 4, []), C [n-2; n-1; n])] with 
   Some answer -> answer
   | None -> 
   Contraction (P (2, n-5, n, []), C [n-4; n-2; n]) ;; 

let check_rtf2 =
   let temp1 = Image.image  (fun k->(k,(tf2 k,rtf2 k))) 
     (Int_range.range 4 30) in 
   List.filter (fun (k,(x,y))->y<>x) temp1;;