(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_head_position_for_szemeredi_problem.ml" ;;

*)


(* Reproduced stab starts here *)(*

#use "watched/watched_and_githubbed/Szemeredi_problem/current_stab_at_szemeredi_problem.ml" ;;


An attempt at creating an algorithm that (given enough time) can compute sytematically
any value of the Szemeredi function. 

*)

(*    
let generic_length n=
  if n<3 then 0 else 
    List.assoc (n mod 3) [0,3;1,2;2,1] ;;
let check_lengths = 
      let bound = 30 in 
      let all_pairs = Cartesian.product 
          (Int_range.range 0 bound) (Int_range.range 1 bound) in     
      let temp1 = Image.image (
          fun (b,n)->
          let (M(_,qpoints)) = Bulk_result.mold
           (compute_bulk_result (P(2,b,n,[]))) in     
          ((b,n),(List.length qpoints,generic_length n))
        ) all_pairs in 
      List.filter (fun (p,(x,y))->x<>y) temp1 ;; 
let u1 = Image.image fst check_lengths ;; 

*)



(*
let check_solutions = 
  let bound = 30 in 
  let all_pairs = Cartesian.product 
      (Int_range.range 0 bound) (Int_range.range 1 bound) in     
  let temp1 = Image.image (
      fun (b,n)->
      let (M(l_sol,_)) = Bulk_result.mold
       (compute_bulk_result (P(2,b,n,[]))) in     
      ((b,n),(l_sol,[Example.vso1 n]))
    ) all_pairs in 
  List.filter (fun (p,(x,y))->x<>y) temp1 ;; 
*)

let vvq2 b n= 
  let pt = ( if (b,n)=(0,5) 
             then ep 
             else P (1, n-5, n-3, []) 
  )
  and l_cstr= (
    if b<=n-7
    then (Int_range.scale (fun j->C[j;j+2;j+4]) 1 b)  
    else if b=n-6 
         then (Int_range.scale (fun j->C[j;j+2;j+4]) 1 (b-1))
         else (* b=n-5 *)
              (C [n-5; n-3])::(Int_range.scale 
              (fun j->C[j;j+2;j+4]) 1 (n-8))
  ) 
  and extra=(if (b,n)=(0,5) 
             then [1;2;4;5]
             else ) in 
  Q (pt,l_cstr,extra) ;; 

open Skeptical_duck_lib ;; 
open Needed_values ;; 
open Sz_types_for_third_stab ;; 
open Sz_preliminaries_for_stab ;;
open Example ;;

let tf1 b n =
   let (M(_,qpoints)) =
     Bulk_result.mold (compute_bulk_result (P(2,b,n,[]))) in 
    List.nth qpoints 0;;


(* when d=9 *)

let tf2 n = tf1 (n-9) n ;; 

let rtf2 n=
  let pt = P (1, n-5, n-3, []) 
  and l_cstr = (Int_range.scale 
  (fun j->C[j;j+2;j+4]) 1 (n-9)) in
Q (pt,l_cstr,[n-1;n]);;

let check_rtf2 = 
    let base = Int_range.range 9 30 in 
    let temp1 = Image.image (
      fun n->((n-9,n),(tf2 n,rtf2 n))
    ) base in 
    List.filter (fun (p,(x,y))->x<>y ) temp1 ;;

let see = List.hd(List.rev check_rtf2) ;; 

(* when d=8 *)

let tf2 n = tf1 (n-8) n ;; 

let rtf2 n=
  let pt = P (1, n-5, n-3, []) 
  and l_cstr = (Int_range.scale 
  (fun j->C[j;j+2;j+4]) 1 (n-8)) in
Q (pt,l_cstr,[n-1;n]);;

let check_rtf2 = 
    let base = Int_range.range 8 30 in 
    let temp1 = Image.image (
      fun n->((n-8,n),(tf2 n,rtf2 n))
    ) base in 
    List.filter (fun (p,(x,y))->x<>y ) temp1 ;;


(* when d=7 *)

let tf2 n = tf1 (n-7) n ;; 

let rtf2 n=
  let pt = P (1, n-5, n-3, []) 
  and l_cstr = (Int_range.scale 
  (fun j->C[j;j+2;j+4]) 1 (n-7)) in
Q (pt,l_cstr,[n-1;n]);;

let check_rtf2 = 
    let base = Int_range.range 7 30 in 
    let temp1 = Image.image (
      fun n->((n-7,n),(tf2 n,rtf2 n))
    ) base in 
    List.filter (fun (p,(x,y))->x<>y ) temp1 ;;


(* when d=6 *)

let tf2 n = tf1 (n-6) n ;; 

let rtf2 n=
  let pt = P (1, n-5, n-3, []) 
  and l_cstr = (Int_range.scale 
  (fun j->C[j;j+2;j+4]) 1 (n-7)) in
Q (pt,l_cstr,[n-1;n]);;

let check_rtf2 = 
    let base = Int_range.range 6 30 in 
    let temp1 = Image.image (
      fun n->((n-6,n),(tf2 n,rtf2 n))
    ) base in 
    List.filter (fun (p,(x,y))->x<>y ) temp1 ;;



(* when d=5 *)

let tf2 n = tf1 (n-5) n ;; 

let rtf2 n=
  match List.assoc_opt n 
  [
     5, Q (ep, [], [1; 2; 4; 5])
  ] with 
  Some answer -> answer
  | None ->
  let pt = P (1, n-5, n-3, []) 
  and l_cstr = (C [n-5; n-3])::(Int_range.scale 
  (fun j->C[j;j+2;j+4]) 1 (n-8)) in
Q (pt,l_cstr,[n-1;n]);;

let check_rtf2 = 
    let base = Int_range.range 5 30 in 
    let temp1 = Image.image (
      fun n->((n-5,n),(tf2 n,rtf2 n))
    ) base in 
    List.filter (fun (p,(x,y))->x<>y ) temp1 ;;



