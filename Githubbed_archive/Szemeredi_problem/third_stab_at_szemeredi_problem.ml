(*

#use "Githubbed_archive/Szemeredi_problem/third_stab_at_szemeredi_problem.ml" ;;

A more in-depth analysis of the width=2 case.

*)

open Needed_values ;;

let current_width = 2 ;; 
let current_maxwidth = Sz_max_width_t.MW current_width ;;

module Prelude = struct 

type t = P of string ;; (* usually with length 2*current_width, in fact *)

let to_string (P s) =s ;;
let of_string s = 
    let d = String.length s - (2*current_width) in 
    if d < 0 
    then P((String.make (-d) '0')^s)
    else P(Cull_string.ending (2*current_width) s) ;;

let to_int_list (P s) =
    let temp1 = Ennig.index_everything (Strung.explode s) in 
    Option.filter_and_unpack (fun (idx,c)->
       if c='1' then Some idx else None) temp1 ;;  

let is_admissible prelude =
  Sz_preliminaries.test_for_admissibility 
    current_maxwidth (to_int_list(prelude)) ;;

let forces_next_bit_to_be_zero (P s) =
    not(is_admissible(P(s^"1"))) ;;
    
let concat (P s) t = of_string (s^t) ;;


let base = 
    let temp1 = Ennig.doyle (fun _->["0";"1"] ) 1 (2*current_width) in 
    let temp2 = Cartesian.general_product temp1 in 
    Option.filter_and_unpack (
       fun l->
          let prelude = P (String.concat "" l) in 
          if is_admissible prelude 
          then Some prelude 
          else None  
    ) temp2 ;;  

end ;;  


let patient_measure1 =Memoized.recursive(fun old_f (prelude,x) ->
    let n = String.length x in 
    if n = 0 then 0 else 
    match Option.seek 
      (fun j->(String.get x (j-1))<>'0') (Ennig.ennig 1 n) with 
    None -> 0  
    |Some j1 ->
       let prelude1 = Prelude.concat prelude  (Cull_string.beginning (j1-1) x) 
       and x1 = Cull_string.cobeginning (j1-1) x in 
       let shortened_x1 = Cull_string.cobeginning 1 x1 in 
       if Prelude.forces_next_bit_to_be_zero prelude1
       then old_f(Prelude.concat prelude1 "0",shortened_x1)      
       else 
      if String.get x1 0 = '1'
      then 1+old_f(Prelude.concat prelude1 "1",shortened_x1)      
      else  
      max (old_f(Prelude.concat prelude1 "0",shortened_x1))
          (1+old_f(Prelude.concat prelude1 "1",shortened_x1))       
) ;;  
   
let tf1 n = patient_measure1 (Prelude.of_string "0000",String.make n 'F') ;;

let u1 = Ennig.doyle tf1 1 30 ;; 