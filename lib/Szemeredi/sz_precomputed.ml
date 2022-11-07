(*

#use"lib/Szemeredi/sz_precomputed.ml";;


*)

exception Untreated_width of int ;;

module Private = struct 

let measure_in_width_three n =
    if n<1 then 0 else 
    let q=(n/8) in 
    match n mod 8 with
     0 -> 4*q
    |1 -> 4*q+1
    |2 -> 4*q+2  
    |3 -> 4*q+2
    |4 -> 4*q+3
    |5 -> 4*q+4
    |6 -> 4*q+4  
    |7 -> 4*q+4
    | _ -> failwith("unforeseen");;  

let measure_in_width_four n =
    if n<1 then 0 else 
    let q=(n/9) in 
    match n mod 9 with
      0 -> 4*q+1 
    |1 -> 4*q+1
    |2 -> 4*q+2  
    |3 -> 4*q+2
    |4 -> 4*q+3
    |5 -> 4*q+4
    |6 -> 4*q+4  
    |7 -> 4*q+4
    |8 -> 4*q+4 
    | _ -> failwith("unforeseen");;     

let lower_measure_in_width_four n =
      if n<1 then 0 else 
      let q=(n/9) in 
      match n mod 9 with
        0 -> 4*q
      |1 -> 4*q
      |2 -> 4*q 
      |3 -> 4*q
      |4 -> 4*q+1
      |5 -> 4*q+1
      |6 -> 4*q+2  
      |7 -> 4*q+2
      |8 -> 4*q+3 
      | _ -> failwith("unforeseen");;  


end ;;

let measure (Sz_max_width_t.MW mw) n=
match mw with 
  3 -> Private.measure_in_width_three n 
| 4 -> Private.measure_in_width_four n 
| _ -> raise( Untreated_width mw);;

let lower_measure (Sz_max_width_t.MW mw) n=
match mw with 
4 -> Private.lower_measure_in_width_four n 
| _ -> raise( Untreated_width mw);;  




