(*

#use"Van_der_Waerden/vdw_precomputed.ml";;


*)

exception Untreated_width of int ;;

module Private = struct 

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

let hashtbl_for_restricted_power_set = Hashtbl.create 4;;

let mw1 = Vdw_max_width_t.MW 4 
and n1 = Ennig.ennig 1 15 ;;

Hashtbl.add hashtbl_for_restricted_power_set (mw1,n1) 
  (Vdw_max_width.naive_restricted_power_set mw1 n1) ;;

let restricted_power_set pair =
   let (max_width,n) = pair in 
   match Hashtbl.find_opt hashtbl_for_restricted_power_set pair with
   Some(old_answer) -> old_answer
   | None ->
    let answer = Vdw_max_width.naive_restricted_power_set max_width n in 
    let _ = Hashtbl.add hashtbl_for_restricted_power_set pair answer in 
    answer ;;

end ;;

let measure (Vdw_max_width_t.MW mw) n=
match mw with 
4 -> Private.measure_in_width_four n 
| _ -> raise( Untreated_width mw);;

let lower_measure (Vdw_max_width_t.MW mw) n=
match mw with 
4 -> Private.lower_measure_in_width_four n 
| _ -> raise( Untreated_width mw);;  

let restricted_power_set = Private.restricted_power_set ;;


