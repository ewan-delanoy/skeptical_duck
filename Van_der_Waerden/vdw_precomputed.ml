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


let restricted_power_set =Memoized.recursive (fun old_f (max_width,soi) ->
  if soi = [] 
  then [[]]  
  else 
  let temp1 = List.rev soi in 
  let (last_elt,temp2) = Listennou.ht temp1 in 
  let soi2 = List.rev temp2 in 
  Vdw_max_width.extender max_width (old_f (max_width,soi2)) last_elt  
);;

let act1 () = Ennig.doyle (fun n->
  restricted_power_set (Vdw_max_width_t.MW 4,Ennig.ennig 1 n)
) 1 24;;

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


