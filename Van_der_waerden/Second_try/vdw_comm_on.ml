(*

#use"Van_der_Waerden/Second_try/vdw_comm_on.ml";;

*)

let measure n =
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

let lower_measure n =
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


let big_base =   
  let unordered_base = 
    Vdw_common.naive_restricted_power_set
   ( Vdw_list_of_constraints_t.Defined_by_max_width 4) 
     (Set_of_integers.safe_set(Ennig.ennig 1 15))
  in 
  Ordered.sort
  (Total_ordering.silex_compare Total_ordering.for_integers)  
  unordered_base ;;

