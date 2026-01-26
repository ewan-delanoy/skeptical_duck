(*

#use"lib/basic.ml";;

*)

module Private = struct 

  let frac_ceiling_for_positive_denominator a b=
  (*we assume that b is positive *)
  if (a mod b)=0 
  then (a/b) 
  else if (a>=0) 
       then (a/b)+1
       else -(( abs a)/b);;
    
   let frac_floor_for_positive_denominator a b=
  (*we assume that b is positive *)
    if (a>=0) 
    then (a/b)
    else -((( abs a)/b)+1);;    

let rec helper_for_power (a,b,accu) =
    if b<1 then accu else 
    if b=1 then a*accu else 
    let multiplier = (if b mod 2=0 then 1 else a) in 
    helper_for_power (a*a,b/2,multiplier * accu) ;;  

end ;;  

let announce cmd =
    print_string (cmd ^ " ...\n\n");
    flush stdout
  ;;


let announce_execution cmd = announce ("Executing " ^ cmd) ;;
let fold_prod=function
[]->1
|a::b->List.fold_left( * )(a)(b);;

let fold_sum=function
[]->0
|a::b->List.fold_left(+)(a)(b);;

let frac_ceiling a b=
 if (b=0) 
 then failwith("division by zero in frac_ceiling")
 else if (b>0)
      then Private.frac_ceiling_for_positive_denominator a b
      else Private.frac_ceiling_for_positive_denominator (-a) (-b);;
let frac_floor a b=
 if (b=0) 
 then failwith("division by zero in frac_floor")
 else if (b>0)
      then Private.frac_floor_for_positive_denominator a b
      else Private.frac_floor_for_positive_denominator (-a) (-b);;  

let power a b = Private.helper_for_power (a,b,1) ;;

let sign x=if x<0 then -1 else if x=0 then 0 else 1;;

let rec smallest_in_range_satisfying_opt (a,b) f =
   if a>b then None else 
   if f a then Some a else 
   smallest_in_range_satisfying_opt (a+1,b) f;;  

let rec largest_in_range_satisfying_opt (a,b) f =
   if a>b then None else 
   if f b then Some b else 
   largest_in_range_satisfying_opt (a,b-1) f;;     