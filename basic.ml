(*

#use"basic.ml";;

*)

module Private = struct 

  let frac_ceiling_for_positive_denominator a b=
  (*we assume that b is positive *)
  if (a mod b)=0 
  then (a/b) 
  else if (a>=0) 
       then (a/b)+1
       else -(( abs a)/b);;
    
let rec helper_for_power (a,b,accu) =
    if b<1 then accu else 
    if b=1 then a*accu else 
    let multiplier = (if b mod 2=0 then 1 else a) in 
    helper_for_power (a*a,b/2,multiplier * accu) ;;  

end ;;  


let fold_sum=function
[]->0
|a::b->List.fold_left(+)(a)(b);;

let frac_ceiling a b=
 if (b=0) 
 then failwith("division by zero in frac_ceiling")
 else if (b>0)
      then Private.frac_ceiling_for_positive_denominator a b
      else Private.frac_ceiling_for_positive_denominator (-a) (-b);;
  
let power a b = Private.helper_for_power (a,b,1) ;;

let sign x=if x<0 then -1 else if x=0 then 0 else 1;;

(*
let delta_list l=
let rec sub_f=
(function (accu,a,rl)->match rl with
[]->List.rev(accu)
|b::x->sub_f((b-a)::accu,b,x)
) in
match l with
[]->[]
|u::v->sub_f([],u,v);;
*)           