(*

#use"lib/string_find_char.ml";;

*)

 
let from_inclusive_opt f s initial_idx=
   let n=(String.length s) in
   let rec tempf=(fun j->
     if j>n then None else
     if f(String.get s  (j-1)) then Some j else
     tempf(j+1)
   ) in
   tempf(initial_idx);;

(*

from_inclusive_opt (fun c->c='3') "123456789" 1 ;;

*)   

let backwards_from_inclusive_opt f s initial_idx=
    let rec tempf=(fun j->
      if j<1 then None else
      if f(String.get s (j-1)) then Some(j) else
      tempf(j-1)
    ) in
    tempf(initial_idx);;   
 
(*

backwards_from_inclusive_opt (fun c->c='3') "123456789" 7 ;;
backwards_from_inclusive_opt (fun c->c='7') "123456789" 7 ;;

*)   