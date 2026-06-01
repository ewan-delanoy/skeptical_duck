(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_grammar.ml";;

*)

open Jvng_jvag_types ;;

exception Add_pair_fearfully_exn of Jvng_duplicated_name.t ;;

module Private = struct 

module Modify = struct 

   let order_on_pairs = Total_ordering.product Jvng_duplicated_name.order Jvng_jvag_form.order ;;

   let add_pair_fearfully pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> raise(Add_pair_fearfully_exn(fst pair))
  )  in 
  AL(Ordered.sort order_on_pairs new_l);; 

  

end ;;    

end ;;   

let add_pair_fearfully = Private.Modify.add_pair_fearfully ;;

let get_opt (AL l) name = List.assoc_opt name l ;;

let singleton name form = AL [name,form] ;;     
