(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_grammar.ml";;

*)

type form = 
    Optional of Jvng_duplicated_name.t 
   |Molecular of Jvsp_types.token_type list
   |Concat of Jvng_duplicated_name.t list 
   |Disjunction of Jvng_duplicated_name.t list 
   |Star of Jvng_duplicated_name.t 
   |Synonym of Jvng_duplicated_name.t
  ;;

type t = AL of (Jvng_duplicated_name.t * form) list ;; 

(*

module Private = struct 

module Modify = struct 

   let order_on_pairs = Total_ordering.product str_order Jvag_form.order ;;

   let add_pair_naively pair (AL l) = 
  let (name,_form) = pair in 
  let new_l = (
    match List.assoc_opt name l with 
    None -> pair :: l 
    |Some _ -> Image.image (fun pair2->
      if (fst pair2)=name then pair else pair2 ) l
  )  in 
  AL(Ordered.sort order_on_pairs new_l);; 

end ;;    

end ;;   

let add_pair_naively = Private.Modify.add_pair_naively ;;

*)
let get_opt (AL l) name = List.assoc_opt name l ;;