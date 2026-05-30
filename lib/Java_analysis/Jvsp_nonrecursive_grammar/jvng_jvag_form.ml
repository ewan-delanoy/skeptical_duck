(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_form.ml";;

*)

open Jvng_jvag_types ;;

let order = (
   (fun form1 form2 ->Total_ordering.standard form1 form2): 
     form Total_ordering_t.t ) ;; 

let uniform_composition link l = match link with 
    Jvag_types.Optional_L -> Optional(List.hd l)
   |Jvag_types.Concat_L -> Concat(l)
   |Jvag_types.Disjunction_L -> Disjunction(l)
   |Jvag_types.Star_L -> Star(List.hd l)
   |Jvag_types.Synonym_L -> Synonym(List.hd l);;     

let uniform_decomposition_opt form = match form with
   |Molecular _ -> None    
   |Concat l ->  Some(Jvag_types.Concat_L,l)
   |Disjunction l -> Some(Jvag_types.Disjunction_L,l)
   |Star nm -> Some(Jvag_types.Star_L,[nm])
   |Optional nm -> Some(Jvag_types.Optional_L,[nm])
   |Synonym nm -> Some(Jvag_types.Synonym_L,[nm]) ;;   