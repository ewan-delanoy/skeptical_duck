(*

#use"lib/Java_analysis/jvsp_abstract_language.ml";;

*)

type element_in_concat = Jvsp_abstract_language_t.element_in_concat = 
   Ref of string |Atomic of Jvsp_types.token_type | Star of string ;;

type element_in_disjunction = Jvsp_abstract_language_t.element_in_disjunction = 
   Concat of element_in_concat list ;;
     
type form =  Jvsp_abstract_language_t.form = Disjunction of element_in_disjunction list ;;

type t =  Jvsp_abstract_language_t.t = AL of (string * form) list ;; 