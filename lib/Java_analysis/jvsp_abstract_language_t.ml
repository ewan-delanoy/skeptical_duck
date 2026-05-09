(*

#use"lib/Java_analysis/jvsp_abstract_language_t.ml";;

*)


type element_in_concat = 
   Ref of string |Atomic of Jvsp_types.token_type | Star of string ;;

type element_in_disjunction = Concat of element_in_concat list ;;
     
type form = Disjunction of element_in_disjunction list ;;

type t = AL of (string * form) list ;; 