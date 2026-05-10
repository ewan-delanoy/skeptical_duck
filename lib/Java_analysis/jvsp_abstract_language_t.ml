(*

#use"lib/Java_analysis/jvsp_abstract_language_t.ml";;

*)


type element_in_concat = 
   Ref of string |Optional of string;;

type element_in_disjunction = Concat of element_in_concat list ;;
     
type form = 
   Disjunction of element_in_disjunction list 
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_star of string 
  ;;

type t = AL of (string * form) list ;; 