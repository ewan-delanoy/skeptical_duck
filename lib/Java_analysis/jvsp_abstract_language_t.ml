(*

#use"lib/Java_analysis/jvsp_abstract_language_t.ml";;

*)

     
type form = 
    Just_an_optional of string 
   |Just_atomic of Jvsp_types.token_type list
   |Just_a_concat of string list 
   |Just_a_disjunction of string list 
   |Just_a_star of string 
   |Synonym of string
  ;;

type t = AL of (string * form) list ;; 

type modification = 
   Set_production of string * form 
  |Rename of string * string 
  |Remove_productions of string list;;