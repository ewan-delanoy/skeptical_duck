(*

#use"lib/Java_analysis/jvsp_abstract_grammar_t.ml";;

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
  |Remove_productions of string list
  |Register_with_standardized_name of form 
  |Expand_in_disjunction of string * string 
  |Expand_in_synonym of string * string
  |Collapse_synonym_locally of string * string
  |Collapse_synonym_globally of string ;;

type nonrecursive_grammar = {
   sons_and_fathers : (string * string) list ;
   productions : (string * (form * string list)) list;
} ;;

type magnifying_glass_line = MGL of (string * ((string * form) list)) ;; 

type magnifying_glass = MG of 
  (string * ((string * form) list)) list 
 ;;
