(*

#use"lib/Java_analysis/jvag_types.ml";;

*)

     
type form = 
    Optional of string 
   |Molecular of Jvsp_types.token_type list
   |Concat of string list 
   |Disjunction of string list 
   |Star of string 
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

type line_in_magnifying_glass = MGL of (string * ((string * form) list)) ;; 

type magnifying_glass = MG of line_in_magnifying_glass list ;;
