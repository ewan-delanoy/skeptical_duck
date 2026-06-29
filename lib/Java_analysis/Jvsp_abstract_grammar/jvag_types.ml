(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_types.ml";;

*)

     
type form = 
    Optional of string 
   |Molecular of Jvsp_types.token_type list
   |Concat of string list 
   |Disjunction of string list 
   |Star of string 
   |Synonym of string
  ;;

type grammar = AL of (string * form) list ;; 

type local_modification = 
   Lm_collapse_synonym of int 
  |Lm_detect_optional of int * (int * int)
  |Lm_expand_disjunction of int * int 
  |Lm_expand_synonym of int * int 
  |Lm_expand_concat of int * int  
  |Lm_explode_molecule of int * int 
  |Lm_implode_concat of int * (int * int)
  |Lm_implode_molecule of int * (int * int)  
  |Lm_reunite_star of int * (int * int)
  |Lm_reunite_disjunction of (int * int) * int
  |Lm_pumping_lemma of string * int 
;;

type modification = 
   Set_production of string * form
  |Create_production of string * form  
  |Rename of string * string 
  |Remove_productions of string list
  |Register_with_standardized_name of form 
  |Expand_in_disjunction of string * string 
  |Expand_in_synonym of string * string
  |Collapse_synonym_locally of string * string
  |Collapse_synonym_globally of string 
  |Local of string * (local_modification list) ;;

type nonrecursive_grammar = {
   sons_and_fathers : (string * string) list ;
   productions : (string * (form * string list)) list;
} ;;

type link =
    Optional_L 
   |Concat_L
   |Disjunction_L 
   |Star_L
   |Synonym_L
  ;;

type hand = {
   outer : string ;
   inner : string ;
   suffix : string list ;
   other_fingers : (string list) list ;
}  ;; 