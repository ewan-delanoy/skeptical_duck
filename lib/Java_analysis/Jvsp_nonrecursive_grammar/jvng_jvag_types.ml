(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_jvag_types.ml";;

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
