(*

#use "Text_editing/Text_lengthening/txl_field.ml";;

The arguments for setters are assumed to be already in order. 

*)

let set_decompressions x v=
  {x with 
    Text_lengthener_t.adjustable_decompressions =v ;
    Text_lengthener_t.case_insensitive_adjustable_decompressions = 
       French_capitalization.generalize_for_false_three v ;
  };;

let set_expansions x v=
  {x with 
    Text_lengthener_t.expansions =v ;
  };;

let set_inert_words x v=
  {x with 
    Text_lengthener_t.inert_words =v ;
    Text_lengthener_t.case_insensitive_inert_words = 
       French_capitalization.generalize_for_one v ;
  };;

let set_left_core_abbreviations x v=
  {x with 
    Text_lengthener_t.left_core_abbreviations =v ;
    Text_lengthener_t.case_insensitive_left_core_abbreviations = 
       French_capitalization.generalize_for_two v ;
  };;

let set_prefix_abbreviations x v=
  {x with 
    Text_lengthener_t.prefix_abbreviations =v ;
    Text_lengthener_t.case_insensitive_prefix_abbreviations = 
       French_capitalization.generalize_for_two v ;
  };;

(* Usual getters *)

let adjustable_decompressions x=x.Text_lengthener_t.case_insensitive_adjustable_decompressions;;
let expansions x=x.Text_lengthener_t.expansions;;
let inert_words x=x.Text_lengthener_t.case_insensitive_inert_words;;
let left_core_abbreviations x=x.Text_lengthener_t.case_insensitive_left_core_abbreviations;;
let prefix_abbreviations x=x.Text_lengthener_t.case_insensitive_prefix_abbreviations;;

(* Getters for a more internal use *)

let uncapitalized_adjustable_decompressions x=x.Text_lengthener_t.adjustable_decompressions;;
let uncapitalized_inert_words x=x.Text_lengthener_t.inert_words;;
let uncapitalized_left_core_abbreviations x=x.Text_lengthener_t.left_core_abbreviations;;
let uncapitalized_prefix_abbreviations x=x.Text_lengthener_t.prefix_abbreviations;;






