(*

#use "Text_editing/Text_lengthening/text_lengthener_field.ml";;

*)

let set_decompressions x v=
  {x with 
    Text_lengthener_t.decompressions =v ;
    Text_lengthener_t.case_insensitive_decompressions = 
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

