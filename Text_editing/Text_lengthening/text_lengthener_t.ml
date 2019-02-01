(*

#use "Text_editing/Text_lengthening/text_lengthener_t.ml";;

*)

type t={
  (* fundamental variables *)  
  adjustable_decompressions : (string * string * ((string * string * string) list)) list;  
  expansions : (string list list);
  inert_words : string list;
  left_core_abbreviations : (string*string) list;
  prefix_abbreviations : (string*string) list;

  (* derived variables *)
  case_insensitive_adjustable_decompressions : (string * string * ((string * string * string) list)) list;  
  case_insensitive_left_core_abbreviations  : (string*string) list;
  case_insensitive_inert_words : string list;
  case_insensitive_prefix_abbreviations  : (string*string) list;
};;