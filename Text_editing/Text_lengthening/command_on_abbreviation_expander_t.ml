(*

#use "Text_editing/Text_lengthening/command_on_abbreviation_expander_t.ml";;

*)

type t=
    Add_newline
   |Add_words of string list
   |Do_nothing 
   |Insert_adjustment of string * string * (string * string * string)
   |Insert_decompression of string * string
   |Insert_expansion of string list
   |Insert_inert_word of string
   |Insert_left_core_abbreviation of string * string
   |Insert_prefix_abbreviation of string * string
   |Remove_adjustment of string * string * (string * string * string)
   |Remove_decompression of string * string
   |Remove_expansion of string list
   |Remove_inert_word of string
   |Remove_left_core_abbreviation of string * string
   |Remove_prefix_abbreviation of string * string;;


