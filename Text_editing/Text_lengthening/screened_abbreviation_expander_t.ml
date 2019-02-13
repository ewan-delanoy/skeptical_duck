(*

#use "Text_editing/Text_lengthening/screened_abbreviation_expander_t.ml";;

*)

type t={
      engine : Abbreviation_expander_t.t ;
      screen : Absolute_path.t 
};;