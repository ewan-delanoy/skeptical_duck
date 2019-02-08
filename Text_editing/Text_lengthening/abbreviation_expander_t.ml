(*

#use "Text_editing/Text_lengthening/abbreviation_expander_t.ml";;

*)

type t={
   mutable engine : Text_lengthener_t.t ;
   mutable cursor : int;
   incoming : (string option) array;
   outcoming : (string option) array; 
};;