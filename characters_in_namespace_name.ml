(*

#use"characters_in_namespace_name.ml";;

*)


let chars=
  (Int_range.doyle char_of_int 65 90)@
  (Int_range.doyle char_of_int 97 122)@
  (Int_range.doyle char_of_int 48 57)@
  ['\\';'_'];;
           