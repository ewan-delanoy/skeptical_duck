(*

#use"lib/characters_in_namespace_name.ml";;

*)


let chars=
  (Int_range.scale char_of_int 65 90)@
  (Int_range.scale char_of_int 97 122)@
  (Int_range.scale char_of_int 48 57)@
  ['\\';'_'];;
           