(*

#use "Text_editing/Text_lengthening/txl_result_t.ml";;

*)


type  t=
    Builtin_inert of string
   |Declared_inert of string
   |Newline 
   |Usual of ((string * string) option * (string * string) option *
      (string list * string) option * string list * string);;

