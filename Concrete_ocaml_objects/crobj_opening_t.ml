(* 

#use"Concrete_ocaml_objects/crobj_opening_t.ml";;

For convenience, the list is reversed when the partial object is closed and becomes full
(new objects are therefore appended directly to the left on a partial object).


*)


type t= 
    Uple 
   |List 
   |Array 
   |Record of string
   |Variant of string;;

