(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/concrete_object_t.ml";;


*)

type t= 
    Int of int 
   |String of Encoded_string_t.t 
   |Uple of t list
   |List of t list
   |Array of t list
   |Record of (string*t) list
   |Variant of string*(t list);;

