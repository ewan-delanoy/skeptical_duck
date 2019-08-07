(* 

#use"Concrete_ocaml_objects/double_partial_concrete_object_t.ml";;

First argument says if a comma appears last, waiting for another item.

The two last arguments are t * (t list) rather than just t list, to enforce
a non-empty list. 

*)


type t= Double of bool * Partial_concrete_object_t.t * (Partial_concrete_object_t.t list) ;;

