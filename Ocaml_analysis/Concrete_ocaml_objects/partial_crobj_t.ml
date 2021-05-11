(* 

#use"Ocaml_analysis/Concrete_ocaml_objects/partial_crobj_t.ml";;

For convenience, the list is reversed when the partial object is closed and becomes full
(new objects are therefore appended directly to the left on a partial object).


*)


type t= 
   |Uple of Concrete_object_t.t list
   |List of Concrete_object_t.t list
   |Array of Concrete_object_t.t list
   |Record of ((string*Concrete_object_t.t) list)
   |RecordPlusFieldName of ((string*Concrete_object_t.t) list)*string
   |Variant of string*(Concrete_object_t.t list);;

