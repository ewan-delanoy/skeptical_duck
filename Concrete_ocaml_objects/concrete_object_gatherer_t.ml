(* 

#use"Concrete_ocaml_objects/partial_concrete_object_t.ml";;

For convenience, the list is reversed when the partial object is closed and becomes full
(new objects are therefore appended directly to the left on a partial object).


*)

type base = Concrete_object_t.t

type t= 
   |Uple of base list
   |List of base list
   |Array of base list
   |Record of ((string*base) list)
   |RecordPlusRecordName of ((string*base) list)*string
   |Variant of string*(base list);;

