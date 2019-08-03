(* 

#use"Concrete_ocaml_objects/concrete_object_field.ml";;


*)

(*
let int i=Concrete_object_t.Int i;;
let string s=Concrete_object_t.String s;;
let uple l=Concrete_object_t.Uple l;;
let list l=Concrete_object_t.List l;;
let array l=Concrete_object_t.Array l;;
let record l=Concrete_object_t.Record l;;
let variant constructor l=Concrete_object_t.Record l;;
*)

exception Get_record_exn of Concrete_object_t.t;;

let get_record ccrt_obj field =
   match ccrt_obj with 
   Concrete_object_t.Record(l)->List.assoc field l 
   |_->raise(Get_record_exn(ccrt_obj));;

