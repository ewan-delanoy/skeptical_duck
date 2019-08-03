(* 

#use"Concrete_ocaml_objects/concrete_object_field.ml";;

*)

exception Get_record_exn of Concrete_object_t.t;;

let get_record ccrt_obj field =
   match ccrt_obj with 
   Concrete_object_t.Record(l)->List.assoc field l 
   |_->raise(Get_record_exn(ccrt_obj));;

exception Unwrap_string_exn of Concrete_object_t.t;;

let unwrap_string ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.String(s)->s 
   |_->raise(Unwrap_string_exn(ccrt_obj));;

let get_str_record ccrt_obj field=unwrap_string(get_record ccrt_obj field);;   