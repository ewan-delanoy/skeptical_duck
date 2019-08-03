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

let truth = Concrete_object_t.Variant("True",[]);;
let falsity = Concrete_object_t.Variant("False",[]);;
let of_bool bowl=if bowl then truth else falsity;;

exception To_bool_exn of Concrete_object_t.t;;

let to_bool ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.Variant(constructor,l)->
      if  l<>[] then raise(To_bool_exn(ccrt_obj)) else 
      if constructor="True" then true else 
      if constructor="False" then false else raise(To_bool_exn(ccrt_obj))
   |_->raise(Unwrap_string_exn(ccrt_obj));;
