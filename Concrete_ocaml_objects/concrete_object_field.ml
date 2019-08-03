(* 

#use"Concrete_ocaml_objects/concrete_object_field.ml";;

*)

exception Get_record_exn of Concrete_object_t.t;;

let get_record ccrt_obj field =
   match ccrt_obj with 
   Concrete_object_t.Record(l)->List.assoc field l 
   |_->raise(Get_record_exn(ccrt_obj));;

exception Get_pair_exn of Concrete_object_t.t;;

let get_pair ccrt_obj =
  match ccrt_obj with 
   Concrete_object_t.Uple(l)->
        if List.length(l)<>2
        then raise(Get_pair_exn(ccrt_obj))
        else (List.nth l 0,List.nth l 1)
   |_->raise(Get_pair_exn(ccrt_obj));;

exception Unwrap_string_exn of Concrete_object_t.t;;

let unwrap_string ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.String(s)->s 
   |_->raise(Unwrap_string_exn(ccrt_obj));;

exception Unwrap_lonely_variant_exn of Concrete_object_t.t;;

let unwrap_lonely_variant l_pairs ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.Variant(constructor,l)->
      if  l<>[] then raise(Unwrap_lonely_variant_exn(ccrt_obj)) else 
      (match Option.seek(fun (key,vaal)->key=constructor) l_pairs with
      None->raise(Unwrap_lonely_variant_exn(ccrt_obj))
     |Some(_,vaal)->vaal) 
   |_->raise(Unwrap_string_exn(ccrt_obj));;

exception To_bool_exn of Concrete_object_t.t;;

let to_bool =unwrap_lonely_variant ["True",true;"False",false] ;; 


let get_str_record ccrt_obj field=unwrap_string(get_record ccrt_obj field);;   

let truth = Concrete_object_t.Variant("True",[]);;
let falsity = Concrete_object_t.Variant("False",[]);;
let of_bool bowl=if bowl then truth else falsity;;


