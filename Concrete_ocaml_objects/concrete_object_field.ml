(* 

#use"Concrete_ocaml_objects/concrete_object_field.ml";;

*)

exception Get_record_exn of Concrete_object_t.t;;
exception Get_pair_exn of Concrete_object_t.t;;
exception Unwrap_array_exn of Concrete_object_t.t;;
exception Unwrap_int_exn of Concrete_object_t.t;;
exception Unwrap_list_exn of Concrete_object_t.t;;
exception Unwrap_string_exn of Concrete_object_t.t;;
exception Wrap_lonely_variant_exn;;
exception Unwrap_lonely_variant_exn of Concrete_object_t.t;;


let get_record ccrt_obj field =
   match ccrt_obj with 
   Concrete_object_t.Record(l)->List.assoc field l 
   |_->raise(Get_record_exn(ccrt_obj));;



let get_pair ccrt_obj =
  match ccrt_obj with 
   Concrete_object_t.Uple(l)->
        if List.length(l)<>2
        then raise(Get_pair_exn(ccrt_obj))
        else (List.nth l 0,List.nth l 1)
   |_->raise(Get_pair_exn(ccrt_obj));;

let unwrap_array ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.Array(l)->Array.of_list l 
   |_->raise(Unwrap_array_exn(ccrt_obj));;

let unwrap_int ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.Int(i)->i 
   |_->raise(Unwrap_int_exn(ccrt_obj));;

let unwrap_list ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.List(l)->l 
   |_->raise(Unwrap_list_exn(ccrt_obj));;

let unwrap_string ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.String(s)->s 
   |_->raise(Unwrap_string_exn(ccrt_obj));;


let wrap_lonely_variant l_pairs unwrapped=
   match Option.seek(fun (key,vaal)->key=unwrapped) l_pairs with
      None->raise(Wrap_lonely_variant_exn)
     |Some(_,constructor)->Concrete_object_t.Variant(constructor,[]) ;;


let unwrap_lonely_variant l_pairs ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.Variant(constructor,l)->
      if  l<>[] then raise(Unwrap_lonely_variant_exn(ccrt_obj)) else 
      (match Option.seek(fun (_,key)->key=constructor) l_pairs with
      None->raise(Unwrap_lonely_variant_exn(ccrt_obj))
     |Some(vaal,_)->vaal) 
   |_->raise(Unwrap_string_exn(ccrt_obj));;

exception Uple_too_big of Concrete_object_t.t;;
exception Uple_too_small of Concrete_object_t.t;;
exception Unwrap_bounded_uple_exn of Concrete_object_t.t;;

let unwrap_bounded_uple ccrt_obj=
  match ccrt_obj with 
   Concrete_object_t.Uple(l)->
      let n=List.length(l) in 
      if  n<2 then raise(Uple_too_small(ccrt_obj)) else 
      if  n>7 then raise(Uple_too_big(ccrt_obj)) else 
      let i3=(if n<3 then 1 else 3)
      and i4=(if n<4 then 1 else 4)
      and i5=(if n<5 then 1 else 5)
      and i6=(if n<6 then 1 else 6)
      and i7=(if n<7 then 1 else 7) in
      let get=(fun k->List.nth l (k-1)) in 
      (get 1,get 2,get i3,get i4,get i5,get i6,get i7)
   | _-> raise(Unwrap_bounded_uple_exn(ccrt_obj));;

exception Variant_too_big of Concrete_object_t.t;;
exception Variant_too_small of Concrete_object_t.t;;
exception Unwrap_bounded_variant_exn of Concrete_object_t.t;;

let unwrap_bounded_variant ccrt_obj=
  match ccrt_obj with 
   Concrete_object_t.Variant(constructor,l)->
      let n=List.length(l) in 
      if  n<1 then raise(Variant_too_small(ccrt_obj)) else 
      if  n>7 then raise(Variant_too_big(ccrt_obj)) else 
      let i2=(if n<2 then 1 else 2) 
      and i3=(if n<3 then 1 else 3)
      and i4=(if n<4 then 1 else 4)
      and i5=(if n<5 then 1 else 5)
      and i6=(if n<6 then 1 else 6)
      and i7=(if n<7 then 1 else 7) in
      let get=(fun k->List.nth l (k-1)) in 
      (constructor,(get 1,get i2,get i3,get i4,get i5,get i6,get i7))
   | _-> raise(Unwrap_bounded_variant_exn(ccrt_obj));;




let to_bool =unwrap_lonely_variant [true,"True";false,"False"] ;; 


let get_str_record ccrt_obj field=unwrap_string(get_record ccrt_obj field);;   

let truth = Concrete_object_t.Variant("True",[]);;
let falsity = Concrete_object_t.Variant("False",[]);;
let of_bool bowl=if bowl then truth else falsity;;


