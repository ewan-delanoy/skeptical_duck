(* 

#use"Concrete_ocaml_objects/concrete_object_field.ml";;

*)

module Exn = struct

   exception Get_record_absent_key_exn of string;;
   exception Get_record_bad_type_exn of Concrete_object_t.t;;
   exception Get_pair_exn of Concrete_object_t.t;;
   exception Unwrap_array_exn of Concrete_object_t.t;;
   exception Unwrap_int_exn of Concrete_object_t.t;;
   exception Unwrap_list_exn of Concrete_object_t.t;;
   exception Unwrap_string_exn of Concrete_object_t.t;;
   exception Wrap_lonely_variant_exn;;
   exception Unwrap_lonely_variant_exn of Concrete_object_t.t;;
   
   exception Uple_too_big of Concrete_object_t.t;;
   exception Uple_too_small of Concrete_object_t.t;;
   exception Unwrap_bounded_uple_exn of Concrete_object_t.t;;
   
   exception Variant_too_big of Concrete_object_t.t;;
   exception Variant_too_small of Concrete_object_t.t;;
   exception Unwrap_bounded_variant_exn of Concrete_object_t.t;;
   
   end;;
   
   let wrap_encoded_string encoded_s=Concrete_object_t.String(encoded_s);;
   let wrap_int i = (Concrete_object_t.Int i);;
   
   let unwrap_int ccrt_obj=
      match ccrt_obj with 
      Concrete_object_t.Int(i)->i 
      |_->raise(Exn.Unwrap_int_exn(ccrt_obj));;
   
   
   let unwrap_list ccrt_obj=
      match ccrt_obj with 
      Concrete_object_t.List(l)->l 
      |_->raise(Exn.Unwrap_list_exn(ccrt_obj));;
   
   let unwrap_array ccrt_obj=
      match ccrt_obj with 
      Concrete_object_t.Array(l)->Array.of_list l 
      |_->raise(Exn.Unwrap_array_exn(ccrt_obj));;
   
   let get_record ccrt_obj field =
      match ccrt_obj with 
      Concrete_object_t.Record(l)->
           (try List.assoc field l with 
           _ ->raise(Exn.Get_record_absent_key_exn(field)))
      |_->raise(Exn.Get_record_bad_type_exn(ccrt_obj));;
   
   let unwrap_bounded_uple ccrt_obj=
     match ccrt_obj with 
      Concrete_object_t.Uple(l)->
         let n=List.length(l) in 
         if  n<2 then raise(Exn.Uple_too_small(ccrt_obj)) else 
         if  n>7 then raise(Exn.Uple_too_big(ccrt_obj)) else 
         let i3=(if n<3 then 1 else 3)
         and i4=(if n<4 then 1 else 4)
         and i5=(if n<5 then 1 else 5)
         and i6=(if n<6 then 1 else 6)
         and i7=(if n<7 then 1 else 7) in
         let get=(fun k->List.nth l (k-1)) in 
         (get 1,get 2,get i3,get i4,get i5,get i6,get i7)
      | _-> raise(Exn.Unwrap_bounded_uple_exn(ccrt_obj));;
   
   
   
   let unwrap_bounded_variant ccrt_obj=
     match ccrt_obj with 
      Concrete_object_t.Variant(constructor,l)->
         let n=List.length(l) in 
         if  n<1 then raise(Exn.Variant_too_small(ccrt_obj)) else 
         if  n>7 then raise(Exn.Variant_too_big(ccrt_obj)) else 
         let i2=(if n<2 then 1 else 2) 
         and i3=(if n<3 then 1 else 3)
         and i4=(if n<4 then 1 else 4)
         and i5=(if n<5 then 1 else 5)
         and i6=(if n<6 then 1 else 6)
         and i7=(if n<7 then 1 else 7) in
         let get=(fun k->List.nth l (k-1)) in 
         (constructor,(get 1,get i2,get i3,get i4,get i5,get i6,get i7))
      | _-> raise(Exn.Unwrap_bounded_variant_exn(ccrt_obj));;
   
   
   let wrap_lonely_variant l_pairs unwrapped=
      match Option.seek(fun (key,vaal)->key=unwrapped) l_pairs with
         None->raise(Exn.Wrap_lonely_variant_exn)
        |Some(_,constructor)->Concrete_object_t.Variant(constructor,[]) ;;
   
   
   let unwrap_lonely_variant l_pairs ccrt_obj=
      match ccrt_obj with 
      Concrete_object_t.Variant(constructor,l)->
         if  l<>[] then raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj)) else 
         (match Option.seek(fun (_,key)->key=constructor) l_pairs with
         None->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj))
        |Some(vaal,_)->vaal) 
      |_->raise(Exn.Unwrap_lonely_variant_exn(ccrt_obj));;
   