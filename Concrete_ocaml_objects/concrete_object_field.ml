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
let wrap_string s = Concrete_object_t.String(Encoded_string.encode s);;

let unwrap_int ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.Int(i)->i 
   |_->raise(Exn.Unwrap_int_exn(ccrt_obj));;

let unwrap_string ccrt_obj=
   match ccrt_obj with 
   Concrete_object_t.String(encoded_s)->Encoded_string.decode encoded_s 
   |_->raise(Exn.Unwrap_string_exn(ccrt_obj));;

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

let truth = Concrete_object_t.Variant("True",[]);;
let falsity = Concrete_object_t.Variant("False",[]);;
let of_bool bowl=if bowl then truth else falsity;;
let to_bool =unwrap_lonely_variant [true,"True";false,"False"] ;; 

let of_string_pair (s1,s2) = 
   Concrete_object_t.Uple (Image.image wrap_string [s1;s2]);;

let to_string_pair crobj=
  let (arg1,arg2,_,_,_,_,_)=unwrap_bounded_uple crobj 
  and us=unwrap_string in 
  (us arg1,us arg2);;

let of_string_triple (s1,s2,s3)=
   Concrete_object_t.Uple (Image.image wrap_string [s1;s2;s3]);;

let to_string_triple crobj=
  let (arg1,arg2,arg3,_,_,_,_)=unwrap_bounded_uple crobj 
  and us=unwrap_string in 
  (us arg1,us arg2,us arg3);;



let of_int_list l=Concrete_object_t.List (Image.image (fun i->Concrete_object_t.Int(i)) l);;
let to_int_list crobj = Image.image unwrap_int (unwrap_list crobj);;

let of_int_pair (i,j)=Concrete_object_t.Uple (Image.image (fun k->Concrete_object_t.Int(k)) [i;j]);;
let to_int_pair crobj = 
     let (arg1,arg2,_,_,_,_,_)=unwrap_bounded_uple crobj in
    (unwrap_int arg1,unwrap_int arg2);;

let of_string_list l=Concrete_object_t.List (Image.image wrap_string l);;
let to_string_list crobj = Image.image unwrap_string (unwrap_list crobj);;

let of_string_pair_list l= Concrete_object_t.List (Image.image of_string_pair l);;
let to_string_pair_list crobj = Image.image to_string_pair (unwrap_list crobj);;

let of_string_triple_list l= Concrete_object_t.List (Image.image of_string_triple l);;
let to_string_triple_list crobj = Image.image to_string_triple (unwrap_list crobj);;

let of_string_list_list l=Concrete_object_t.List (Image.image of_string_list l);;
let to_string_list_list crobj = Image.image to_string_list (unwrap_list crobj);;

let of_pair of_a of_b (a,b)=Concrete_object_t.Uple[of_a a;of_b b];;
let to_pair to_a to_b crobj=
    let (arg1,arg2,_,_,_,_,_)=unwrap_bounded_uple crobj in
    (to_a arg1,to_b arg2);;

let of_list of_a l= Concrete_object_t.List(Image.image of_a l);;
let to_list to_a crobj= Image.image to_a (unwrap_list crobj);;

let of_array of_a arr= Concrete_object_t.Array(Array.to_list(Array.map of_a arr));;
let to_array to_a crobj= Array.map to_a (unwrap_array crobj);;

let of_pair_list of_a of_b l=of_list (of_pair of_a of_b) l;;
let to_pair_list to_a to_b crobj = to_list (to_pair to_a to_b) crobj;;

