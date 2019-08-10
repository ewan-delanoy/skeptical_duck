(* 

#use"Concrete_ocaml_objects/partial_concrete_object.ml";;


*)

exception Field_With_No_Name of Concrete_object_t.t;;
exception Unused_Field_Name of string;;
exception Misapplied_Field_Name of string;;
exception Category_Mismatch of Crobj_category_t.t * Partial_concrete_object_t.t;;

let of_opening=function 
    Crobj_opening_t.Uple -> Partial_concrete_object_t.Uple[]
   |List -> Partial_concrete_object_t.List[]
   |Array -> Partial_concrete_object_t.Array[]
   |Record(first_record_label)->Partial_concrete_object_t.RecordPlusFieldName([],first_record_label)
   |Variant(constructor)->Partial_concrete_object_t.Variant(constructor,[]);;

let category = function 
    Partial_concrete_object_t.Uple(l)->Crobj_category_t.Uple
   |List(_)->Crobj_category_t.List
   |Array(_)->Crobj_category_t.Array
   |Record(_)->Crobj_category_t.Record
   |RecordPlusFieldName(l,rcdname)->Crobj_category_t.Record
   |Variant(constructor,l)->Crobj_category_t.Variant;;
 

let push_one_more_item item =function 
    Partial_concrete_object_t.Uple(l)->Partial_concrete_object_t.Uple(item::l)
   |List(l)->Partial_concrete_object_t.List(item::l)
   |Array(l)->Partial_concrete_object_t.Array(item::l)
   |Record(_)->raise(Field_With_No_Name(item))
   |RecordPlusFieldName(l,rcdname)->Partial_concrete_object_t.Record((rcdname,item)::l)
   |Variant(constructor,l)->Partial_concrete_object_t.Variant(constructor,item :: l);;

let push_int i = push_one_more_item (Concrete_object_t.Int(i));;
let push_string s = push_one_more_item (Concrete_object_t.String(s));;

let push_field_name recdname=function 
    Partial_concrete_object_t.Record(l)->Partial_concrete_object_t.RecordPlusFieldName(l,recdname)
   |_->raise(Misapplied_Field_Name(recdname));;



let close =function 
    Partial_concrete_object_t.Uple(l)->Concrete_object_t.Uple(List.rev l)
   |List(l)->Concrete_object_t.Uple(List.rev l)
   |Array(l)->Concrete_object_t.Array(List.rev l)
   |Record(l)->Concrete_object_t.Record(List.rev l)
   |RecordPlusFieldName(_,rcdname)->raise(Unused_Field_Name(rcdname))
   |Variant(constructor,l)->Concrete_object_t.Variant(constructor,List.rev l);;


let check_category_and_close ctgr pcrobj=
   if category(pcrobj)<>ctgr 
   then raise(Category_Mismatch(ctgr,pcrobj))
   else 
   close pcrobj;;