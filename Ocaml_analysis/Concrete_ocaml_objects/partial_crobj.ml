(* 

#use"Concrete_ocaml_objects/partial_crobj.ml";;


*)

exception Field_With_No_Name of Concrete_object_t.t;;
exception Unused_Field_Name of string;;
exception Misapplied_Field_Name of string;;
exception Category_Mismatch of Crobj_category_t.t * Partial_crobj_t.t;;

let initialize=function 
    Crobj_opening_t.Uple -> Partial_crobj_t.Uple[]
   |List -> Partial_crobj_t.List[]
   |Array -> Partial_crobj_t.Array[]
   |Record->Partial_crobj_t.Record([])
   |Variant(constructor)->Partial_crobj_t.Variant(constructor,[]);;

let category = function 
    Partial_crobj_t.Uple(l)->Crobj_category_t.Uple
   |List(_)->Crobj_category_t.List
   |Array(_)->Crobj_category_t.Array
   |Record(_)->Crobj_category_t.Record
   |RecordPlusFieldName(l,rcdname)->Crobj_category_t.Record
   |Variant(constructor,l)->Crobj_category_t.Variant;;
 

let push_one_more_item item =function 
    Partial_crobj_t.Uple(l)->Partial_crobj_t.Uple(item::l)
   |List(l)->Partial_crobj_t.List(item::l)
   |Array(l)->Partial_crobj_t.Array(item::l)
   |Record(_)->raise(Field_With_No_Name(item))
   |RecordPlusFieldName(l,rcdname)->Partial_crobj_t.Record((rcdname,item)::l)
   |Variant(constructor,l)->Partial_crobj_t.Variant(constructor,item :: l);;

let push_int i = push_one_more_item (Concrete_object_t.Int(i));;
let push_string encoded_s = push_one_more_item (Concrete_object_field.wrap_encoded_string encoded_s);;

let push_field_name recdname=function 
    Partial_crobj_t.Record(l)->Partial_crobj_t.RecordPlusFieldName(l,recdname)
   |_->raise(Misapplied_Field_Name(recdname));;



let close =function 
    Partial_crobj_t.Uple(l)->Concrete_object_t.Uple(List.rev l)
   |List(l)->Concrete_object_t.List(List.rev l)
   |Array(l)->Concrete_object_t.Array(List.rev l)
   |Record(l)->Concrete_object_t.Record(List.rev l)
   |RecordPlusFieldName(_,rcdname)->raise(Unused_Field_Name(rcdname))
   |Variant(constructor,l)->Concrete_object_t.Variant(constructor,List.rev l);;


let check_category_and_close ctgr pcrobj=
   if category(pcrobj)<>ctgr 
   then raise(Category_Mismatch(ctgr,pcrobj))
   else 
   close pcrobj;;