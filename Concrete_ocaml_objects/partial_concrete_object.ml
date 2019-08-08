(* 

#use"Concrete_ocaml_objects/partial_concrete_object.ml";;


*)

exception Record_With_No_Name of Concrete_object_t.t;;
exception Unused_Record_Name of string;;
exception Misapplied_Record_Name of string;;

let of_opening=function 
    Crobj_opening_t.Uple -> Partial_concrete_object_t.Uple[]
   |List -> Partial_concrete_object_t.List[]
   |Array -> Partial_concrete_object_t.Array[]
   |Record(first_record_label)->Partial_concrete_object_t.RecordPlusRecordName([],first_record_label)
   |Variant(constructor)->Partial_concrete_object_t.Variant(constructor,[]);;

let category = function 
    Partial_concrete_object_t.Uple(l)->Crobj_category_t.Uple
   |List(_)->Crobj_category_t.List
   |Array(_)->Crobj_category_t.Array
   |Record(_)->Crobj_category_t.Record
   |RecordPlusRecordName(l,rcdname)->Crobj_category_t.Record
   |Variant(constructor,l)->Crobj_category_t.Variant;;
 

let push_one_more_item item =function 
    Partial_concrete_object_t.Uple(l)->Partial_concrete_object_t.Uple(item::l)
   |List(l)->Partial_concrete_object_t.List(item::l)
   |Array(l)->Partial_concrete_object_t.Array(item::l)
   |Record(_)->raise(Record_With_No_Name(item))
   |RecordPlusRecordName(l,rcdname)->Partial_concrete_object_t.Record((rcdname,item)::l)
   |Variant(constructor,l)->Partial_concrete_object_t.Variant(constructor,item :: l);;

let push_int i = push_one_more_item (Concrete_object_t.Int(i));;
let push_string s = push_one_more_item (Concrete_object_t.String(s));;

let push_record_name recdname=function 
    Partial_concrete_object_t.Record(l)->Partial_concrete_object_t.RecordPlusRecordName(l,recdname)
   |_->raise(Misapplied_Record_Name(recdname));;



let close =function 
    Partial_concrete_object_t.Uple(l)->Concrete_object_t.Uple(List.rev l)
   |List(l)->Concrete_object_t.Uple(List.rev l)
   |Array(l)->Concrete_object_t.Array(List.rev l)
   |Record(l)->Concrete_object_t.Record(List.rev l)
   |RecordPlusRecordName(_,rcdname)->raise(Unused_Record_Name(rcdname))
   |Variant(constructor,l)->Concrete_object_t.Variant(constructor,List.rev l);;
