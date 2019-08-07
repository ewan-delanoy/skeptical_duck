(* 

#use"Concrete_ocaml_objects/partial_concrete_object.ml";;



*)

exception Record_With_No_Name of Concrete_object_t.t;;
exception Unused_Record_Name of string;;
exception Misapplied_Record_Name of string;;


let add_one_more_item item =function 
    Partial_concrete_object_t.Uple(l)->Partial_concrete_object_t.Uple(item::l)
   |List(l)->Partial_concrete_object_t.List(item::l)
   |Array(l)->Partial_concrete_object_t.Array(item::l)
   |Record(_)->raise(Record_With_No_Name(item))
   |RecordPlusRecordName(l,rcdname)->Partial_concrete_object_t.Record((rcdname,item)::l)
   |Variant(constructor,l)->Partial_concrete_object_t.Variant(constructor,item :: l);;

let add_record_name recdname=function 
    Partial_concrete_object_t.Record(l)->Partial_concrete_object_t.RecordPlusRecordName(l,recdname)
   |_->raise(Misapplied_Record_Name(recdname));;

let close =function 
    Partial_concrete_object_t.Uple(l)->Concrete_object_t.Uple(List.rev l)
   |List(l)->Concrete_object_t.Uple(List.rev l)
   |Array(l)->Concrete_object_t.Array(List.rev l)
   |Record(l)->Concrete_object_t.Record(List.rev l)
   |RecordPlusRecordName(_,rcdname)->raise(Unused_Record_Name(rcdname))
   |Variant(constructor,l)->Concrete_object_t.Variant(constructor,List.rev l);;
