(* 

#use"Concrete_ocaml_objects/double_partial_concrete_object.ml";;


*)

exception Close_on_comma;;
exception Comma_on_comma;;

let push_int i (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
  (Double_partial_concrete_object_t.Double(false,
    Partial_concrete_object.push_int i last_opened,opened_before));;

let push_string s (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
  (Double_partial_concrete_object_t.Double(false,
    Partial_concrete_object.push_string s last_opened,opened_before));;    

let push_comma  (Double_partial_concrete_object_t.Double(comma_present,last_opened,opened_before))=
  if comma_present
  then raise(Comma_on_comma)
  else 
  (Double_partial_concrete_object_t.Double(true,last_opened,opened_before));;

let open_new_uple 
  (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
    Double_partial_concrete_object_t.Double(false,
      Partial_concrete_object_t.Uple[],last_opened::opened_before);;

let open_new_list 
  (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
    Double_partial_concrete_object_t.Double(false,
      Partial_concrete_object_t.List[],last_opened::opened_before);;

let open_new_array
  (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
    Double_partial_concrete_object_t.Double(false,
      Partial_concrete_object_t.Array[],last_opened::opened_before);;

let open_new_record item_name
  (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
    Double_partial_concrete_object_t.Double(false,
      Partial_concrete_object_t.RecordPlusRecordName([],item_name),last_opened::opened_before);;


let open_new_variant constructor
  (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
    Double_partial_concrete_object_t.Double(false,
      Partial_concrete_object_t.Variant(constructor,[]),last_opened::opened_before);;



let close_current
    (Double_partial_concrete_object_t.Double(comma_present,last_opened,opened_before))=
    if comma_present 
    then raise(Close_on_comma)
    else 
    let newfound=Partial_concrete_object.close last_opened in 
    match opened_before with 
    []->(None,Some(newfound))
    |next_opened_one::others ->
      let new_frontier = Partial_concrete_object.push_one_more_item newfound next_opened_one in 
      let answer=(Double_partial_concrete_object_t.Double(false,new_frontier,others)) in 
      (Some answer,None);;


        




