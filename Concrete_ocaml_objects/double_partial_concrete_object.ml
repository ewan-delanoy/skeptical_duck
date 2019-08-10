(* 

#use"Concrete_ocaml_objects/double_partial_concrete_object.ml";;

*)

exception Close_on_separator;;
exception Redundant_separator;;
exception Category_Mismatch of Crobj_category_t.t * Partial_concrete_object_t.t;;

let push_int i (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
  (Double_partial_concrete_object_t.Double(false,
    Partial_concrete_object.push_int i last_opened,opened_before));;

let push_string s (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
  (Double_partial_concrete_object_t.Double(false,
    Partial_concrete_object.push_string s last_opened,opened_before));;    

let push_separator ctgr (Double_partial_concrete_object_t.Double(separator_present,last_opened,opened_before))=
  if separator_present
  then raise(Redundant_separator)
  else 
        let ctgr2 = Partial_concrete_object.category last_opened in 
        if ctgr <> ctgr2
        then raise(Category_Mismatch(ctgr,last_opened))
        else  (Double_partial_concrete_object_t.Double(true,last_opened,opened_before));;

let push_record_name record_name (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
  (Double_partial_concrete_object_t.Double(false,
    Partial_concrete_object.push_record_name record_name last_opened,opened_before));;    

let open_new opening 
   (Double_partial_concrete_object_t.Double(_,last_opened,opened_before))=
    Double_partial_concrete_object_t.Double(false,
      Partial_concrete_object.of_opening opening,last_opened::opened_before);;

exception End_reached of Concrete_object_t.t ;;

let close ctgr
    (Double_partial_concrete_object_t.Double(separator_present,last_opened,opened_before))=
    if separator_present 
    then raise(Close_on_separator)
    else 
    let newfound=Partial_concrete_object.check_category_and_close ctgr last_opened in 
    match opened_before with 
    []->raise(End_reached(newfound))
    |next_opened_one::others ->
      let new_frontier = Partial_concrete_object.push_one_more_item newfound next_opened_one in 
      Double_partial_concrete_object_t.Double(false,new_frontier,others);;


        




