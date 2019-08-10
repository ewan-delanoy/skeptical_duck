(* 

#use"Concrete_ocaml_objects/double_partial_crobj.ml";;

*)

exception Close_on_separator;;
exception Redundant_separator;;
exception Category_Mismatch of Crobj_category_t.t * Partial_crobj_t.t;;
exception End_reached of Concrete_object_t.t ;;

module Private = struct 

let push_int i (Double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Double_partial_crobj_t.Double(false,
    Partial_crobj.push_int i last_opened,opened_before));;

let push_string s (Double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Double_partial_crobj_t.Double(false,
    Partial_crobj.push_string s last_opened,opened_before));;    

let push_separator ctgr (Double_partial_crobj_t.Double(separator_present,last_opened,opened_before))=
  if separator_present
  then raise(Redundant_separator)
  else 
        let ctgr2 = Partial_crobj.category last_opened in 
        if ctgr <> ctgr2
        then raise(Category_Mismatch(ctgr,last_opened))
        else  (Double_partial_crobj_t.Double(true,last_opened,opened_before));;

let push_field_name record_name (Double_partial_crobj_t.Double(_,last_opened,opened_before))=
  (Double_partial_crobj_t.Double(false,
    Partial_crobj.push_field_name record_name last_opened,opened_before));;    

let open_new opening 
   (Double_partial_crobj_t.Double(_,last_opened,opened_before))=
    Double_partial_crobj_t.Double(false,
      Partial_crobj.initialize opening,last_opened::opened_before);;



let close ctgr
    (Double_partial_crobj_t.Double(separator_present,last_opened,opened_before))=
    if separator_present 
    then raise(Close_on_separator)
    else 
    let newfound=Partial_crobj.check_category_and_close ctgr last_opened in 
    match opened_before with 
    []->raise(End_reached(newfound))
    |next_opened_one::others ->
      let new_frontier = Partial_crobj.push_one_more_item newfound next_opened_one in 
      Double_partial_crobj_t.Double(false,new_frontier,others);;

end ;; 

let initialize opening = 
    Double_partial_crobj_t.Double(false,Partial_crobj.initialize opening,[]);;

let increase = function 
   Crobj_basic_increase_t.Push_int(i)->Private.push_int i 
    |Push_string(s)->Private.push_string s 
    |Push_field_name(rcdname)->Private.push_field_name rcdname
    |Open(opening) -> Private.open_new opening
    |Separate(cat) -> Private.push_separator cat 
    |Close(cat) -> Private.close cat;;
        




