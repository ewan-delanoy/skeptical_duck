(* 

#use"Concrete_ocaml_objects/crobj_parsing.ml";;


*)

exception Unreadable of int * string ;;

module Private = struct

let salt = Encoded_string.salt ;; 

let array_opener = salt ^ "ao";;
let list_opener = salt ^ "lo";;
let record_opener = salt ^ "ro";;
let string_opener = salt ^ "so";;
let uple_opener = salt ^ "uo";; 
let variant_opener = salt ^ "vo";; 

let array_separator = salt ^ "as";;
let list_separator = salt ^ "ls";;
let record_separator = salt ^ "rs";;
let uple_separator = salt ^ "us";; 
let variant_separator = salt ^ "vs";; 

let array_closer = salt ^ "ac";;
let list_closer = salt ^ "lc";;
let record_closer = salt ^ "rc";;
let string_closer = salt ^ "sc";;
let uple_closer = salt ^ "uc";; 
let variant_closer = salt ^ "vc";; 

let record_arrow = salt ^ "ra";;


let array_cat = Crobj_category_t.Array
and list_cat  = Crobj_category_t.List
and record_cat = Crobj_category_t.Record 
and uple_cat = Crobj_category_t.Uple 
and variant_cat = Crobj_category_t.Variant;;

let list_for_category_of_lexeme=[
    (array_opener,array_cat); 
    (list_opener,list_cat); 
    (record_opener,record_cat); 
    (uple_opener,uple_cat); 
    (variant_opener,variant_cat); 

    (array_separator,array_cat); 
    (list_separator,list_cat); 
    (record_separator,record_cat); 
    (uple_separator,uple_cat); 
    (variant_separator,variant_cat); 

    (array_closer,array_cat); 
    (list_closer,list_cat); 
    (record_closer,record_cat); 
    (uple_closer,uple_cat); 
    (variant_closer,variant_cat); 

    (record_arrow,record_cat); (* a little bit of convenient convention here *)
];;

let category_of_lexeme lexeme=List.assoc lexeme list_for_category_of_lexeme;;

let list_for_preludeless_increasers=[
    (array_opener,Crobj_basic_increase_t.Open(Crobj_opening_t.Array)); 
    (list_opener,Crobj_basic_increase_t.Open(Crobj_opening_t.List)); 
    (record_opener,Crobj_basic_increase_t.Open(Crobj_opening_t.Record)); 
    (uple_opener,Crobj_basic_increase_t.Open(Crobj_opening_t.Uple)); 

    (array_separator,Crobj_basic_increase_t.Separate(Crobj_category_t.Array)); 
    (list_separator,Crobj_basic_increase_t.Separate(Crobj_category_t.List)); 
    (record_separator,Crobj_basic_increase_t.Separate(Crobj_category_t.Record)); 
    (uple_separator,Crobj_basic_increase_t.Separate(Crobj_category_t.Uple)); 
    (variant_separator,Crobj_basic_increase_t.Separate(Crobj_category_t.Variant)); 

    (array_closer,Crobj_basic_increase_t.Close(Crobj_category_t.Array)); 
    (list_closer,Crobj_basic_increase_t.Close(Crobj_category_t.List)); 
    (record_closer,Crobj_basic_increase_t.Close(Crobj_category_t.Record)); 
    (uple_closer,Crobj_basic_increase_t.Close(Crobj_category_t.Uple)); 
    (variant_closer,Crobj_basic_increase_t.Close(Crobj_category_t.Variant)); 

];;

exception Unreadable_int of string;;

let parse_int s=try int_of_string s with _->raise(Unreadable_int(s));;

let next_basic_increase_in_variant_opening_case s idx idx1=
   let opening = Crobj_opening_t.Variant (Cull_string.interval s idx (idx1-1)) in 
   (Crobj_basic_increase_t.Open(opening),idx1+(String.length variant_opener));;

let next_basic_increase_in_field_naming_case s idx idx1=
   let name = Cull_string.interval s idx (idx1-1) in 
   (Crobj_basic_increase_t.Push_field_name(name),idx1+(String.length record_arrow));;

let next_basic_increase_in_preludy_case s idx idx1=
   if Substring.is_a_substring_located_at variant_opener s idx1 
   then next_basic_increase_in_variant_opening_case s idx idx1
   else 
   if Substring.is_a_substring_located_at record_arrow s idx1 
   then next_basic_increase_in_field_naming_case s idx idx1
   else let i=parse_int(Cull_string.interval s idx (idx1-1)) in 
       (Crobj_basic_increase_t.Push_int(i),idx1);;


exception Missing_string_closer of int * string;;

let next_basic_increase_in_push_string_case s idx=
   let idx1=idx+(String.length string_opener) in 
   let idx2=Substring.leftmost_index_of_in_from string_closer s idx1 in 
   if idx2<0
   then raise(Missing_string_closer(idx1,s))
   else
   (* we know that the string is already encoded *)
   let encoded_s = Encoded_string.retrieve (Cull_string.interval s idx1 (idx2-1)) in 
   (Crobj_basic_increase_t.Push_string(encoded_s),idx2+(String.length string_closer));;




exception Unreadable_increase of int * string ;;

let next_basic_increase  s idx=
   let idx1= Substring.leftmost_index_of_in_from salt s idx in 
   if idx1<0 
   then let i=parse_int (Cull_string.cobeginning (idx-1) s) in
        (Crobj_basic_increase_t.Push_int(i),String.length(s)+1)
   else      
   if idx1>idx 
   then next_basic_increase_in_preludy_case s idx idx1
   else 
   if Substring.is_a_substring_located_at string_opener s idx 
   then next_basic_increase_in_push_string_case s idx
   else 
   match Option.seek (fun 
      (text,action)->Substring.is_a_substring_located_at text s idx
   ) list_for_preludeless_increasers with 
   None -> raise(Unreadable_increase(idx,s))
   |Some(text,action)->(action,idx+(String.length text));;

let one_step_more machine =
   let (action,next_idx) =
      next_basic_increase machine.Crobj_parsing_machine_t.parsed_one 
                            machine.Crobj_parsing_machine_t.current_index in 
   {
      machine with 
      Crobj_parsing_machine_t.current_index = next_idx ;
      Crobj_parsing_machine_t.data = Double_partial_crobj.increase action machine.Crobj_parsing_machine_t.data;
   }  ;;

let prudent_push machine = try (None,Some(one_step_more machine)) with 
   Double_partial_crobj.End_reached(solution) -> (Some solution,None);;

exception First_step_exn of Crobj_basic_increase_t.t ;; 

let first_step s =
   let (action,next_idx) = next_basic_increase s 1 in 
   match action with 
    Crobj_basic_increase_t.Push_int(i)->(Some(Concrete_object_t.Int(i)),None,next_idx)
   |Crobj_basic_increase_t.Push_string(encoded_s)->(Some(Concrete_object_field.wrap_encoded_string(encoded_s)),None,next_idx)
   |Crobj_basic_increase_t.Open(opening)->(None,Some(Double_partial_crobj.initialize(opening)),next_idx)
   |_->raise(First_step_exn(action));;

exception Ends_too_soon of Concrete_object_t.t * string ;; 

let parse s =
    let (opt_quick_result,opt_start,next_idx) = first_step s in 
    match opt_quick_result with 
    Some (res)-> if next_idx < (String.length s)
                 then raise(Ends_too_soon(res,s)) 
                 else res 
    |None -> let start_partial_obj = Option.unpack opt_start in   
             let machine = {
                Crobj_parsing_machine_t.parsed_one = s ;
                Crobj_parsing_machine_t.current_index = next_idx ;
                Crobj_parsing_machine_t.data = start_partial_obj;
             } in 
             let rec iterator = (fun mach ->
                let (opt_sol,opt_term) = prudent_push mach in 
                match opt_term with 
                None -> Option.unpack opt_sol 
                |Some(term)->iterator(term) 
             ) in 
             iterator machine;;

let rec unparse = function 
   Concrete_object_t.Int(i)->string_of_int i 
   |String(t)->string_opener^(Encoded_string.store t)^string_closer
   |Uple(l)->let temp1=Image.vorstellung unparse l in 
             uple_opener^(String.concat uple_separator temp1)^uple_closer
   |List(l)->let temp1=Image.vorstellung unparse l in 
             list_opener^(String.concat list_separator temp1)^list_closer 
   |Array(l)->let temp1=Image.vorstellung unparse l in 
             array_opener^(String.concat array_separator temp1)^array_closer
   |Record(l)->let temp1=Image.vorstellung (fun (key,vaal)->key ^ record_arrow ^ (unparse vaal))  l in 
             record_opener^(String.concat record_separator temp1)^record_closer          
   |Variant(constructor,l)->let temp1=Image.vorstellung unparse l in 
             constructor^variant_opener^(String.concat variant_separator temp1)^variant_closer ;; 

end;;

let parse = Private.parse;;
let unparse = Private.unparse;;