(*

#use"lib/Poor_mans_row_polymorphism/poor_mans_row_polymorphism.ml";;

*)

type item = {
   item_name : string;
   code_for_transparent_mli : string ;
   code_for_opaque_mli : string;
   code_for_ml : string;
} ;;

let order_on_items = ((fun item1 item2 ->
  let trial1 = Total_ordering.lex_for_strings item1.item_name item2.item_name in 
  if trial1 <> Total_ordering_result_t.Equal then trial1 else
  Total_ordering.standard item1 item2  
) : item Total_ordering_t.t);;


let fold_merge_items = Ordered.fold_merge order_on_items;;
let sort_items = Ordered.sort order_on_items;;

let ds = ";" ^ ";\n\n" ;;

let protect t = 
    if String.contains t ' ' then "("^t^")" else t
;;

let getter_item (Pmrp_types.F(s),t) =
  {
   item_name = s;
   code_for_transparent_mli ="val "^s^" : (t,"^t^") decorated_map1" ;
   code_for_opaque_mli ="val "^s^" : (t,"^t^") decorated_map1";
   code_for_ml = 
  "let "^s^" = { \n"^
  " dm_input = Pmrp_involved_or_not_t.Involved(Pmrp_field_set.make [ Pmrp_types.F \""^s^"\" ]) ;\n"^
  " dm_output = Pmrp_involved_or_not_t.Not_involved(\""^t^"\") ;\n"^
  " dm_additional_info = Some \"(getter)\" ;\n"^
  " dm_actor= (fun x ->try Option.get(x."^s^") with _ ->raise(Get_exn(\""^s^"\")));\n"^
  "} " ^ds ;
} ;;

let setter_item (Pmrp_types.F(s),t) =
  {
   item_name = "set_"^s;
   code_for_transparent_mli ="val set_"^s^" : (t,"^t^",t) decorated_map2" ;
   code_for_opaque_mli ="val set_"^s^" : (t,"^t^",t) decorated_map2";
   code_for_ml = 
  "let set_"^s^"= { \n"^
  " dm2_input1 = Pmrp_involved_or_not_t.Involved(Pmrp_field_set.make [ Pmrp_types.F \""^s^"\" ]) ;\n"^
  " dm2_input2 = Pmrp_involved_or_not_t.Not_involved(\""^t^"\") ;\n"^
  " dm2_output = Pmrp_involved_or_not_t.Involved(Pmrp_field_set.make [ Pmrp_types.F \""^s^"\" ]) ;\n"^
  " dm2_additional_info = Some \"(setter)\" ;\n"^
  " dm2_actor= (fun x v->{ x with "^s^" = Some v});\n"^
  "} " ^ds ;
} ;;


let all_getter_items config =
  sort_items(
  Image.image getter_item config.Pmrp_types.fields_with_their_types
  );;
let all_setter_items config = 
  let typed_fields = Image.image (
   fun field -> (field,List.assoc field config.Pmrp_types.fields_with_their_types)
  ) config.Pmrp_types.mutable_fields in
    sort_items(
    Image.image setter_item typed_fields
    );;


let main_type_definition_item config =
   let definition_body = 
    "type t = {\n" ^
  (
   String.concat "\n" (Image.image
   (fun (Pmrp_types.F(s),t)->"   "^s^" : "^(protect t)^" option ;")
   config.Pmrp_types.fields_with_their_types)
  ) ^
  "\n}" in 
    {
     item_name = "t";
     code_for_transparent_mli = definition_body ;
     code_for_opaque_mli ="type t";
     code_for_ml = definition_body^" " ^ds ;
  } ;;

let decorated_map1_definition_item =
    let definition_body = 
    "type ('a,'b) decorated_map1   = { \n"^
    " dm_input : Pmrp_involved_or_not_t.t ;\n"^
    " dm_output : Pmrp_involved_or_not_t.t ;\n"^
    " dm_additional_info : string option ;\n"^
    " dm_actor: 'a -> 'b; \n"^
    "}"
    in 
    {
       item_name = "decorated_map1";
       code_for_transparent_mli = definition_body ;
       code_for_opaque_mli ="type ('a,'b) decorated_map1";
       code_for_ml = definition_body^" " ^ds ;
    } ;;

let decorated_map2_definition_item =
  let definition_body = 
    "type ('a1,'a2, 'b) decorated_map2   = { \n"^
    " dm2_input1 : Pmrp_involved_or_not_t.t ;\n"^
    " dm2_input2 : Pmrp_involved_or_not_t.t ;\n"^
    " dm2_output : Pmrp_involved_or_not_t.t ;\n"^
    " dm2_additional_info : string option ;\n"^
    " dm2_actor: 'a1 -> 'a2 -> 'b; \n"^
    "}"
    in 
    {
         item_name = "decorated_map2";
         code_for_transparent_mli = definition_body ;
         code_for_opaque_mli ="type ('a1,'a2, 'b) decorated_map2";
         code_for_ml = definition_body^" " ^ds ;
    } ;;
  


let all_type_definitions_items config =
  [
    (main_type_definition_item config);
    decorated_map1_definition_item;
    decorated_map2_definition_item;
  ] ;;

let dm1_applier_item =
    {
       item_name = "apply_decorated_map1";
       code_for_transparent_mli = "('a,'b) decorated_map1 -> 'a -> 'b" ;
       code_for_opaque_mli ="('a,'b) decorated_map1 -> 'a -> 'b";
       code_for_ml = "let apply_decorated_map1 f x   = f.dm_actor x " ^ds ;
    } ;;  

let dm2_applier_item =
    {
      item_name = "apply_decorated_map2";
      code_for_transparent_mli = "('a1,'a2, 'b) decorated_map2 -> 'a1 -> 'a2 -> 'b" ;
      code_for_opaque_mli ="('a1,'a2, 'b) decorated_map2 -> 'a1 -> 'a2 -> 'b";
      code_for_ml = "let apply_decorated_map2 f x1 x2 = f.dm2_actor x1 x2 " ^ds ;
    } ;;      

let all_applier_items =
    (sort_items [
      dm1_applier_item;
      dm2_applier_item
    ]) ;;

let get_exn_applier_item =
  {
    item_name = "Get_exn";
    code_for_transparent_mli = "exception Get_exn of string" ;
    code_for_opaque_mli ="exception Get_exn of string";
    code_for_ml = "exception Get_exn of string " ^ds ;
  } ;;      

let all_exn_items = 
    sort_items [
     get_exn_applier_item
    ];;

let all_non_typedef_non_exn_items config = 
   fold_merge_items [
    all_applier_items;
    all_getter_items config;
    all_setter_items config
   ];;

let all_items config = 
  (all_type_definitions_items config) @ 
  (all_exn_items) @
  (all_non_typedef_non_exn_items config) ;;

let ml_content config= 
  let items = all_items config in 
  let lines = Image.image (fun item -> item.code_for_ml) items in 
  "\n\n\n"^  
  (String.concat "" lines)^
  "\n\n\n";;


let write_ml_content config = 
  let  ap = config.Pmrp_types.receiving_file in
  let _ = Io.overwrite_with ap (ml_content config) in 
  Use_directive_in_initial_comment.prepend_or_replace_with_usual
   Coma_big_constant.This_World.root ap ;;


let config1 = {
  Pmrp_types.fields_with_their_types = [
  Pmrp_types.F "apple", "int";
  Pmrp_types.F "pear", "string";
  Pmrp_types.F "cranberry", "float";
  Pmrp_types.F "strawberry", "int list";
  ];
  fieldsets=[];
  mutable_fields=[Pmrp_types.F "apple";Pmrp_types.F "pear"];
  receiving_file = (Absolute_path.of_string 
  "lib/Poor_mans_row_polymorphism/pmrp_guinea_pig.ml");
} ;; 

let act1 () = write_ml_content config1 ;; 


  