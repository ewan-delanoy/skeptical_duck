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

let getter_item (Pmrp_field_t.F(s),t) =
  {
   item_name = s;
   code_for_transparent_mli ="val "^s^" : "^t^" self_to_outside_mapper_t" ;
   code_for_opaque_mli ="val "^s^" : "^t^" self_to_outside_mapper_t";
   code_for_ml = 
  "let "^s^" = { \n"^
  " sto_input_fieldset = Pmrp_field_set.make [ Pmrp_field_t.F \""^s^"\" ] ;\n"^
  " sto_additional_info = Some \"(getter)\" ;\n"^
  " sto_output_type = \""^t^"\" ;\n"^
  " sto_actor = (fun x ->x."^s^");\n"^
  "} " ^ds ;
} ;;

let all_getter_items config =
  sort_items(
  Image.image getter_item config.Pmrp_config_t.fields_with_their_types
  );;

let main_type_definition_item config =
   let definition_body = 
    "type t = {\n" ^
  (
   String.concat "\n" (Image.image
   (fun (Pmrp_field_t.F(s),t)->"   "^s^" : "^t^" ;")
   config.Pmrp_config_t.fields_with_their_types)
  ) ^
  "\n}" in 
    {
     item_name = "t";
     code_for_transparent_mli = definition_body ;
     code_for_opaque_mli ="type t";
     code_for_ml = definition_body^" " ^ds ;
  } ;;
let ots_type_definition_item =
  let definition_body = 
  "type 'a outside_to_self_mapper_t   = { \n"^
  " ots_input_fieldset : string ;\n"^
  " ots_output_fieldset : Pmrp_field_set_t.t ;\n"^
  " ots_additional_info : string option ;\n"^
  " ots_actor: 'a -> t; "^
  "}"
  in 
  {
     item_name = "outside_to_self_mapper_t";
     code_for_transparent_mli = definition_body ;
     code_for_opaque_mli ="type 'a outside_to_self_mapper_t";
     code_for_ml = definition_body^" " ^ds ;
  } ;;

let sto_type_definition_item =
    let definition_body = 
    "type 'b self_to_outside_mapper_t   = { \n"^
  " sto_input_fieldset : Pmrp_field_set_t.t ;\n"^
  " sto_output_type : string ;\n"^
  " sto_additional_info : string option ;\n"^
  " sto_actor: t -> 'b; "^
  "}"
    in 
    {
       item_name = "self_to_outside_mapper_t";
       code_for_transparent_mli = definition_body ;
       code_for_opaque_mli ="type 'b self_to_outside_mapper_t";
       code_for_ml = definition_body^" " ^ds ;
    } ;;  

let sts_type_definition_item =
      let definition_body = 
    "type self_to_self_mapper_t   = { \n"^
  " sts_input_fieldset : Pmrp_field_set_t.t ;\n"^
  " sts_output_fieldset : Pmrp_field_set_t.t ;\n"^
  " sts_additional_info : string option ;\n"^
  " sts_actor: t -> t; "^
  "}" in 
      {
         item_name = "self_to_self_mapper_t";
         code_for_transparent_mli = definition_body ;
         code_for_opaque_mli ="type self_to_self_mapper_t";
         code_for_ml = definition_body^" " ^ds ;
      } ;;     

let all_type_definitions_items config =
  (main_type_definition_item config)::
  (sort_items [
    ots_type_definition_item;
    sto_type_definition_item;
    sts_type_definition_item
  ]) ;;

let ots_applier_item =
    {
       item_name = "apply_ots";
       code_for_transparent_mli = "'a outside_to_self_mapper_t -> 'a -> t" ;
       code_for_opaque_mli ="'a outside_to_self_mapper_t -> 'a -> t";
       code_for_ml = "let apply_ots f x   = f.ots_actor x " ^ds ;
    } ;;  

let sto_applier_item =
  {
    item_name = "apply_sto";
    code_for_transparent_mli = "'a self_to_outside_mapper_t -> t -> 'a" ;
    code_for_opaque_mli ="'a self_to_outside_mapper_t -> t -> 'a";
    code_for_ml = "let apply_sto f x   = f.sto_actor x " ^ds ;
  } ;;      

let sts_applier_item =
  {
    item_name = "apply_sts";
    code_for_transparent_mli = "val apply_sts : self_to_self_mapper_t -> t -> t" ;
    code_for_opaque_mli ="val apply_sts : self_to_self_mapper_t -> t -> t";
    code_for_ml = "let apply_sts f x   = f.sts_actor x " ^ds ;
  } ;;      

let all_applier_items =
    (sort_items [
      ots_applier_item;
      sto_applier_item;
      sts_applier_item
    ]) ;;

let all_non_typedef_items config = 
   fold_merge_items [
    all_applier_items;
    all_getter_items config
   ];;

let all_items config = 
  (all_type_definitions_items config) @ (all_non_typedef_items config) ;;

let ml_content config= 
  let items = all_items config in 
  let lines = Image.image (fun item -> item.code_for_ml) items in 
  "\n\n\n"^  
  (String.concat "" lines)^
  "\n\n\n";;


let write_ml_content config = 
  let  ap = config.Pmrp_config_t.receiving_file in
  let _ = Io.overwrite_with ap (ml_content config) in 
  Use_directive_in_initial_comment.prepend_or_replace_with_usual
   Coma_big_constant.This_World.root ap ;;


let config1 = {
  Pmrp_config_t.fields_with_their_types = [
  Pmrp_field_t.F "apple", "int";
  Pmrp_field_t.F "pear", "string";
  Pmrp_field_t.F "cranberry", "float";
  Pmrp_field_t.F "strawberry", "int list";
  ];
  fieldsets=[];
  mutable_fields=[];
  receiving_file = (Absolute_path.of_string 
  "lib/Poor_mans_row_polymorphism/pmrp_guinea_pig.ml");
} ;; 

let act1 () = write_ml_content config1 ;; 


  