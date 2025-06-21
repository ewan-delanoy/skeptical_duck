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

let inhabitated_definition_item field =
  let s = field.Pmrp_types.field_name 
  and inhabitated = field.Pmrp_types.field_type in 
  {
   item_name = s^"_inhabitated_type";
   code_for_transparent_mli ="val "^s^"_inhabitated_type : Pmrp_types.inhabitated_type" ;
   code_for_opaque_mli ="";
   code_for_ml = 
  "let "^s^"_inhabitated_type = { \n"^
  " Pmrp_types.type_name = \""^(String.escaped inhabitated.Pmrp_types.type_name)^"\"  ;\n"^
  " inhabitant = \""^(String.escaped inhabitated.Pmrp_types.inhabitant)^"\"  ;\n"^
  "} " ^ds ;
} ;;

let all_inhabitated_definition_items config =
  sort_items(
  Image.image inhabitated_definition_item config.Pmrp_types.fields_in_config
  );;

let field_definition_item field =
  let s = field.Pmrp_types.field_name  in 
    {
     item_name = s^"_field";
     code_for_transparent_mli ="val "^s^"_field : Pmrp_types.field" ;
     code_for_opaque_mli ="";
     code_for_ml = 
    "let "^s^"_field = { \n"^
    " Pmrp_types.field_name = \""^s^"\"  ;\n"^
    " field_type = "^s^"_inhabitated_type  ;\n"^
    " is_mutable = "^(string_of_bool field.Pmrp_types.is_mutable)^" ;\n"^
    "} " ^ds ;
  } ;;
  
let all_field_definition_items config =
    sort_items(
    Image.image field_definition_item config.Pmrp_types.fields_in_config
    );;

let singleton_fieldset_item field =
  let s = field.Pmrp_types.field_name  in 
  {
    item_name = s^"_singleton_fieldset";
    code_for_transparent_mli ="val "^s^"_singleton_fieldset : Pmrp_types.field_set" ;
    code_for_opaque_mli ="";
    code_for_ml = 
      "let "^s^"_singleton_fieldset = Pmrp_common.make_field_set [ "^s^"_field ] "^ds;
  } ;;
      
let all_singleton_fieldset_items config =
  sort_items(
        Image.image singleton_fieldset_item config.Pmrp_types.fields_in_config
  );;
    
let nonsingleton_fieldset_item field_set =
  let fs = field_set.Pmrp_types.field_set_name  
  and fields = field_set.Pmrp_types.fields in 
  let fields_description = String.concat " ; " (Image.image (
    fun field -> (field.Pmrp_types.field_name)^"_field"
  ) fields) in 
  {
      item_name = fs;
      code_for_transparent_mli ="val "^fs^" : Pmrp_types.field_set" ;
      code_for_opaque_mli ="";
      code_for_ml = 
      "let "^fs^" = { \n"^
      " Pmrp_types.field_set_name = \""^fs^"\"  ;\n"^
      " fields = [ "^fields_description^" ]  ;\n"^
      "} " ^ds ;
  } ;;
        
let all_nonsingleton_fieldset_items config =
  sort_items(
    Image.image nonsingleton_fieldset_item config.Pmrp_types.field_sets
  );;

let empty_object_item config =
    let definition_body = 
     "let empty_object = {\n" ^
   (
    String.concat "\n" (Image.image
    (fun 
    field ->
   let s = field.Pmrp_types.field_name  in  
     "   "^s^" = None ;")
    config.Pmrp_types.fields_in_config)
   ) ^
   "\n}" in 
     {
      item_name = "empty_object";
      code_for_transparent_mli = "val empty_object:t" ;
      code_for_opaque_mli ="";
      code_for_ml = definition_body^" " ^ds ;
   } ;;
 
let abstainer_item config excluded_field =
  let es = excluded_field.Pmrp_types.field_name in 
  let definition_body = 
      
     "let "^es^"_abstainer = {\n" ^
   (
    String.concat "\n" (Image.image
    (fun 
    field ->
   let content = (
      if field = excluded_field 
      then "None" 
      else 
      let holder = (field.Pmrp_types.field_type).Pmrp_types.inhabitant in   
      "Some( "^holder^" )" 
   ) in     
     "   "^field.Pmrp_types.field_name^" = "^content^" ;")
    config.Pmrp_types.fields_in_config)
   ) ^
   "\n}" in 
     {
      item_name = es^"_abstainer";
      code_for_transparent_mli = "val "^es^"_abstainer:t" ;
      code_for_opaque_mli ="";
      code_for_ml = definition_body^" " ^ds ;
   } ;;
  
let all_abstainer_items config =
    sort_items(
      Image.image (abstainer_item config) config.Pmrp_types.fields_in_config
);;   

let all_cumulative_data_items config = 
List.flatten [
  all_inhabitated_definition_items config;
  all_field_definition_items config;
  all_singleton_fieldset_items config;
  all_nonsingleton_fieldset_items config;
  [empty_object_item config];
  all_abstainer_items config;
 ];;

let getter_item field =
  let s = field.Pmrp_types.field_name 
  and t = Pmrp_common.field_typename field in 
  {
   item_name = s;
   code_for_transparent_mli ="val "^s^" : (t,"^t^") decorated_map1" ;
   code_for_opaque_mli ="val "^s^" : (t,"^t^") decorated_map1";
   code_for_ml = 
  "let "^s^" = { \n"^
  " dm1_input1 = Pmrp_types.Involved("^s^"_singleton_fieldset) ;\n"^
  " dm1_output = Pmrp_types.Not_involved("^s^"_inhabitated_type) ;\n"^
  " dm1_additional_info = Some \"(getter)\" ;\n"^
  " dm1_actor= (fun x ->try Option.get(x."^s^") with _ ->raise(Get_exn(\""^s^"\")));\n"^
  "} " ^ds ;
} ;;

let setter_item field =
  let s = field.Pmrp_types.field_name 
  and t = Pmrp_common.field_typename field in 
  {
   item_name = "set_"^s;
   code_for_transparent_mli ="val set_"^s^" : (t,"^t^",t) decorated_map2" ;
   code_for_opaque_mli ="val set_"^s^" : (t,"^t^",t) decorated_map2";
   code_for_ml = 
  "let set_"^s^"= { \n"^
  " dm2_input1 = Pmrp_types.Involved("^s^"_singleton_fieldset) ;\n"^
  " dm2_input2 = Pmrp_types.Not_involved("^s^"_inhabitated_type) ;\n"^
  " dm2_output = Pmrp_types.Involved("^s^"_singleton_fieldset) ;\n"^
  " dm2_additional_info = Some \"(setter)\" ;\n"^
  " dm2_actor= (fun x v->{ x with "^s^" = Some v});\n"^
  "} " ^ds ;
} ;;

let setter_item_opt field = 
   if field.Pmrp_types.is_mutable 
   then Some(setter_item field) 
   else None ;;


let all_getter_items config =
  sort_items(
  Image.image getter_item config.Pmrp_types.fields_in_config
  );;
let all_setter_items config = 
    sort_items(
    List.filter_map setter_item_opt config.Pmrp_types.fields_in_config
    );;


let main_type_definition_item config =
   let definition_body = 
    "type t = {\n" ^
  (
   String.concat "\n" (Image.image
   (fun 
   field ->
  let s = field.Pmrp_types.field_name 
  and t = Pmrp_common.field_typename field in  
    "   "^s^" : "^(protect t)^" option ;")
   config.Pmrp_types.fields_in_config)
  ) ^
  "\n}" in 
    {
     item_name = "t";
     code_for_transparent_mli = definition_body ;
     code_for_opaque_mli ="type t";
     code_for_ml = definition_body^" " ^ds ;
  } ;;

let decorated_map_definition_with_n_arguments n=
  let sn = string_of_int n in 
  let uple = "("^(String.concat "," (Int_range.scale (fun j->"'a"^(string_of_int j)) 1 n))^",'b)"
  and arrows = (String.concat " -> " (Int_range.scale (fun j->"'a"^(string_of_int j)) 1 n))^" -> 'b" in 
    let definition_body = 
    "type "^uple^" decorated_map"^sn^" = { \n"^
    (
     String.concat ""
      (Int_range.scale (fun j->
        " dm"^sn^"_input"^(string_of_int j)^
        " : Pmrp_types.involved_or_not ;\n"
        ) 1 n)
    )^
    " dm"^sn^"_output : Pmrp_types.involved_or_not ;\n"^
    " dm"^sn^"_additional_info : string option ;\n"^
    " dm"^sn^"_actor: "^arrows^" ; \n"^
    "}"
    in 
    {
       item_name = "decorated_map"^sn;
       code_for_transparent_mli = definition_body ;
       code_for_opaque_mli ="type "^uple^" decorated_map"^sn;
       code_for_ml = definition_body^" " ^ds ;
    } ;;


let max_number_of_arguments = 2;;  


let all_type_definitions_items config =
  [
    (main_type_definition_item config)
  ]
  @
  (Int_range.scale decorated_map_definition_with_n_arguments 
  1 max_number_of_arguments)
  ;;

let applier_with_n_arguments n=
  let sn = string_of_int n in 
  let uple = "("^(String.concat "," (Int_range.scale (fun j->"'a"^(string_of_int j)) 1 n))^",'b)" 
  and axes = (String.concat " " (Int_range.scale (fun j->"x"^(string_of_int j)) 1 n))
  and arrows = (String.concat " -> " (Int_range.scale (fun j->"'a"^(string_of_int j)) 1 n)) in 
  let val_signature =  
    "val apply_decorated_map"^sn^" : "^
    uple^" decorated_map"^sn^" -> "^arrows^" -> 'b" in  
  let definition_body = 
    "let  apply_decorated_map"^sn^" f " ^ axes ^ "= \n"^
    " f.dm"^sn^"_actor "^axes^" "^ds
    in 
    {
       item_name = "apply_decorated_map"^sn;
       code_for_transparent_mli = val_signature ;
       code_for_opaque_mli =val_signature;
       code_for_ml = definition_body ;
    } ;;

let all_applier_items =
    (sort_items 
    (Int_range.scale applier_with_n_arguments 
     1 max_number_of_arguments)
    ) ;;

let get_exn_item =
  {
    item_name = "Get_exn";
    code_for_transparent_mli = "exception Get_exn of string" ;
    code_for_opaque_mli ="exception Get_exn of string";
    code_for_ml = "exception Get_exn of string " ^ds ;
  } ;;      

let all_exn_items = 
    sort_items [
     get_exn_item
    ];;

let active_fields_item config =
  let definition_body = 
       "let active_fields x = \n" ^
       " let comparator = [\n" ^
     (
      String.concat "\n" (Image.image
      (fun 
      field ->
     let s = field.Pmrp_types.field_name in  
       " ( "^s^"_field , x."^s^" = None ) ;")
      config.Pmrp_types.fields_in_config)
     ) ^
     "\n ] in \n"^
     " List.filter_map (fun (fld,is_active)->\n"^
     "   if is_active \n"^
     "   then Some fld\n"^
     "   else None) comparator" in 
  {
    item_name = "active_fields";
    code_for_transparent_mli = "val active_fields: t -> field list" ;
    code_for_opaque_mli ="";
    code_for_ml = definition_body^" " ^ds ;
  } ;;
   
let all_primary_non_typedef_non_exn_items config = 
    fold_merge_items [
     all_applier_items;
     all_getter_items config;
     all_setter_items config;
    ];;
 
let test_for_getexn_raise_with_n_arguments n=
    let sn = string_of_int n in 
    let axes = (String.concat " " (Int_range.scale (fun j->"x"^(string_of_int j)) 1 n))
    and arrows = (String.concat " -> " (Int_range.scale (fun j->"'a"^(string_of_int j)) 1 n)) in 
      let definition_body = 
      "let getexn_is_raised_in_dm"^sn^" f "^axes^" =  \n"^
      "try (fun _ ->false)(f "^axes^") with \n" ^
      "Get_exn _ -> true "^ds in 
      {
         item_name = "getexn_is_raised_in_dm"^sn;
         code_for_transparent_mli = "val getexn_is_raised_in_dm"^sn^" : ( t -> "^arrows^" -> 'b) -> "^arrows^" -> bool";
         code_for_opaque_mli ="";
         code_for_ml = definition_body ;
      } ;;


let all_secondary_items config= 
  [   
     active_fields_item config;
  ] @
  (
    (Int_range.scale test_for_getexn_raise_with_n_arguments
     1 max_number_of_arguments)
  );;


let all_items config = 
  (all_type_definitions_items config) @ 
  (all_exn_items) @
  (all_cumulative_data_items config) @
  (all_primary_non_typedef_non_exn_items config) @
  (all_secondary_items config) ;;

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


let apple_field = 
  {Pmrp_types.field_name = "apple"; 
   field_type={Pmrp_types.type_name="int";
   inhabitant="0";
   };
   is_mutable=true }  ;;

let pear_field = 
    {Pmrp_types.field_name = "pear"; 
     field_type={Pmrp_types.type_name="string";
     inhabitant="\"\"";
     };
     is_mutable=false }  ;;   

let cranberry_field = 
  {Pmrp_types.field_name = "cranberry"; 
    field_type={Pmrp_types.type_name="float";
    inhabitant="0.";
    };
    is_mutable=true }  ;;

let strawberry_field = 
  {Pmrp_types.field_name = "strawberry"; 
    field_type={Pmrp_types.type_name="int list";
    inhabitant="[]"};
    is_mutable=false }  ;;

let nonberry_field_set = {
  Pmrp_types.field_set_name ="nonberry";
  fields = [apple_field;pear_field];
} ;;

let config1 = {
  Pmrp_types.fields_in_config = [
    apple_field;
    pear_field;
    cranberry_field;
    strawberry_field;
  ];
  field_sets=[nonberry_field_set];
  receiving_file = (Absolute_path.of_string 
  "lib/Poor_mans_row_polymorphism/pmrp_guinea_pig.ml");
} ;; 

let act1 () = write_ml_content config1 ;; 

(*
let sg1 = 
  let config = config1 in 
  [  
  (all_type_definitions_items config) ;
  (all_exn_items) ;
  (all_cumulative_data_items config) ;
  (all_primary_non_typedef_non_exn_items config) ;
  (all_secondary_items config) 
];;

let sg2 = Image.image (Image.image (fun item ->item.item_name)) sg1 ;;
let sg3 = 
let config = config1 in 
   [
   all_applier_items;
   all_getter_items config;
   all_setter_items config;
  ];;

let sg4 = Image.image (Image.image (fun item ->item.item_name)) sg3 ;;  
*)