(*

#use"lib/Text_editing/crobj_conversion_boilerplate.ml";;

*)

module Private = struct 

  let write_definitions_for_labels type_name fields =
    let m = Max.list (Image.image String.length fields) in 
    let temp = Image.image (
      fun fld->"let " ^fld ^"_label"^
       (String.make (m-String.length fld) ' ')^
      " = salt ^ \"" ^ fld ^ "\" ;;"
    ) fields in 
    let first_line = "let salt = \""^(
     String.capitalize_ascii type_name
    )^".\" ;;" in 
    let temp2 = first_line :: temp in
    "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n" ;;
 
 let write_conversion_from_crobj type_name fields =
     let indexed_fields =  Int_range.index_everything fields in 
     let temp = Image.image (
       fun (idx,fld)->
         let prefix = (if idx=1 
           then  String.capitalize_ascii type_name ^ "."
           else "") in
         "   "^prefix^fld ^" = Crobj_converter   (g "^fld^"_label);"
     ) indexed_fields in 
     "let of_concrete_object ccrt_obj = \n"^
     "  let g=Concrete_object.get_record ccrt_obj in\n"^
     " {\n"^
     (String.concat "\n" temp)^
     "\n } ;;" ;;
 
 let write_conversion_to_crobj var_name type_name  fields =
       let taker =  "." ^ String.capitalize_ascii type_name ^ "." in 
       let temp = Image.image (
         fun fld->
           "   "^fld ^"_label,  Crobj_converter   ("^var_name^taker^fld^");"
       ) fields in 
       "let to_concrete_object "^var_name^" = \n"^
       "  let items=\n"^
       " [\n"^
       (String.concat "\n" temp)^
       "\n ] in \n"^
       "Concrete_object_t.Record items;;" ;;
 
 let full_conversion_code ~var_name ~type_name ~fields =
  "\n\n\n"^
  (write_definitions_for_labels type_name fields)^"\n\n\n"^
  (write_conversion_from_crobj type_name fields) ^"\n\n\n"^ 
  (write_conversion_to_crobj var_name type_name  fields) ^"\n\n\n";; 
 
end ;;

let write = Private.full_conversion_code ;;
