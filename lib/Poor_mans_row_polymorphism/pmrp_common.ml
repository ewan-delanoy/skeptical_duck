(*

#use"lib/Poor_mans_row_polymorphism/pmrp_common.ml";;

*)

let field_typename field = (field.Pmrp_types.field_type).Pmrp_types.type_name;; 

let make_field_set ?nondefault_name fields=
    let unordered_fieldnames = Image.image (fun fld -> fld.Pmrp_types.field_name) fields in 
    let temp = Ordered.sort Total_ordering.lex_for_strings unordered_fieldnames in 
    let fields_in_order = Image.image (fun s -> 
     List.find (fun fld -> fld.Pmrp_types.field_name=s) fields
    ) temp in
    let final_name =  (
     match nondefault_name with 
      (Some given_name) -> given_name 
      | None -> "< "^(String.concat " ; " temp)^" >"
    ) in 
   {
     Pmrp_types.field_set_name = final_name;
     fields = fields_in_order;
   } ;;

