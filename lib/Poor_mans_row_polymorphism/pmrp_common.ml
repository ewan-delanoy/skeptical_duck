(*

#use"lib/Poor_mans_row_polymorphism/pmrp_common.ml";;

*)


let make_field_set ?nondefault_name l=
    let unordered_fields = Image.image (fun (Pmrp_types.F s) -> s) l in 
    let temp = Ordered.sort Total_ordering.lex_for_strings unordered_fields in 
    let fields_in_order = Image.image (fun s -> (Pmrp_types.F s) ) temp in
    let final_name =  (
     match nondefault_name with 
      (Some given_name) -> given_name 
      | None -> "< "^(String.concat " ; " temp)^" >"
    ) in 
   {
     Pmrp_types.field_set_name = final_name;
     fields = fields_in_order;
   } ;;



