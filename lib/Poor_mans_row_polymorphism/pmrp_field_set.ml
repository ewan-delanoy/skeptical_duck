(*

#use"lib/Poor_mans_row_polymorphism/pmrp_field_set.ml";;

*)


let make ?nondefault_name l=
    let unordered_fields = Image.image (fun (Pmrp_field_t.F s) -> s) l in 
    let temp = Ordered.sort Total_ordering.lex_for_strings unordered_fields in 
    let fields_in_order = Image.image (fun s -> (Pmrp_field_t.F s) ) temp in
    let final_name =  (
     match nondefault_name with 
      (Some given_name) -> given_name 
      | None -> "< "^(String.concat " ; " temp)^" >"
    ) in 
   {
     Pmrp_field_set_t.name = final_name;
     fields = fields_in_order;
   } ;;



