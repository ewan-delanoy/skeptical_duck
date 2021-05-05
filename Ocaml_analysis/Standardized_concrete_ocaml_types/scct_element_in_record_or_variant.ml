(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_element_in_record_or_variant.ml";;


*)



module Private = struct

    let c= "Concrete_"^"object_field.";;
    let wr = Scct_common.wrap_in_parentheses_if_needed ;;
    
    let listify is_a_list name =
          if not(is_a_list) 
          then name 
          else (wr name)^" list" ;; 

    let write_record_in_ocaml 
          (Scct_element_in_record_or_variant_t.U(item_name,is_a_list1, l)) =
           let temp1 = Image.image (
             fun  (varname,is_a_list2,atm) ->
                wr(listify is_a_list2 (Scct_atomic_type.write_in_ocaml atm))
          ) l in 
           let first_draft = String.concat " * " temp1 in     
           item_name ^" : "^(listify is_a_list1 first_draft) 
         ;;

    let write_variant_in_ocaml 
         (Scct_element_in_record_or_variant_t.U(vague_item_name,is_a_list1, l)) =
          let temp1 = Image.image (
            fun  (varname,is_a_list2,atm) ->
               Scct_common.wrap_in_parentheses_if_needed(
                   listify is_a_list2 (Scct_atomic_type.write_in_ocaml atm))
         ) l in 
          let first_draft = String.concat " * " temp1 in   
          let item_name = String.capitalize_ascii  vague_item_name in 
          item_name ^" of "^(listify is_a_list1 first_draft) 
        ;;
    

    let arguments_in_variant_output third_tab_width argname l=
       let temp1 = Ennig.index_everything l in  
       let n = List.length(l) in 
       Image.image (
         fun (k,(varname,is_a_list,atm)) ->
            let comma_or_not = (if (k=n)&&(n>1) then "" else ",") in
            (String.make  third_tab_width ' ')^(Scct_atomic_type.converter_from_crobj atm)
            ^" "^argname^(string_of_int k)^comma_or_not
       ) temp1 ;;

    let hook_name variant_name =
          "hook_for_"^(String.lowercase_ascii variant_name) ;;
    
    let full_variant_name  module_name variant_name =
    (String.capitalize_ascii module_name)^"."^(String.capitalize_ascii variant_name) ;;     

    let preliminary_in_variant ~variant_name ()=
       "let "^(hook_name variant_name)^" = salt ^ \""^
        (String.capitalize_ascii variant_name) ^ "\" " ^ 
        Particular_string.double_semicolon        
    ;;       

    let converter_from_crobj_in_nonlisty_variant 
          ~module_name 
          (Scct_element_in_record_or_variant_t.U(item_name,_, l))=
      [
        "if hook = "^(hook_name item_name);
        "then "^(full_variant_name  module_name item_name)^"(";
      ]@
        ( arguments_in_variant_output 5 "arg" l)@
      [  "      )";   
        "else"
      ] ;;

      let converter_from_crobj_in_listy_variant 
         ~module_name 
        (Scct_element_in_record_or_variant_t.U(item_name,_, l))=
        [
          "if hook = "^(hook_name item_name);
          "then let temp = Concrete_object_field.unwrap_list arg1 in ";
          "     Image.image ( fun uple_obj -> ";
          "       let "^(Scct_common.arguments_in_input "urg" (List.length l))^" = Concrete_object_field.unwrap_bounded_uple uple_obj in ";
          "        "^(full_variant_name  module_name item_name)^"(";
        ]@
          ( arguments_in_variant_output 4 "urg" l)@
        [  "       )) temp";   
           "else"
        ] ;;  

      let converter_from_crobj_in_variant 
        ~module_name  elt = 
        match elt with
          (Scct_element_in_record_or_variant_t.U(item_name,is_a_list, l))->
         if is_a_list 
         then converter_from_crobj_in_listy_variant ~module_name  elt 
         else  converter_from_crobj_in_nonlisty_variant ~module_name elt ;;   

end ;;

let converter_from_crobj_in_variant = Private.converter_from_crobj_in_variant ;;
let preliminary_in_variant = Private.preliminary_in_variant ;; 
let write_record_in_ocaml = Private.write_record_in_ocaml ;; 
let write_variant_in_ocaml = Private.write_variant_in_ocaml ;; 