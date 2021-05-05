(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_element_in_record_or_variant.ml";;


*)



module Private = struct

  let c= "Concrete_"^"object_field.";;
  let wrap = Scct_common.wrap_in_parentheses_if_needed ;;
  
  let listify is_a_list name =
        if not(is_a_list) 
        then name 
        else (wrap name)^" list" ;; 

  let write_record_in_ocaml 
        (Scct_element_in_record_or_variant_t.U(item_name,is_a_list1, l)) = 
         let first_draft = Scct_inner_uple.write_type l in   
         item_name ^" : "^(listify is_a_list1 first_draft) 
       ;;

  let write_variant_in_ocaml 
       (Scct_element_in_record_or_variant_t.U(vague_item_name,is_a_list1, l)) =
        let item_name = String.capitalize_ascii  vague_item_name in 
        let first_draft = Scct_inner_uple.write_type l in  
        item_name ^" of "^(listify is_a_list1 first_draft) 
      ;;
  


  let hook_name variant_name =
        "hook_for_"^(String.lowercase_ascii variant_name) ;;
  
  let full_variant_name  module_name variant_name =
  (String.capitalize_ascii module_name)^"."^(String.capitalize_ascii variant_name) ;;     

  
  let converter_from_crobj_in_nonlisty_variant 
        ~module_name 
        (Scct_element_in_record_or_variant_t.U(item_name,_, l))=
    [
      "if hook = "^(hook_name item_name);
      "then "^(full_variant_name  module_name item_name)^"(";
    ]@
      (Scct_inner_uple.vertical_homogeneous_from_crobj ~tab_width:8 ~separator:"," "arg" l)@
    [  "     )";   
      "else"
    ] ;;

    let inner_converter_from_crobj_in_listy_variant  ~module_name 
        (Scct_element_in_record_or_variant_t.U(item_name,_,l))=
      let n = Scct_inner_uple.dimension(l) in 
      let local_tab_width = 4 in 
      let local_tab = String.make local_tab_width ' ' in 
      let not_indented_yet =
      (if n>1
      then  
            [
             (full_variant_name  module_name item_name)^"(Image.image ( fun uple_obj -> ";
             "let "^(Scct_common.arguments_in_input "urg" (Scct_inner_uple.dimension l))^" = Concrete_object_field.unwrap_bounded_uple uple_obj in ";
             "(";
            ]@
              (Scct_inner_uple.vertical_homogeneous_from_crobj ~tab_width:0 ~separator:"," "urg" l)@
            [")) temp)"]
      else let pl_atm = Scct_inner_uple.first_component l in  
           [
              (full_variant_name  module_name item_name)^"(Image.image (  ";
              "       "^(Scct_possibly_listy_atom.converter_from_crobj pl_atm);
              ") temp)"
           ]) in 
      Image.image (fun line->local_tab^line) not_indented_yet;;   


    let converter_from_crobj_in_listy_variant 
       ~module_name elt =
      let (Scct_element_in_record_or_variant_t.U(item_name,_,l))=elt in 
      [
        "if hook = "^(hook_name item_name);
        "then let temp = Concrete_object_field.unwrap_list arg1 in ";
      ]@
       (inner_converter_from_crobj_in_listy_variant  ~module_name elt)@
      [    
        "else"
      ] ;;  

    let converter_from_crobj_in_variant 
      ~module_name  elt = 
      match elt with
        (Scct_element_in_record_or_variant_t.U(item_name,is_a_list, l))->
       if is_a_list 
       then  converter_from_crobj_in_listy_variant ~module_name  elt 
       else  converter_from_crobj_in_nonlisty_variant ~module_name elt ;;   

end ;;

let converter_from_crobj_in_variant = Private.converter_from_crobj_in_variant ;;
let write_record_in_ocaml = Private.write_record_in_ocaml ;; 
let write_variant_in_ocaml = Private.write_variant_in_ocaml ;; 