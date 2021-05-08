(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_possibly_listy_uple.ml";;


*)



module Private = struct

  let c= "Concrete_"^"object_field.";;
  let wrap = Scct_common.wrap_in_parentheses_if_needed ;;

  
  let write_type ~separator
      (Scct_possibly_listy_uple_t.U(item_name,is_a_list1, l)) = 
       let first_draft = Scct_inner_uple.write_type l in   
       item_name ^separator^(Scct_common.listify is_a_list1 first_draft) 
     ;;

  let converter_from_crobj_in_nonlisty_variant 
        ~constructor ~question
        (Scct_possibly_listy_uple_t.U(item_name,_, l))=
    [
      "if "^question;
      "then "^constructor^"(";
    ]@
      (Scct_inner_uple.vertical_homogeneous_from_crobj ~tab_width:8 ~separator:"," "arg" l)@
    [  "     )";   
      "else"
    ] ;;

    let inner_converter_from_crobj_in_listy_variant  ~constructor 
        (Scct_possibly_listy_uple_t.U(item_name,_,l))=
      let n = Scct_inner_uple.dimension(l) in 
      let local_tab_width = 4 in 
      let local_tab = String.make local_tab_width ' ' in 
      let not_indented_yet =
      (if n>1
      then  
            [
             constructor^"(Image.image ( fun uple_obj -> ";
             "let "^(Scct_common.arguments_in_input "urg" (Scct_inner_uple.dimension l))^" = Concrete_object_field.unwrap_bounded_uple uple_obj in ";
             "(";
            ]@
              (Scct_inner_uple.vertical_homogeneous_from_crobj ~tab_width:0 ~separator:"," "urg" l)@
            [")) temp)"]
      else let pl_atm = Scct_inner_uple.first_component l in  
           [
              constructor^"(Image.image (  ";
              "       "^(Scct_possibly_listy_atom.converter_from_crobj pl_atm);
              ") temp)"
           ]) in 
      Image.image (fun line->local_tab^line) not_indented_yet;;   


    let converter_from_crobj_in_listy_variant 
       ~constructor ~question elt =
      let (Scct_possibly_listy_uple_t.U(item_name,_,l))=elt in 
      [
        "if "^question;
        "then let temp = Concrete_object_field.unwrap_list arg1 in ";
      ]@
       (inner_converter_from_crobj_in_listy_variant  ~constructor elt)@
      [    
        "else"
      ] ;;  

    let converter_from_crobj_in_variant 
      ~constructor ~question elt = 
      match elt with
        (Scct_possibly_listy_uple_t.U(item_name,is_a_list, l))->
       if is_a_list 
       then  converter_from_crobj_in_listy_variant ~constructor ~question elt 
       else  converter_from_crobj_in_nonlisty_variant ~constructor ~question elt ;;   

end ;;

let converter_from_crobj_in_variant = Private.converter_from_crobj_in_variant ;;
let write_type = Private.write_type ;; 