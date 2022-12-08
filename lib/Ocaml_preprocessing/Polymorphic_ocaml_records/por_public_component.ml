(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_public_component.ml";;

*)


module Private = struct 

  let annotated_text_for_field_getter 
     (por:Por_types.t) 
       (field:Por_types.field_t) =
       let fn = field.Por_types.field_name in 
       {
        Por_public_definition_t.value_name = fn ;
        lines_in_definition = ["let "^fn^" x = x."^
        (String.capitalize_ascii(por.Por_types.module_name))^
        "_t."^fn^" ;;"];
     } ;;
          
   let annotated_text_for_field_setter 
     (por:Por_types.t) 
       (field:Por_types.field_t) =
       let fn = field.Por_types.field_name 
       and vn = field.Por_types.var_name in 
       {
        Por_public_definition_t.value_name = "set_"^fn ;
        lines_in_definition = ["let set_"^fn^" x "^vn^" = { x with "^
        (String.capitalize_ascii(por.Por_types.module_name))^
        "_t."^fn^" = "^vn^"} ;;"];
     } ;;
     

   
   
   
   let annotated_text_for_getters por = Image.image (annotated_text_for_field_getter por)
     por.Por_types.fields ;;
   let annotated_text_for_setters por = Image.image (annotated_text_for_field_setter por)
     por.Por_types.fields ;;
   let annotated_text_for_crobj_symlinks  = 
    [
      {
        Por_public_definition_t.value_name = "of_concrete_object" ;
        lines_in_definition = ["let of_concrete_object = Private.Crobj.of_concrete_object ;;"];
      } ;
      {
        Por_public_definition_t.value_name = "to_concrete_object" ;
        lines_in_definition = ["let to_concrete_object = Private.Crobj.to_concrete_object ;;"];
      } ;
    ] ;;
    let annotated_text_for_extender_symlinks por=
      Image.image (
          fun (before_ext,after_ext) ->
            let ext_name = Por_common.extender_name (before_ext,after_ext) in 
            {
        Por_public_definition_t.value_name = ext_name ;
        lines_in_definition = ["let extend_"^ext_name^"  = Private.Extender."^ext_name^" ;;"];
      } 
      ) (Por_common.extensions_from_different_sources por) ;;

      let annotated_text_for_parenting_symlinks _por=
      [
        {
          Por_public_definition_t.value_name = "parent" ;
          lines_in_definition = ["let parent  = Private.Parent.get ;;"];
        } ;
        {
          Por_public_definition_t.value_name = "set_parent" ;
          lines_in_definition = ["let set_parent  = Private.Parent.set ;;"];
        } ;
       ] ;; 

       let annotated_text_for_typeinfo_symlinks _por=
       [
         {
           Por_public_definition_t.value_name = "show_fields" ;
           lines_in_definition = ["let show_fields  = Private.Type_information.show_fields ;;"];
         } ;
        ] ;;  
     
      let snippet_for_constructor_element (j,fd) = 
        let var_name  = Por_common.indexed_varname_for_field (j,fd) in 
        (String.make 3 ' ')^(fd.Por_types.field_name)^" = "^
        var_name^" ;" ;;     
   
    let annotated_definition_for_constructor por constructed_instance =
      let constructor_name = "construct_"^(String.uncapitalize_ascii constructed_instance) in 
      let full_instance = Por_common.get_instance por constructed_instance  in 
      let field_names = full_instance.Por_types.instance_fields in 
      let fields = Image.image (Por_common.get_field por)field_names in 
      let indexed_fields = Int_range.index_everything fields in 
      let filling_fields = Image.image snippet_for_constructor_element indexed_fields in 
      let indexed_and_labeled = Image.image (fun (j,fd)->
         "~"^(fd.Por_types.field_name)^":"^(Por_common.indexed_varname_for_field (j,fd))) indexed_fields in 
      let vars = String.concat " " indexed_and_labeled in 
      let main_module_name = (String.capitalize_ascii por.Por_types.module_name) in  
      {
        Por_public_definition_t.value_name = constructor_name ;
        lines_in_definition = ["let "^constructor_name^" "^vars^" = {";
        (String.make 3 ' ')^"Private.origin with ";
        (String.make 3 ' ')^main_module_name^"_t.type_name = \""^(String.capitalize_ascii constructed_instance)^"\" ;"]@
          filling_fields
        @["} ;;"];
      } ;;  

    let annotated_text_for_constructors por = 
      Image.image (annotated_definition_for_constructor por) por.Por_types.constructors
   ;;     
   
   let annotated_definition_for_restrictor por after_restr =
    let restr_name = "to_"^after_restr in 
    let main_module_name = (String.capitalize_ascii por.Por_types.module_name) in  
    {
      Por_public_definition_t.value_name = restr_name ;
      lines_in_definition = ["let "^restr_name^" fw  = ";
      (String.make 2 ' ')^"let tname = fw."^main_module_name^"_t.type_name in ";
      (String.make 2 ' ')^"let _ = Private.Type_information.check_inclusion \""^after_restr^"\" tname in ";
      (String.make 3 ' ')^"{";
      (String.make 3 ' ')^"fw with ";
      (String.make 3 ' ')^main_module_name^"_t.type_name = \""^(String.capitalize_ascii after_restr)^"\" ;"]
      @["} ;;"];
    } ;;  


   let annotated_text_for_restrictors por = 
    Image.image (annotated_definition_for_restrictor por) por.Por_types.restrictions
 ;;     


 let annotated_definition_for_print_out por =
  let main_module_name = (String.capitalize_ascii por.Por_types.module_name) in 
  {
    Por_public_definition_t.value_name = "print_out" ;
    lines_in_definition = ["let print_out (fmt:Format.formatter) fw  = "^
    "Format.fprintf fmt \"@[%s@]\" (\"< \"^(fw."^main_module_name^"_t.type_name)^\" >\") ;;";];
  } ;;  

end ;;

let main por = 
        Por_public_definition.expand_list (
          (Private.annotated_text_for_getters por)@
          (Private.annotated_text_for_setters por)@
          (Private.annotated_text_for_crobj_symlinks)@
          (Private.annotated_text_for_extender_symlinks por)@
          (Private.annotated_text_for_parenting_symlinks por)@
          (Private.annotated_text_for_typeinfo_symlinks por)@
          (Private.annotated_text_for_constructors por)@
          (Private.annotated_text_for_restrictors por)@
          [Private.annotated_definition_for_print_out por] );;   

     


