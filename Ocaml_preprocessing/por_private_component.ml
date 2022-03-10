(*

#use"Ocaml_preprocessing/por_private_component.ml";;

*)


module Private = struct 

      module Crobj = struct 
      
      let text_for_label 
            (por:Polymorphic_ocaml_record_t.t) 
              max_namesize
             (field:Polymorphic_ocaml_record_t.field_t) = 
             let fn = field.Polymorphic_ocaml_record_t.field_name in 
             let offset = String.make (max_namesize-String.length fn) ' ' in 
             "let label_for_"^fn^offset^" = salt ^ \""^fn^"\" ;;" ;;
          
       
      let fields_with_crobj_conversion por =
             Option.filter_and_unpack (
               fun fld ->
                match fld.Polymorphic_ocaml_record_t.crobj_converters with 
                None -> None 
                |Some(of_crobj,to_crobj) -> Some(fld,(of_crobj,to_crobj))
             ) por.Polymorphic_ocaml_record_t.fields  ;;
       
      let special_type_name_field = {
                Polymorphic_ocaml_record_t.field_name = "type_name" ;
                field_type = "" ;
                var_name = "" ;
                default_value = "" ;
                crobj_converters = None ;
             } ;;
       
      let text_for_labels por =
           let crobjed_fields = special_type_name_field ::(Image.image fst (fields_with_crobj_conversion por)) in 
           let max_namesize = snd (Max.maximize_it (fun fd->
             String.length(fd.Polymorphic_ocaml_record_t.field_name)) crobjed_fields) in 
           String.concat "\n"
             ("let salt = \"Fw_poly_t.\" ;;" ::
           (Image.image (text_for_label por max_namesize) crobjed_fields)) ;;  
          
      let text_for_ofcrobj_element 
           (por:Polymorphic_ocaml_record_t.t) 
           fld = 
           let vowel = (
             match fld.Polymorphic_ocaml_record_t.crobj_converters with 
             None ->  (fld.Polymorphic_ocaml_record_t.default_value) 
           | Some(of_crobj,to_crobj) ->
               of_crobj^" (g label_for_"^(fld.Polymorphic_ocaml_record_t.field_name)^") "
           ) in 
               (String.make 3 ' ')^(fld.Polymorphic_ocaml_record_t.field_name)^" = "^
               vowel^" ;" ;;
       
      let text_for_ofcrobj_converter por = 
           let all_fields = por.Polymorphic_ocaml_record_t.fields in 
           let temp1 = (String.make 3 ' ')^(String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^
           "_t.type_name = Crobj_converter.string_of_concrete_object (g label_for_type_name) ;" 
           and temp2 = Image.image (text_for_ofcrobj_element por) all_fields in 
           String.concat "\n"
           ( 
           [ "let of_concrete_object ccrt_obj = ";
             " let g=Concrete_object.get_record ccrt_obj in ";
             " {"
            ]@
            (temp1::temp2)
            @
            [
              "} ;;"
             ]) ;;
       
      let text_for_tocrobj_element 
             (por:Polymorphic_ocaml_record_t.t) 
             (fld,(of_crobj,to_crobj)) = 
             let field_name  = fld.Polymorphic_ocaml_record_t.field_name in 
                 (String.make 4 ' ')^" label_for_"^field_name^", "^ 
                 to_crobj^" fw."^(String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^"_t."^
                 field_name^" ;" ;;    
       
      let text_for_tocrobj_converter por = 
           let fields_with_crobj = fields_with_crobj_conversion por  in 
           let temp1 = (String.make 4 ' ')^" label_for_type_name,"^ 
           " Crobj_converter.string_to_concrete_object fw."^
            (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^"_t.type_name ;"
           and temp2 = Image.image (text_for_tocrobj_element por) fields_with_crobj in 
           String.concat "\n"
           ( 
            [ "let to_concrete_object fw = ";
              " let items =  ";
              " ["
            ]@
             (temp1 :: temp2)
             @
            [
              " ] in ";
              " Concrete_object_t.Record items ;;"
            ]) ;;
       
         
       
      
      
      let full_text por =
            if not(por.Polymorphic_ocaml_record_t.has_crobj_conversion)
            then ""  
            else
            "module Crobj = struct \n"^
            (text_for_labels por)^"\n\n"^
            (text_for_ofcrobj_converter por)^"\n\n"^
            (text_for_tocrobj_converter por)^"\n\n"^
            "\nend;; \n\n\n"      
      
      end ;;
      
      module Extender = struct 
      
      let extender_data por (before_ext,after_ext) =
            let ext_name = Por_common.extender_name (before_ext,after_ext) in 
            let inst_before = Por_common.get_instance por before_ext 
            and inst_after = Por_common.get_instance por after_ext  in 
            let field_names_before = inst_before.Polymorphic_ocaml_record_t.fields 
            and field_names_after = inst_after.Polymorphic_ocaml_record_t.fields in 
            let _ = Por_common.check_inclusion field_names_before field_names_after in 
            let extra_field_names = List.filter (fun fdn->not(List.mem fdn field_names_before)) field_names_after in 
            let extra_fields = Image.image (Por_common.get_field por) extra_field_names in 
            let indexed_extra_fields = Ennig.index_everything extra_fields in 
            let indexed_and_labeled = Image.image (fun (j,fd)->
                     (fd.Polymorphic_ocaml_record_t.field_name,Por_common.indexed_varname_for_field (j,fd))) indexed_extra_fields in 
            (ext_name,indexed_and_labeled);;   
      
      let snippet_for_element (field_name,indexed_varname) = 
          (String.make 3 ' ')^field_name^" = "^indexed_varname^" ;" ;;    
      
            
      let text_for_extender por (before_ext,after_ext) =
          let (ext_name,indexed_and_labeled) = extender_data por (before_ext,after_ext) in 
          let filling_fields = Image.image (snippet_for_element) indexed_and_labeled in 
          let joined_indexes_and_labels = Image.image (fun (field_name,indexed_varname)->
             "~"^field_name^":"^indexed_varname) indexed_and_labeled in 
          let vars = String.concat " " joined_indexes_and_labels in 
          let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in 
          String.concat "\n" (["let "^ext_name^" fw "^vars^" = {";
            (String.make 3 ' ')^"fw with ";
            (String.make 3 ' ')^main_module_name^"_t.type_name = \""^(String.capitalize_ascii after_ext)^"\" ;"]@
              filling_fields
            @["} ;;"]);;  
       
       let full_text por = 
          String.concat "\n" (Image.image (text_for_extender por) por.Polymorphic_ocaml_record_t.extensions)
       ;;       
            
       let full_text por =
            let l = Por_common.extensions_from_different_sources por in 
            if l = []
            then ""  
            else
            "module Extender = struct \n\n"^
            (String.concat "\n" (Image.image (text_for_extender por) l))^
            "\nend;;"     ;; 
      
      end ;;      
      
      
      module Parent = struct
      
      let text_for_main_list por =
            let l = por.Polymorphic_ocaml_record_t.designated_parents in 
            let tempf = (fun r-> Strung.enclose(String.capitalize_ascii r)) in 
            let temp1 = Image.image (fun (s,t)->
                  (String.make 4 ' ')^(tempf s)^" , "^(tempf t)^" ;"
            ) l in 
            String.concat "\n"
            ("let designated_parents = ["::
            (temp1@
            ["] ;;"]));;
              
      let text_for_exceptions =
            String.concat "\n"
           [ 
             "exception No_designated_parent of string ;; ";
             "exception Set_parent_exn of string ;; ";
            ] ;; 
              
      let text_for_get_parent_name por = 
            let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in 
            String.concat "\n"
            ([
                  "let get_parent_name fw = ";
                  " let name = fw."^main_module_name^"_t.type_name in ";
                  " match List.assoc_opt name designated_parents with ";
                  "  Some(answer) ->answer";
                  " |None -> raise (No_designated_parent(name)) ;;"
            ]);;
         
      let text_for_parent_setter por (sibling,parent) =   
            let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) 
            and (ext_name,indexed_and_labeled) = Extender.extender_data por (parent,sibling) in  
            let uc_sibling = String.uncapitalize_ascii sibling in  
            String.concat "\n"
            ([
                  "let sp_for_"^uc_sibling^" child new_parent = ";
                  " Extender."^ext_name^" new_parent ";
            ]@(   
               Image.image (fun (field_name,indexed_varname)->
                 (String.make 3 ' ')^"~"^field_name^":(child."^main_module_name^"_t."^field_name^")" ) indexed_and_labeled    
            )@[" ;;"
            ]) ;; 
      
      let text_for_parent_setters por = 
         String.concat "\n"   
        (Image.image (text_for_parent_setter por) por.Polymorphic_ocaml_record_t.designated_parents);;
      
      let text_for_main_parent_setter por = 
            let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in   
            String.concat "\n"
            ([
                  "let set ~child ~new_parent = ";
                  " let name = child."^main_module_name^"_t.type_name in ";
                  " match List.assoc_opt name ["
            ]@(   
               Image.image (fun (sibling,parent)->
                  let c_sibling = String.capitalize_ascii sibling 
                  and uc_sibling = String.uncapitalize_ascii sibling in 
                 (String.make 3 ' ')^(Strung.enclose c_sibling)^" , sp_for_"^uc_sibling^" child new_parent ;" ) 
                 por.Polymorphic_ocaml_record_t.designated_parents
            )@[
               " ] with "; 
               "  Some(answer) ->answer";
               " |None -> raise (Set_parent_exn(name)) ;;"
      
            ]) ;;   

      let text_for_main_parent_getter por = 
            let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in   
            String.concat "\n"
            ([
                  "let get child = ";
                  " let parent_name = get_parent_name child in ";
                  " { child with "^main_module_name^"_t.type_name = parent_name } ;;"
            ]) ;;         

      let full_text por =
            if por.Polymorphic_ocaml_record_t.designated_parents = []
            then ""  
            else
            "module Parent = struct \n"^
            (text_for_main_list por)^"\n\n"^
            (text_for_exceptions)^"\n\n"^
            (text_for_get_parent_name por)^"\n\n"^
            (text_for_parent_setters por)^"\n\n"^
            (text_for_main_parent_setter por)^"\n\n"^
            (text_for_main_parent_getter por)^"\n\n"^
            "end;; \n\n\n"     ;; 
      
      end ;;      
      
      module Origin = struct 
            
      let snippet
        (por:Polymorphic_ocaml_record_t.t) 
          (field:Polymorphic_ocaml_record_t.field_t) = 
             (String.make 3 ' ')^(field.Polymorphic_ocaml_record_t.field_name)^" = "^
             (field.Polymorphic_ocaml_record_t.default_value)^" ;" ;;
          
      let  text por =
            let temp1 = (String.make 3 ' ')^(String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^
                        "_t.type_name = \"\" ;" 
            and temp2 = Image.image (snippet por) por.Polymorphic_ocaml_record_t.fields in 
             (String.concat "\n" 
             (["let origin = {";]@
               ( temp1 :: temp2 )
               @["} ;;"])
             );;        
      
      end ;;      
      
      module Type_information = struct

      let element_in_fields_for_instances (inst_name,inst_fields)=
         let temp1 = Image.image Strung.enclose inst_fields in 
         (Strung.enclose inst_name)^" , ["^(String.concat ";" temp1)^"]" ;;

      let  text_for_fields_for_instances por =
            let temp1 = Image.image (fun 
              inst -> (inst.Polymorphic_ocaml_record_t.instance_name,
                  inst.Polymorphic_ocaml_record_t.fields)
            ) por.Polymorphic_ocaml_record_t.instances in 
            
             (
             "let fields_for_instances = [\n"^
               ( String.concat ";\n" (Image.image element_in_fields_for_instances temp1 ))
               ^"\n] ;;"
             );;            

      let full_text por =
          (text_for_fields_for_instances por )   

      end ;;      

      end ;;      
      
      
         
      let main por =   
            "module Private = struct \n\n"^
                 (Private.Crobj.full_text por)^"\n\n"^
                 (Private.Extender.full_text por)^"\n\n"^
                 (Private.Parent.full_text por)^"\n\n"^
                 (Private.Origin.text por)^"\n\n"^
                 (Private.Type_information.full_text por)^"\n\n"^
                 "\nend;; \n\n\n" ;;