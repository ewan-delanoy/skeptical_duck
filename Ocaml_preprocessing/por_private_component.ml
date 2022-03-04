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

   
let indexed_varname_for_field (j,fd)=
      "v"^(string_of_int j)^"_"^(fd.Polymorphic_ocaml_record_t.var_name) ;;
 
let snippet_for_extender_element (j,fd) = 
      let var_name  = indexed_varname_for_field (j,fd) in 
      (String.make 3 ' ')^(fd.Polymorphic_ocaml_record_t.field_name)^" = "^
      var_name^" ;" ;;
 
 
let text_for_extender por (before_ext,after_ext) =
    let ext_name = "extend_"^before_ext^"_to_"^after_ext in 
    let inst_before = Por_common.get_instance por before_ext 
    and inst_after = Por_common.get_instance por after_ext  in 
    let field_names_before = inst_before.Polymorphic_ocaml_record_t.fields 
    and field_names_after = inst_after.Polymorphic_ocaml_record_t.fields in 
    let _ = Por_common.check_inclusion field_names_before field_names_after in 
    let extra_field_names = List.filter (fun fdn->not(List.mem fdn field_names_before)) field_names_after in 
    let extra_fields = Image.image (Por_common.get_field por) extra_field_names in 
    let indexed_extra_fields = Ennig.index_everything extra_fields in 
    let filling_fields = Image.image (snippet_for_extender_element) indexed_extra_fields in 
    let indexed_and_labeled = Image.image (fun (j,fd)->
       "~"^(fd.Polymorphic_ocaml_record_t.field_name)^":"^(indexed_varname_for_field (j,fd))) indexed_extra_fields in 
    let vars = String.concat " " indexed_and_labeled in 
    let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in 
    String.concat "\n" (["let "^ext_name^" fw "^vars^" = {";
      (String.make 3 ' ')^"fw with ";
      (String.make 3 ' ')^main_module_name^"_t.type_name = \""^(String.capitalize_ascii after_ext)^"\" ;"]@
        filling_fields
      @["} ;;"]);;  
 
 let full_text por = 
    String.concat "\n" (Image.image (text_for_extender por) por.Polymorphic_ocaml_record_t.extensions)
 ;;      

 let needed_extensions por =
      por.Polymorphic_ocaml_record_t.extensions @
      (Image.image (fun (x,y)->(y,x)) por.Polymorphic_ocaml_record_t.designated_parents) ;;   
      
 let full_text por =
      let l = needed_extensions por in 
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
      let temp1 = Image.image (fun (s,t)->
            (String.make 4 ' ')^(Strung.enclose s)^" , "^(Strung.enclose t)^" ;"
      ) l in 
      String.concat "\n"
      ("let designated_parents = ["::
      (temp1@
      ["] ;;"]));;
        
let text_for_exceptions =
      "exception No_designated_parent of string ;; " ;; 
        
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
        
let full_text por =
      if por.Polymorphic_ocaml_record_t.designated_parents = []
      then ""  
      else
      "module Parent = struct \n"^
      (text_for_main_list por)^"\n\n"^
      (text_for_exceptions)^"\n\n"^
      (text_for_get_parent_name por)^"\n\n"^
      "\nend;; \n\n\n"     ;; 

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

end ;;      


   
let main por =   
      "module Private = struct \n\n"^
           (Private.Crobj.full_text por)^"\n\n"^
           (Private.Extender.full_text por)^"\n\n"^
           (Private.Parent.full_text por)^"\n\n"^
           (Private.Origin.text por)^"\n\n"^
           "\nend;; \n\n\n" ;;