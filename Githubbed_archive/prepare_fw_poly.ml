(*

#use "Githubbed_archive/prepare_fw_poly.ml";;

*)

open Needed_values ;;


   
   module Annotated_definition_t = struct 

  type t = {
      value_name : string ;
      is_private : bool ;
      lines_in_definition : string list ;
   } ;;

   end ;; 

   module Annotated_definition = struct 

    let expand anndef =
       String.concat "\n" anndef.Annotated_definition_t.lines_in_definition ;;

  let order = ((fun anndef1 anndef2 ->
    let is_private1 = anndef1.Annotated_definition_t.is_private 
    and is_private2 = anndef1.Annotated_definition_t.is_private in 
    let trial1 = Total_ordering.standard is_private2 is_private1 in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else 
    let val_name1 = anndef1.Annotated_definition_t.value_name 
    and val_name2 = anndef2.Annotated_definition_t.value_name in 
    let trial2 = Total_ordering.lex_for_strings val_name1 val_name2 in 
    if trial2 <> Total_ordering_result_t.Equal then trial2 else    
     Total_ordering.standard anndef1 anndef2 
  ) :>  Annotated_definition_t.t Total_ordering_t.t ) ;;   
      


   end ;; 

   
   let pair_for_field (porf:Polymorphic_ocaml_record_t.field_t) =
     (String.make 3 ' ')^
     porf.Polymorphic_ocaml_record_t.field_name^" : "^
     porf.Polymorphic_ocaml_record_t.field_type^" ;" ;;
   
   let initial_comment_in_type_signature_file por =
       let ap = por.Polymorphic_ocaml_record_t.type_signature_file 
       and root = Coma_big_constant.This_World.root in 
       let s_ap=Absolute_path.to_string ap in 
       let s_cdir=Dfa_root.connectable_to_subpath root in 
       let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
       "(*\n\n#use\""^shortened_path^"\";"^";\n\n*)\n\n\n" ;;
   
   let text_for_type_signature_file (por:Polymorphic_ocaml_record_t.t) = 
     let pairs = 
       ((String.make 3 ' ')^"subtype_name : string ;")::
       (Image.image pair_for_field por.Polymorphic_ocaml_record_t.fields) in 
     (initial_comment_in_type_signature_file por)^
     "type "^(por.Polymorphic_ocaml_record_t.main_type_name)^" = { \n"^ 
     (String.concat "\n" pairs) ^ 
     "\n} ;;" ;;
   
   let write_to_type_signature_file (por:Polymorphic_ocaml_record_t.t) = 
       let text = text_for_type_signature_file por 
       and file = por.Polymorphic_ocaml_record_t.type_signature_file in 
       Io.overwrite_with file text ;;
   
   
   let annotated_text_for_field_getter 
     (por:Polymorphic_ocaml_record_t.t) 
       (field:Polymorphic_ocaml_record_t.field_t) =
       let fn = field.Polymorphic_ocaml_record_t.field_name in 
       {
        Annotated_definition_t.value_name = fn ;
        is_private = false ;
        lines_in_definition = ["let "^fn^" x = x."^
        (String.capitalize_ascii(por.Polymorphic_ocaml_record_t.module_name))^
        "_t."^fn^" ;;"];
     } ;;
          
   let annotated_text_for_field_setter 
     (por:Polymorphic_ocaml_record_t.t) 
       (field:Polymorphic_ocaml_record_t.field_t) =
       let fn = field.Polymorphic_ocaml_record_t.field_name 
       and vn = field.Polymorphic_ocaml_record_t.var_name in 
       {
        Annotated_definition_t.value_name = "set_"^fn ;
        is_private = false ;
        lines_in_definition = ["let set_"^fn^" x "^vn^" = { x with "^
        (String.capitalize_ascii(por.Polymorphic_ocaml_record_t.module_name))^
        "_t."^fn^" = "^vn^"} ;;"];
     } ;;
     

   
   let snippet_for_origin_element 
     (por:Polymorphic_ocaml_record_t.t) 
      (field:Polymorphic_ocaml_record_t.field_t) = 
      (String.make 3 ' ')^(field.Polymorphic_ocaml_record_t.field_name)^" = "^
      (field.Polymorphic_ocaml_record_t.default_value)^" ;" ;;
   
   let  annotated_text_for_origin_element por =
     let temp1 = (String.make 3 ' ')^(String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^
                 "_t.type_name = \"\" ;" 
     and temp2 = Image.image (snippet_for_origin_element por) por.Polymorphic_ocaml_record_t.fields in 
     {
        Annotated_definition_t.value_name = "origin" ;
        is_private = true ;
        lines_in_definition = ["let origin = {";]@
        ( temp1 :: temp2 )
        @["} ;;"];
      } ;;  

   let initial_comment_in_implementation_file por =
      let ap = por.Polymorphic_ocaml_record_t.implementation_file 
      and root = Coma_big_constant.This_World.root in 
      let s_ap=Absolute_path.to_string ap in 
      let s_cdir=Dfa_root.connectable_to_subpath root in 
      let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
      "(*\n\n#use\""^shortened_path^"\";"^";\n\n*)\n\n\n" ;;  
   
   let annotated_text_for_getters por = Image.image (annotated_text_for_field_getter por)
     por.Polymorphic_ocaml_record_t.fields ;;
   let annotated_text_for_setters por = Image.image (annotated_text_for_field_setter por)
     por.Polymorphic_ocaml_record_t.fields ;;
   let annotated_text_for_crobj_symlinks  = 
    [
      {
        Annotated_definition_t.value_name = "of_concrete_object" ;
        is_private = false ;
        lines_in_definition = ["let of_concrete_object = Private.Crobj.of_concrete_object ;;"];
      } ;
      {
        Annotated_definition_t.value_name = "to_concrete_object" ;
        is_private = false ;
        lines_in_definition = ["let to_concrete_object = Private.Crobj.to_concrete_object ;;"];
      } ;
    ] ;;
    


  let simple_text_for_label 
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

  let simple_text_for_all_labels por =
    let crobjed_fields = special_type_name_field ::(Image.image fst (fields_with_crobj_conversion por)) in 
    let max_namesize = snd (Max.maximize_it (fun fd->
      String.length(fd.Polymorphic_ocaml_record_t.field_name)) crobjed_fields) in 
    String.concat "\n"
      ("let salt = \"Fw_poly_t.\" ;;" ::
    (Image.image (simple_text_for_label por max_namesize) crobjed_fields)) ;;  
   
  let simple_text_for_ofcrobj_element 
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

  let simple_text_for_ofcrobj_converter por = 
    let all_fields = por.Polymorphic_ocaml_record_t.fields in 
    let temp1 = (String.make 3 ' ')^(String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^
    "_t.type_name = Crobj_converter.string_of_concrete_object (g label_for_type_name) ;" 
    and temp2 = Image.image (simple_text_for_ofcrobj_element por) all_fields in 
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

    let simple_text_for_tocrobj_element 
      (por:Polymorphic_ocaml_record_t.t) 
      (fld,(of_crobj,to_crobj)) = 
      let field_name  = fld.Polymorphic_ocaml_record_t.field_name in 
          (String.make 4 ' ')^" label_for_"^field_name^", "^ 
          to_crobj^" fw."^(String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^"_t."^
          field_name^" ;" ;;    

  let simple_text_for_tocrobj_converter por = 
    let fields_with_crobj = fields_with_crobj_conversion por  in 
    let temp1 = (String.make 4 ' ')^" label_for_type_name,"^ 
    " Crobj_converter.string_to_concrete_object fw."^
     (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^"_t.type_name ;"
    and temp2 = Image.image (simple_text_for_tocrobj_element por) fields_with_crobj in 
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

  let simple_text_for_crobj_related_code por =
    if not(por.Polymorphic_ocaml_record_t.has_crobj_conversion)
    then ""  
    else
    "module Crobj = struct \n"^
    (simple_text_for_all_labels por)^"\n\n"^
    (simple_text_for_ofcrobj_converter por)^"\n\n"^
    (simple_text_for_tocrobj_converter por)^"\n\n"^
    "\nend;; \n\n\n"

    exception Get_field_exn of string ;;


    let get_field por fd_name =
       match Option.seek (fun fd->fd.Polymorphic_ocaml_record_t.field_name = fd_name)
          por.Polymorphic_ocaml_record_t.fields with 
       Some answer -> answer 
       | None -> raise ( Get_field_exn(fd_name)) ;;    
    
     exception Get_instance_exn of string ;; 
    
     let get_instance por inst_name =
         match Option.seek (fun fd->fd.Polymorphic_ocaml_record_t.instance_name = inst_name)
            por.Polymorphic_ocaml_record_t.instances with 
         Some answer -> answer 
         | None -> raise ( Get_instance_exn(inst_name)) ;;    
    
     exception Check_inclusion_exn of (string list) * (string list) * (string list) ;;
    
     let check_inclusion small_list large_list =
        let temp1 = List.filter (fun s->not(List.mem s large_list)) small_list in 
        if temp1 <> []
        then raise(Check_inclusion_exn(temp1,small_list,large_list))
        else () ;;  
    
     let indexed_varname_for_field (j,fd)=
         "v"^(string_of_int j)^"_"^(fd.Polymorphic_ocaml_record_t.var_name) ;;
    
     let snippet_for_extender_element (j,fd) = 
         let var_name  = indexed_varname_for_field (j,fd) in 
         (String.make 3 ' ')^(fd.Polymorphic_ocaml_record_t.field_name)^" = "^
         var_name^" ;" ;;
    
    
    let annotated_definition_for_extender por (before_ext,after_ext) =
       let ext_name = "extend_"^before_ext^"_to_"^after_ext in 
       let inst_before = get_instance por before_ext 
       and inst_after = get_instance por after_ext  in 
       let field_names_before = inst_before.Polymorphic_ocaml_record_t.fields 
       and field_names_after = inst_after.Polymorphic_ocaml_record_t.fields in 
       let _ = check_inclusion field_names_before field_names_after in 
       let extra_field_names = List.filter (fun fdn->not(List.mem fdn field_names_before)) field_names_after in 
       let extra_fields = Image.image (get_field por) extra_field_names in 
       let indexed_extra_fields = Ennig.index_everything extra_fields in 
       let filling_fields = Image.image (snippet_for_extender_element) indexed_extra_fields in 
       let indexed_and_labeled = Image.image (fun (j,fd)->
          "~"^(fd.Polymorphic_ocaml_record_t.field_name)^":"^(indexed_varname_for_field (j,fd))) indexed_extra_fields in 
       let vars = String.concat " " indexed_and_labeled in 
       let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in 
       {
         Annotated_definition_t.value_name = ext_name ;
         is_private = false ;
         lines_in_definition = ["let "^ext_name^" fw "^vars^" = {";
         (String.make 3 ' ')^"fw with ";
         (String.make 3 ' ')^main_module_name^"_t.type_name = \""^(String.capitalize_ascii after_ext)^"\" ;"]@
           filling_fields
         @["} ;;"];
       } ;;  
    
    let annotated_text_for_extenders por = 
       Image.image (annotated_definition_for_extender por) por.Polymorphic_ocaml_record_t.extensions
    ;;      
   
    let annotated_definition_for_constructor por constructed_instance =
      let constructor_name = "construct_"^(String.uncapitalize_ascii constructed_instance) in 
      let full_instance = get_instance por constructed_instance  in 
      let field_names = full_instance.Polymorphic_ocaml_record_t.fields in 
      let fields = Image.image (get_field por)field_names in 
      let indexed_fields = Ennig.index_everything fields in 
      let filling_fields = Image.image (snippet_for_extender_element) indexed_fields in 
      let indexed_and_labeled = Image.image (fun (j,fd)->
         "~"^(fd.Polymorphic_ocaml_record_t.field_name)^":"^(indexed_varname_for_field (j,fd))) indexed_fields in 
      let vars = String.concat " " indexed_and_labeled in 
      let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in  
      {
        Annotated_definition_t.value_name = constructor_name ;
        is_private = false ;
        lines_in_definition = ["let "^constructor_name^" "^vars^" = {";
        (String.make 3 ' ')^"Private.origin with ";
        (String.make 3 ' ')^main_module_name^"_t.type_name = \""^(String.capitalize_ascii constructed_instance)^"\" ;"]@
          filling_fields
        @["} ;;"];
      } ;;  

    let annotated_text_for_constructors por = 
      Image.image (annotated_definition_for_constructor por) por.Polymorphic_ocaml_record_t.constructors
   ;;     
   
   let annotated_definition_for_restrictor por (before_restr,after_restr) =
    let restr_name = "restrict_"^before_restr^"_to_"^after_restr in 
    let inst_before = get_instance por before_restr 
    and inst_after = get_instance por after_restr  in 
    let field_names_before = inst_before.Polymorphic_ocaml_record_t.fields 
    and field_names_after = inst_after.Polymorphic_ocaml_record_t.fields in 
    let _ = check_inclusion field_names_after field_names_before in 
    let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in  
    {
      Annotated_definition_t.value_name = restr_name ;
      is_private = false ;
      lines_in_definition = ["let "^restr_name^" fw  = {";
      (String.make 3 ' ')^"fw with ";
      (String.make 3 ' ')^main_module_name^"_t.type_name = \""^(String.capitalize_ascii after_restr)^"\" ;"]
      @["} ;;"];
    } ;;  


   let annotated_text_for_restrictors por = 
    Image.image (annotated_definition_for_restrictor por) por.Polymorphic_ocaml_record_t.restrictions
 ;;     


 let annotated_definition_for_print_out por =
  let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in 
  {
    Annotated_definition_t.value_name = "print_out" ;
    is_private = false ;
    lines_in_definition = ["let print_out (fmt:Format.formatter) fw  = "^
    "Format.fprintf fmt \"@[%s@]\" (\"< \"^(fw."^main_module_name^"_t.type_name)^\" >\") ;;";];
  } ;;  

  let simple_text_for_parenting_list por =
    let l = por.Polymorphic_ocaml_record_t.designated_parents in 
    let temp1 = Image.image (fun (s,t)->
      (String.make 4 ' ')^(Strung.enclose s)^" , "^(Strung.enclose t)^" ;"
    ) l in 
    String.concat "\n"
    ("let designated_parents = ["::
     (temp1@
     ["] ;;"]));;

  let simple_text_for_parenting_exceptions =
     "exception No_designated_parent of string ;; " ;; 

  let simple_text_for_get_parent_name por = 
    let main_module_name = (String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name) in 
      String.concat "\n"
      ([
        "let get_parent_name fw = ";
        " let name = fw."^main_module_name^"_t.type_name in ";
        " match List.assoc_opt name designated_parents with ";
        "  Some(answer) ->answer";
        " |None -> raise (No_designated_parent(name)) ;;"
      ]);;

  let simple_text_for_parent_related_code por =
    if por.Polymorphic_ocaml_record_t.designated_parents = []
    then ""  
    else
    "module Parent = struct \n"^
    (simple_text_for_parenting_list por)^"\n\n"^
    (simple_text_for_parenting_exceptions)^"\n\n"^
    (simple_text_for_get_parent_name por)^"\n\n"^
    "\nend;; \n\n\n"


   let expand_privatized_text l =
      let temp1 = Ordered.sort Annotated_definition.order l in 
      String.concat "\n" (Image.image Annotated_definition.expand temp1) ;;
      
   let expand_annotated_text por l =   
      let (private_component,public_component) =
         List.partition (
           fun anndef -> anndef.Annotated_definition_t.is_private 
      ) l in 
      let private_text = (
         if private_component = []
         then ""
         else   
         "module Private = struct \n"^
         (simple_text_for_crobj_related_code por)^
         (simple_text_for_parent_related_code por)^
         (expand_privatized_text private_component)^
         "\nend;; \n\n\n"
      ) in 
      private_text^  
      (expand_privatized_text public_component) ;;

   let full_annotated_text por = 
      expand_annotated_text por (
         annotated_text_for_origin_element por :: 
         ((annotated_text_for_getters por)@
         (annotated_text_for_setters por)@
         (annotated_text_for_crobj_symlinks)@
         (annotated_text_for_extenders por)@
         (annotated_text_for_constructors por)@
         (annotated_text_for_restrictors por)@
         [annotated_definition_for_print_out por] )
      ) ;;

   let text_for_implementation_file (por:Polymorphic_ocaml_record_t.t) = 
      (initial_comment_in_implementation_file por)^
      (full_annotated_text por) ;;
    
   let write_to_implementation_file (por:Polymorphic_ocaml_record_t.t) = 
      let text = text_for_implementation_file por 
      and file = por.Polymorphic_ocaml_record_t.implementation_file in 
      Io.overwrite_with file text ;;

  let decode_pair_of_converters s =
        match String.index_opt s '#' with 
         None -> None 
       |Some idx -> Some(Cull_string.beginning idx s,Cull_string.cobeginning (idx+1) s) ;;
     
   

   let field_list_constructor l = Image.image (
      fun (a,b,c,d,e) -> {
       Polymorphic_ocaml_record_t.field_name = a ;
       field_type = b ;
       var_name =c ;
       default_value = d ;
       crobj_converters = decode_pair_of_converters e ;
    }
   ) l;;

  
   let fields_for_fw_configuration = field_list_constructor [
     "root","Dfa_root_t.t","r","Dfa_root.of_line \"dummy\"","Dfa_root.of_concrete_object#Dfa_root.to_concrete_object";
     "ignored_subdirectories","Dfa_subdirectory_t.t list","ign_subdirs","[]","Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object#Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object";
     "ignored_files","Dfn_rootless_t.t list","ign_files","[]","Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object#Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object"     ;
   ] ;; 
   
   let fields_for_file_watcher = field_list_constructor [
     "watched_files", "(Dfn_rootless_t.t * string) list","files","[]","Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Crobj_converter.string_of_concrete_object#Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Crobj_converter.string_to_concrete_object";
   ] ;; 
   
   let fields_for_fw_with_archives = field_list_constructor [
     "subdirs_for_archived_mlx_files","Dfa_subdirectory_t.t list","archives_subdirs","[]","Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object#Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object";
   ] ;; 
   
   let fields_for_fw_with_small_details = field_list_constructor [
     "small_details_in_files","(Dfn_rootless_t.t * Fw_file_small_details_t.t) list","small_details","[]","Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Fw_file_small_details.of_concrete_object#Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Fw_file_small_details.to_concrete_object";
   ] ;; 
   
   let fields_for_fw_with_dependencies = field_list_constructor [
     "index_for_caching", "Fw_instance_index_t.t * Fw_state_index_t.t", "cache_idx", "(Fw_instance_index_t.I(0),Fw_state_index_t.I(0))", "";
   ] ;; 
   
   let fields_for_fw_with_batch_compilation = field_list_constructor [
     "last_compilation_result_for_module","(Dfa_module_t.t * bool) list","compilation_results","[]","Crobj_converter_combinator.to_pair_list Dfa_module.of_concrete_object Crobj_converter.bool_of_concrete_object#Crobj_converter_combinator.of_pair_list Dfa_module.to_concrete_object Crobj_converter.bool_to_concrete_object" ;
   ] ;; 
   
   let fields_for_fw_with_githubbing = field_list_constructor [
     "dir_for_backup","Dfa_root_t.t","backup_dir","Dfa_root.of_line \"dummy\"","Dfa_root.of_concrete_object#Dfa_root.to_concrete_object";
     "gitpush_after_backup","bool","gab","false","Crobj_converter.bool_of_concrete_object#Crobj_converter.bool_to_concrete_object";
     "github_url","string","url","\"\"","Crobj_converter.string_of_concrete_object#Crobj_converter.string_to_concrete_object";
     "encoding_protected_files","(Dfn_rootless_t.t * Dfn_rootless_t.t) list","protected_pairs","[]","Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object#Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object";
   ] ;; 

   let instance_list_constructor l = Image.image (
      fun (a,b) -> {
       Polymorphic_ocaml_record_t.instance_name = a ;
       fields = b ;
    }
   ) l;;
  
  let first_base =  [
    "fw_configuration",fields_for_fw_configuration ;
    "file_watcher",fields_for_file_watcher ;
    "fw_with_archives",fields_for_fw_with_archives ;
    "fw_with_small_details",fields_for_fw_with_small_details ;
    "fw_with_dependencies",fields_for_fw_with_dependencies ;
    "fw_with_batch_compilation",fields_for_fw_with_batch_compilation ;
    "fw_with_githubbing",fields_for_fw_with_githubbing ;
  ] ;; 

  

   let all_fields = List.flatten (Image.image snd first_base) ;;

   let cumulative_first_base =
    let temp1 = Three_parts.generic first_base in 
    List.rev_map (fun (b,a,_)->
      let ttemp3 = List.rev_map snd (a::b) in
      (fst a,List.flatten ttemp3) 
      ) temp1 ;;

  let root_field = Listennou.force_find (fun fd->
      fd.Polymorphic_ocaml_record_t.field_name = "root"
    ) fields_for_fw_configuration ;;

   let second_base = [
      "github_configuration",root_field :: fields_for_fw_with_githubbing ;
   ] ;; 

   let full_base =  cumulative_first_base @ second_base ;;     
   
   let instance_list_constructor l = Image.image (
      fun (a,b) -> {
        Polymorphic_ocaml_record_t.instance_name = a ;
        fields = Image.image (fun fd->fd.Polymorphic_ocaml_record_t.field_name ) b ;
      }
   ) l;;

   let field_order = ((fun fld1 fld2 ->
      let trial1 = Total_ordering.lex_for_strings 
         fld1.Polymorphic_ocaml_record_t.field_name fld2.Polymorphic_ocaml_record_t.field_name in 
      if trial1<> Total_ordering_result_t.Equal then trial1 else
         Total_ordering.standard fld1 fld2         
   ) : Polymorphic_ocaml_record_t.field_t Total_ordering_t.t);;
   
   
      
    
 

   let example = 
      let home = Sys.getenv "HOME" in 
      let file_there = (fun s-> 
        Absolute_path.create_file_if_absent(home^"/Teuliou/OCaml/Ordinary/Filewatching/"^s^".ml")) in 
     {
       Polymorphic_ocaml_record_t.main_type_name = "t" ;
       module_name = "fw_poly" ;
       fields = Ordered.sort field_order all_fields ;
       instances = instance_list_constructor full_base ;
       type_signature_file = (file_there "fw_poly_t") ;
       implementation_file = (file_there "fw_poly") ;
       has_crobj_conversion = true ;
       extensions = ["fw_configuration","file_watcher";
                     "fw_with_batch_compilation","fw_with_githubbing"] ;
       restrictions = ["fw_with_githubbing","github_configuration"] ;
       constructors = ["fw_configuration";"github_configuration"] ;
       designated_parents = ["fw_with_archives","file_watcher"] ;
    } ;;
   
    let act () = write_to_implementation_file example ;;
   (*  
   let act () = write_to_type_signature_file example ;;
   *)
