(*

#use "Githubbed_archive/prepare_fw_poly.ml";;

*)

open Needed_values ;;


module Polymorphic_ocaml_record_t = struct 

   type field_t = {
      field_name : string ;
      field_type : string ;
      var_name : string ;
      default_value : string ;
   } ;;
   
   type instance_t = {
      instance_name : string ;
      fields : string list ;
   } ;;
   
   type t = {
      main_type_name : string ;
      module_name : string ;
      fields : field_t list ;
      instances : instance_t list ;
      type_signature_file : Absolute_path.t ;
      implementation_file : Absolute_path.t ;
      has_crobj_conversion : bool ;

   } ;;
   
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
     (fn,(false,["let "^fn^" x = x."^
     (String.capitalize_ascii(por.Polymorphic_ocaml_record_t.module_name))^
     "_t."^fn^" ;;"])) ;;
          
   let annotated_text_for_field_setter 
     (por:Polymorphic_ocaml_record_t.t) 
       (field:Polymorphic_ocaml_record_t.field_t) =
       let fn = field.Polymorphic_ocaml_record_t.field_name 
       and vn = field.Polymorphic_ocaml_record_t.var_name in 
     ("set_"^fn,(false,["let set_"^fn^" x "^vn^" = { x with "^
     (String.capitalize_ascii(por.Polymorphic_ocaml_record_t.module_name))^
     "_t."^fn^" = "^vn^"} ;;"])) ;;  

   
   let annotated_text_for_origin_element 
     (por:Polymorphic_ocaml_record_t.t) 
      (field:Polymorphic_ocaml_record_t.field_t) = 
      (String.make 3 ' ')^(field.Polymorphic_ocaml_record_t.field_name)^" = "^
      (field.Polymorphic_ocaml_record_t.default_value)^" ;" ;;
   
   let  annotated_text_for_origin_element por =
     let temp1 = (String.make 3 ' ')^(String.capitalize_ascii por.Polymorphic_ocaml_record_t.module_name)^
                 "_t.type_name = \"\" ;" 
     and temp2 = Image.image (annotated_text_for_origin_element por) por.Polymorphic_ocaml_record_t.fields in 
     ("origin",(true,["let origin = {";]@
     ( temp1 :: temp2 )
     @["} ;;"])) ;;  

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

  let simple_text_for_label 
     (por:Polymorphic_ocaml_record_t.t) 
       max_namesize
      (field:Polymorphic_ocaml_record_t.field_t) = 
      let fn = field.Polymorphic_ocaml_record_t.field_name in 
      let offset = String.make (max_namesize-String.length fn) ' ' in 
      "let label_for_"^fn^offset^" = salt ^ \""^fn^"\" ;;" ;;
   
  let simple_text_for_all_labels por =
    let all_fields = por.Polymorphic_ocaml_record_t.fields in 
    let max_namesize = snd (Max.maximize_it (fun fd->
      String.length(fd.Polymorphic_ocaml_record_t.field_name)) all_fields) in 
    String.concat "\n"
      ("let salt = \"Fw_poly_t.\" ;;" ::
    (Image.image (simple_text_for_label por max_namesize) all_fields)) ;;  
   
  let simple_text_for_crobj_related_code por =
    if not(por.Polymorphic_ocaml_record_t.has_crobj_conversion)
    then ""  
    else
    "module Crobj = struct \n"^
    (simple_text_for_all_labels por)^
    "\nend;; \n\n\n"

   let expand_text_element
     (val_name,(is_private,lines)) =
        String.concat "\n" lines ;;

   let order_for_annotated_elements = ((fun elt1 elt2 ->
     let  (val_name1,(is_private1,lines1)) = elt1 
     and  (val_name2,(is_private2,lines2)) = elt2 in 
     let trial1 = Total_ordering.standard is_private2 is_private1 in 
     if trial1 <> Total_ordering_result_t.Equal then trial1 else 
     let trial2 = Total_ordering.lex_for_strings val_name1 val_name2 in 
     if trial2 <> Total_ordering_result_t.Equal then trial2 else    
      Total_ordering.standard elt1 elt2 
   ) :> (string * (bool * (string list))) Total_ordering_t.t ) ;;   
     

   let expand_privatized_text l =
      let temp1 = Ordered.sort order_for_annotated_elements l in 
      String.concat "\n" (Image.image expand_text_element temp1) ;;
      
   let expand_annotated_text por l =   
      let (private_component,public_component) =
         List.partition (
           fun (_,(is_private,_)) -> is_private 
      ) l in 
      let private_text = (
         if private_component = []
         then ""
         else   
         "module Private = struct \n"^
         (simple_text_for_crobj_related_code por)^
         (expand_privatized_text private_component)^
         "\nend;; \n\n\n"
      ) in 
      private_text^  
      (expand_privatized_text public_component) ;;

   let full_annotated_text por = 
      expand_annotated_text por (
         annotated_text_for_origin_element por :: 
         ((annotated_text_for_getters por)@
         (annotated_text_for_setters por))
      ) ;;

   let text_for_implementation_file (por:Polymorphic_ocaml_record_t.t) = 
      (initial_comment_in_implementation_file por)^
      (full_annotated_text por) ;;
    
   let write_to_implementation_file (por:Polymorphic_ocaml_record_t.t) = 
      let text = text_for_implementation_file por 
      and file = por.Polymorphic_ocaml_record_t.implementation_file in 
      Io.overwrite_with file text ;;

   let field_list_constructor l = Image.image (
      fun (a,b,c,d) -> {
       Polymorphic_ocaml_record_t.field_name = a ;
       field_type = b ;
       var_name =c ;
       default_value = d ;
    }
   ) l;;
   
   let fields_for_fw_configuration = field_list_constructor [
     "root","Dfa_root_t.t","r","Dfa_root.of_line \"dummy\"";
     "ignored_subdirectories","Dfa_subdirectory_t.t list","ign_subdirs","[]";
     "ignored_files","Dfn_rootless_t.t list","ign_files","[]";
   ] ;; 
   
   let fields_for_file_watcher = field_list_constructor [
     "watched_files", "(Dfn_rootless_t.t * string) list","files","[]";
   ] ;; 
   
   let fields_for_fw_with_archives = field_list_constructor [
     "subdirs_for_archived_mlx_files","Dfa_subdirectory_t.t list","archives_subdirs","[]";
   ] ;; 
   
   let fields_for_fw_with_small_details = field_list_constructor [
     "small_details_in_files","(Dfn_rootless_t.t * Fw_file_small_details_t.t) list","small_details","[]";
   ] ;; 
   
   let fields_for_fw_with_dependencies = field_list_constructor [
     "index_for_caching", "Fw_instance_index_t.t * Fw_state_index_t.t", "cache_idx", "(Fw_instance_index_t.I(0),Fw_state_index_t.I(0))";
   ] ;; 
   
   let fields_for_fw_with_batch_compilation = field_list_constructor [
     "last_compilation_result_for_module","(Dfa_module_t.t * bool) list","compilation_results","[]";
   ] ;; 
   
   let fields_for_fw_with_githubbing = field_list_constructor [
     "dir_for_backup","Dfa_root_t.t","backup_dir","Dfa_root.of_line \"dummy\"";
     "gitpush_after_backup","bool","gab","false";
     "github_url","string","url","\"\"";
     "encoding_protected_files","(Dfn_rootless_t.t * Dfn_rootless_t.t) list","protected_pairs","[]";
   ] ;; 
   
   let all_fields = List.flatten [
     fields_for_fw_configuration ;
     fields_for_file_watcher ;
     fields_for_fw_with_archives ;
     fields_for_fw_with_small_details ;
     fields_for_fw_with_dependencies ;
     fields_for_fw_with_batch_compilation ;
     fields_for_fw_with_githubbing ;
   ] ;;
   
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
       instances = [] ;
       type_signature_file = (file_there "fw_poly_t") ;
       implementation_file = (file_there "fw_poly") ;
       has_crobj_conversion = true ;
    } ;;
   
    let act () = write_to_implementation_file example ;;
   (*  
   let act () = write_to_type_signature_file example ;;
   *)
