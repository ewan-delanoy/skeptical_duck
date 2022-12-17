(*

#use"lib/Filewatching/gw_guthib_configuration.ml";;

*)


module Background = struct 

  let fw_space = Por_space_example.filewatching ;; 
  
  let root_field = Gw_configuration.Background.root_field ;; 

  let dir_for_backup_field =
    {Por_field_t.field_name = "dir_for_backup";
    field_type = "Dfa_root_t.t"; var_name = "backup_dir";
    default_value = "Dfa_root.of_line \"dummy\"";
    crobj_converters =
     Some ("Dfa_root.of_concrete_object", "Dfa_root.to_concrete_object");
     dependency_data = None;}
  ;;
  
  let gitpush_after_backup_field =
    {Por_field_t.field_name = "gitpush_after_backup";
    field_type = "bool"; var_name = "gab"; default_value = "false";
    crobj_converters =
     Some
      ("Crobj_converter.bool_of_concrete_object",
       "Crobj_converter.bool_to_concrete_object");
    dependency_data = None;}
  ;;
  
  let github_url_field =
    {Por_field_t.field_name = "github_url";
    field_type = "string"; var_name = "url"; default_value = "\"\"";
    crobj_converters =
     Some
      ("Crobj_converter.string_of_concrete_object",
       "Crobj_converter.string_to_concrete_object");
    dependency_data = None;}
  ;;
  
  
  let encoding_protected_files_field =
    {Por_field_t.field_name = "encoding_protected_files";
    field_type = "(Dfn_rootless_t.t * Dfn_rootless_t.t) list";
    var_name = "protected_pairs"; default_value = "[]";
    crobj_converters =
     Some
      ("Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object",
       "Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object");
    dependency_data = None;}
  
  ;;
  
  
  
  let gw_guthib_configuration_subclass =
    {Por_subclass_t.subclass_name = "gw_guthib_configuration";
    subclass_fields = [root_field;dir_for_backup_field;gitpush_after_backup_field;github_url_field;encoding_protected_files_field];
    parent = None; 
    extensions_leading_here = [];
    has_restriction = true; 
    has_constructor = true} ;;
    
    
  Por_space.add_subclass fw_space gw_guthib_configuration_subclass ;;  
    
  
  
  end ;; 
  
  (* End of POR(Polymorphic Ocaml Record)-related code *)

