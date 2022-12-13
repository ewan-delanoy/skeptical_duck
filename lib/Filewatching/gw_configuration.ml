(*

#use"lib/Filewatching/gw_configuration.ml";;

*)

(* Beginning of POR(Polymorphic Ocaml Record)-related code *)

module Background = struct 
let root_field =
  {Por_types.field_name = "root";
     field_type = "Dfa_root_t.t"; 
     var_name = "r";
     default_value = "Dfa_root.of_line \"dummy\"";
     crobj_converters =
      Some ("Dfa_root.of_concrete_object", 
      "Dfa_root.to_concrete_object")} ;; 
  
  let ignored_subdirectories_field =
  {Por_types.field_name = "ignored_subdirectories";
     field_type = "Dfa_subdirectory_t.t list"; 
     var_name = "ign_subdirs";
     default_value = "[]";
     crobj_converters =
            Some ("Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object", 
            "Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object")} ;;     
  
  
  let ignored_files_field =
  {Por_types.field_name = "ignored_files";
  field_type = "Dfn_rootless_t.t list"; 
  var_name = "ign_files";
  default_value = "[]";
  crobj_converters =
  Some ("Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object", 
  "Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object")} ;;     
              
  let gw_configuration_subclass =
  {Por_subclass_t.subclass_name = "gw_configuration";
     subclass_fields =
      [root_field;
       ignored_subdirectories_field;
       ignored_files_field];
     parent = None; 
     extensions_leading_here = []; 
     has_restriction = true;
     has_constructor = true} ;;           

Por_space.add_subclass Por_space_example.filewatching gw_configuration_subclass ;;

end ;; 



(* End of POR(Polymorphic Ocaml Record)-related code *)



let of_root root_dir = 
    Gw_poly.construct_gw_configuration 
      ~root:root_dir
      ~ignored_subdirectories:Coma_constant.git_ignored_subdirectories
      ~ignored_files:[]
    ;; 

let test_for_admissibility data rl=
  (List.mem (
    (Dfn_rootless.to_ending rl)
  ) Dfa_ending.endings_for_readable_files)
  &&
   (List.for_all (
     fun sd->not(Dfn_rootless.is_in rl sd)
  ) (Gw_poly.ignored_subdirectories data)
  )  
  &&
  (
    not(List.mem rl (Gw_poly.ignored_files data))
  )
  ;;



