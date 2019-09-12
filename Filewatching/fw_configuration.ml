(*

#use"Filewatching/fw_configuration.ml";;

*)

module Private = struct 

let salt = "Fw_"^"configuration.";;

let root_label                      = salt ^ "root";;
let ignored_subdirectories_label    = salt ^ "ignored_subdirectories";;
let additional_files_label          = salt ^ "additional_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_configuration_t.root = Dfa_root.of_concrete_object(g root_label);
      ignored_subdirectories = Concrete_object_field.to_list Dfa_subdirectory.of_concrete_object(g ignored_subdirectories_label);
      additional_files = Concrete_object_field.to_list Dfn_rootless.of_concrete_object (g additional_files_label);
   };; 

let to_concrete_object cs=
   let items= 
   [
    root_label, Dfa_root.to_concrete_object cs.Fw_configuration_t.root;
    ignored_subdirectories_label, Concrete_object_field.of_list Dfa_subdirectory.to_concrete_object cs.Fw_configuration_t.ignored_subdirectories;
    additional_files_label, Concrete_object_field.of_list Dfn_rootless.to_concrete_object  cs.Fw_configuration_t.additional_files;
   ]  in
   Concrete_object_t.Record items;;

end ;;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


