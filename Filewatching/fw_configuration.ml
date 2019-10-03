(*

#use"Filewatching/fw_configuration.ml";;

*)

module Private = struct 

let salt = "Fw_"^"configuration_t.";;

let root_label                   = salt ^ "root";;
let ignored_subdirectories_label = salt ^ "ignored_subdirectories";;
let ignored_files_label          = salt ^ "ignored_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_configuration_t.root = Dfa_root.of_concrete_object(g root_label);
      ignored_subdirectories = Concrete_object_field.to_list Dfa_subdirectory.of_concrete_object(g ignored_subdirectories_label);
      ignored_files = Concrete_object_field.to_string_list (g ignored_files_label);
   };; 

let to_concrete_object config=
   let items= 
   [
    root_label, Dfa_root.to_concrete_object config.Fw_configuration_t.root;
    ignored_subdirectories_label, Concrete_object_field.of_list Dfa_subdirectory.to_concrete_object config.Fw_configuration_t.ignored_subdirectories;
    ignored_files_label, Concrete_object_field.of_string_list config.Fw_configuration_t.ignored_files;
   ]  in
   Concrete_object_t.Record items;;

end ;;

let root config = config.Fw_configuration_t.root;;
let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


