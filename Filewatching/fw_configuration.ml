(*

#use"Filewatching/fw_configuration.ml";;

*)

module Private = struct 

let salt = "Fw_"^"configuration_t.";;

let root_label                   = salt ^ "root";;
let allowed_endings_label        = salt ^ "allowed_endings";;
let ignored_subdirectories_label = salt ^ "ignored_subdirectories";;
let special_files_label          = salt ^ "special_files";;
let final_cleaner_label          = salt ^ "final_cleaner";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_configuration_t.root = Dfa_root.of_concrete_object(g root_label);
      allowed_endings = Concrete_object_field.to_list Dfa_ending.of_concrete_object (g allowed_endings_label);
      ignored_subdirectories = Concrete_object_field.to_list Dfa_subdirectory.of_concrete_object(g ignored_subdirectories_label);
      special_files = Concrete_object_field.to_list Dfn_rootless.of_concrete_object (g special_files_label);
      final_cleaner = Fw_final_cleaner.of_concrete_object (g final_cleaner_label); 
   };; 

let to_concrete_object config=
   let items= 
   [
    root_label, Dfa_root.to_concrete_object config.Fw_configuration_t.root;
    allowed_endings_label, Concrete_object_field.of_list Dfa_ending.to_concrete_object config.Fw_configuration_t.allowed_endings;
    ignored_subdirectories_label, Concrete_object_field.of_list Dfa_subdirectory.to_concrete_object config.Fw_configuration_t.ignored_subdirectories;
    special_files_label, Concrete_object_field.of_list Dfn_rootless.to_concrete_object config.Fw_configuration_t.special_files;
    final_cleaner_label, Fw_final_cleaner.to_concrete_object config.Fw_configuration_t.final_cleaner;
   ]  in
   Concrete_object_t.Record items;;

end ;;

let root config = config.Fw_configuration_t.root;;
let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;

let constructor root_dir edgs  ign_subdirs spc_files cleaner= 
    {
      Fw_configuration_t.root = root_dir;
      allowed_endings = edgs;
      ignored_subdirectories = ign_subdirs;
      special_files = spc_files;
      final_cleaner = Fw_final_cleaner.constructor(cleaner);
    };; 

let empty_one root_dir = constructor root_dir [] [] [];;


