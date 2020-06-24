(*

#use"Filewatching/fw_configuration.ml";;

*)

module Private = struct 


let salt = "Fw_"^"configuration_t.";;

let root_label                       = salt ^ "root";;
let allowed_endings_label            = salt ^ "allowed_endings";;
let git_ignored_subdirectories_label = salt ^ "git_ignored_subdirectories";;
let special_git_saved_files_label    = salt ^ "special_git_saved_files";;
let ignored_files_label              = salt ^ "ignored_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_configuration_t.root = Dfa_root.of_concrete_object(g root_label);
      allowed_endings = Concrete_object_field.to_list Dfa_ending.of_concrete_object (g allowed_endings_label);
      git_ignored_subdirectories = Concrete_object_field.to_list Dfa_subdirectory.of_concrete_object(g git_ignored_subdirectories_label);
      special_git_saved_files = Concrete_object_field.to_list Dfn_rootless.of_concrete_object (g special_git_saved_files_label);
      ignored_files = Concrete_object_field.to_list Dfn_rootless.of_concrete_object (g ignored_files_label);
   };; 

let to_concrete_object config=
   let items= 
   [
    root_label, Dfa_root.to_concrete_object config.Fw_configuration_t.root;
    allowed_endings_label, Concrete_object_field.of_list Dfa_ending.to_concrete_object config.Fw_configuration_t.allowed_endings;
    git_ignored_subdirectories_label, Concrete_object_field.of_list Dfa_subdirectory.to_concrete_object config.Fw_configuration_t.git_ignored_subdirectories;
    special_git_saved_files_label, Concrete_object_field.of_list Dfn_rootless.to_concrete_object config.Fw_configuration_t.special_git_saved_files;
    ignored_files_label, Concrete_object_field.of_list Dfn_rootless.to_concrete_object config.Fw_configuration_t.ignored_files;
   ]  in
   Concrete_object_t.Record items;;

end ;;

let root config = config.Fw_configuration_t.root;;
let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;

let constructor (root_dir,edgs,ign_subdirs,spc_files,ign_files)= 
    {
      Fw_configuration_t.root = root_dir;
      allowed_endings = edgs;
      git_ignored_subdirectories = ign_subdirs;
      special_git_saved_files = spc_files;
      ignored_files = ign_files;
    };; 

let default root_dir = 
    {
      Fw_configuration_t.root = root_dir;
      allowed_endings = Dfa_ending.all_ocaml_endings;
      git_ignored_subdirectories = Coma_constant.git_ignored_subdirectories;
      special_git_saved_files = Coma_constant.special_git_saved_files;
      ignored_files = Coma_constant.git_ignored_files;
    };; 


