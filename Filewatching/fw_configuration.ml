(*

#use"Filewatching/fw_configuration.ml";;

*)

module Private = struct 


let salt = "Fw_"^"configuration_t.";;

let root_label                    = salt ^ "root";;
let dir_for_backup_label          = salt ^ "dir_for_backup";;
let gitpush_after_backup_label    = salt ^ "gitpush_after_backup";;
let ignored_subdirectories_label  = salt ^ "ignored_subdirectories";;
let ignored_files_label           = salt ^ "ignored_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Fw_configuration_t.root = Dfa_root.of_concrete_object(g root_label);
      dir_for_backup = Dfa_root.of_concrete_object(g dir_for_backup_label);
      gitpush_after_backup = Concrete_object_field.to_bool (g gitpush_after_backup_label);
      ignored_subdirectories = Concrete_object_field.to_list Dfa_subdirectory.of_concrete_object(g ignored_subdirectories_label);
      ignored_files = Concrete_object_field.to_list Dfn_rootless.of_concrete_object (g ignored_files_label);
   };; 

let to_concrete_object config=
   let items= 
   [
    root_label, Dfa_root.to_concrete_object config.Fw_configuration_t.root;
    dir_for_backup_label, Dfa_root.to_concrete_object config.Fw_configuration_t.dir_for_backup;
    gitpush_after_backup_label, Concrete_object_field.of_bool  config.Fw_configuration_t.gitpush_after_backup;
    ignored_subdirectories_label, Concrete_object_field.of_list Dfa_subdirectory.to_concrete_object config.Fw_configuration_t.ignored_subdirectories;
    ignored_files_label, Concrete_object_field.of_list Dfn_rootless.to_concrete_object config.Fw_configuration_t.ignored_files;
   ]  in
   Concrete_object_t.Record items;;

end ;;

let root config = config.Fw_configuration_t.root;;
let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;

let constructor (root_dir,backup_dir,g_after_b) = 
    {
      Fw_configuration_t.root = root_dir;
      dir_for_backup = backup_dir ;
      gitpush_after_backup = g_after_b ;
      ignored_subdirectories = Coma_constant.git_ignored_subdirectories;
      ignored_files = Coma_constant.git_ignored_files;
    };; 


