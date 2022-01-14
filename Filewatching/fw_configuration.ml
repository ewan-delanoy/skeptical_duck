(*

#use"Filewatching/fw_configuration.ml";;

*)

module Private = struct 

let salt = "Fw_"^"configuration_t.";;

let root_label                           = salt ^ "root";;
let ignored_subdirectories_label         = salt ^ "ignored_subdirectories";;
let ignored_files_label                  = salt ^ "ignored_files";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object.get_record ccrt_obj in
   {
      Fw_configuration_t.root = Dfa_root.of_concrete_object(g root_label);
      ignored_subdirectories = Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object(g ignored_subdirectories_label);
      ignored_files = Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object (g ignored_files_label);
   };; 

let to_concrete_object config=
   let items= 
   [
    root_label, Dfa_root.to_concrete_object config.Fw_configuration_t.root;
    ignored_subdirectories_label, Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object config.Fw_configuration_t.ignored_subdirectories;
    ignored_files_label, Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object config.Fw_configuration_t.ignored_files;
   ]  in
   Concrete_object_t.Record items;;

end ;;

let root config = config.Fw_configuration_t.root;;
let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;

let of_root root_dir = 
    {
      Fw_configuration_t.root = root_dir;
      ignored_subdirectories = Coma_constant.git_ignored_subdirectories;
      ignored_files = [];
    };; 



let test_for_admissibility data rl=
  (List.mem (
    (Dfn_rootless.to_ending rl)
  ) Dfa_ending.endings_for_readable_files)
  &&
   (List.for_all (
     fun sd->not(Dfn_rootless.is_in rl sd)
  ) data.Fw_configuration_t.ignored_subdirectories
  )  
  &&
  (
    not(List.mem rl data.Fw_configuration_t.ignored_files)
  )
  ;;

