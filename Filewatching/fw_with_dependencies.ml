(*

#use"Filewatching/fw_with_dependencies.ml";;

*)

module Private = struct 

  let index fw = fw.Fw_with_dependencies_t.index_for_caching ;;  
  let parent fw = fw.Fw_with_dependencies_t.parent ;;
  
  let new_state (instance,state) = (instance,Fw_indexer.new_state instance) ;;
  
  let getter f fw = f (parent fw) ;;
  
  let constructor f arg =
    {
       Fw_with_dependencies_t.parent = f arg;
       index_for_caching = Fw_indexer.new_instance ();
    } ;;

  let univar f fw arg=
     let old_parent = parent fw  in 
     let new_parent = f old_parent arg
     and new_index = new_state (index fw) in  
    {
       Fw_with_dependencies_t.parent = new_parent;
       index_for_caching = new_index;
    } ;;

  let zeroplump f fw =
    let old_parent = parent fw  in 
    let (new_parent,additional_data) = f old_parent
    and new_index = new_state (index fw) in  
   ({
      Fw_with_dependencies_t.parent = new_parent;
      index_for_caching = new_index;
   },additional_data) ;;  
  
  let uniplump f fw arg=
    let old_parent = parent fw  in 
    let (new_parent,additional_data) = f old_parent arg
    and new_index = new_state (index fw) in  
   ({
      Fw_with_dependencies_t.parent = new_parent;
      index_for_caching = new_index;
   },additional_data) ;; 

  end ;;   
  
let configuration = Private.getter Fw_with_small_details.configuration ;;
let empty_one = Private.constructor Fw_with_small_details.empty_one;;
let forget_modules = Private.univar Fw_with_small_details.forget_modules ;;
let get_content = Private.getter Fw_with_small_details.get_content ;;  
let get_mtime = Private.getter Fw_with_small_details.get_mtime ;;    
let get_mtime_or_zero_if_file_is_nonregistered = Private.getter Fw_with_small_details.get_mtime_or_zero_if_file_is_nonregistered ;;  
let inspect_and_update = Private.zeroplump Fw_with_small_details.inspect_and_update;;
let last_noticed_changes = Private.getter Fw_with_small_details.last_noticed_changes ;;
let noncompilable_files = Private.getter Fw_with_small_details.noncompilable_files ;;
let of_concrete_object = Private.constructor Fw_with_small_details.of_concrete_object ;;
let of_configuration = Private.constructor Fw_with_small_details.of_configuration ;;
let of_configuration_and_list = Private.constructor Fw_with_small_details.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.univar Fw_with_small_details.overwrite_file_if_it_exists;;
let reflect_latest_changes_in_github = Private.univar Fw_with_small_details.reflect_latest_changes_in_github ;;
let register_rootless_paths = Private.uniplump Fw_with_small_details.register_rootless_paths ;;
let relocate_module_to = Private.univar Fw_with_small_details.relocate_module_to ;;
let remove_files = Private.univar Fw_with_small_details.remove_files ;;
let rename_module_on_filename_level_and_in_files = Private.uniplump Fw_with_small_details.rename_module_on_filename_level_and_in_files;;
let rename_subdirectory_as = Private.univar Fw_with_small_details.rename_subdirectory_as;;
let replace_string = Private.uniplump Fw_with_small_details.replace_string ;;
let replace_value = Private.uniplump Fw_with_small_details.replace_value ;;
let root = Private.getter Fw_with_small_details.root ;;
let set_gitpush_after_backup = Private.univar Fw_with_small_details.set_gitpush_after_backup ;;
let set_last_noticed_changes = Private.univar Fw_with_small_details.set_last_noticed_changes ;;
let to_concrete_object = Private.getter Fw_with_small_details.to_concrete_object ;;
let usual_compilable_files = Private.getter Fw_with_small_details.usual_compilable_files ;;



