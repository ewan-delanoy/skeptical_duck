(*

#use"Filewatching/fw_with_dependencies.ml";;

*)

(*

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

  module Without_dependencies = struct 

  let empty_one = constructor Fw_with_small_details.empty_one;;
  let forget_modules = univar Fw_with_small_details.forget_modules ;;
  let inspect_and_update = zeroplump Fw_with_small_details.inspect_and_update;;
  let of_concrete_object = constructor Fw_with_small_details.of_concrete_object ;;
  let of_configuration = constructor Fw_with_small_details.of_configuration ;;
  let of_configuration_and_list = constructor Fw_with_small_details.of_configuration_and_list ;;
  let overwrite_file_if_it_exists =univar Fw_with_small_details.overwrite_file_if_it_exists;;
  let register_rootless_paths = uniplump Fw_with_small_details.register_rootless_paths ;;  
  let relocate_module_to = univar Fw_with_small_details.relocate_module_to ;;
  let remove_files = univar Fw_with_small_details.remove_files ;;
  let rename_module_on_filename_level_and_in_files = uniplump Fw_with_small_details.rename_module_on_filename_level_and_in_files;;  
  let rename_subdirectory_as = univar Fw_with_small_details.rename_subdirectory_as;;
  let replace_string = uniplump Fw_with_small_details.replace_string ;;
  let replace_value = uniplump Fw_with_small_details.replace_value ;;
  let set_gitpush_after_backup = univar Fw_with_small_details.set_gitpush_after_backup ;;
  let set_last_noticed_changes = univar Fw_with_small_details.set_last_noticed_changes ;;

  end ;;  

  module First_dependencies = struct 

    let the_hashtbl = ((Hashtbl.create 10) : (Fw_instance_index_t.t * Fw_state_index_t.t,
    Dfa_module_t.t list * (Dfa_module_t.t * Dfa_subdirectory_t.t) list *
    (Dfa_module_t.t * Dfa_ending_t.t) list * (Dfa_module_t.t * bool) list *
    string list * string list * Dfa_module_t.t list list *
    (Dfa_module_t.t * Dfa_module_t.t list) list)
    Hashtbl.t );;
   
    let get fw =
       match Hashtbl.find_opt the_hashtbl (fw.Fw_with_dependencies_t.index_for_caching) with 
       Some(mods,su,pr_end,mli,pr_mt,mli_mt,fath,ances)->(mods,su,pr_end,mli,pr_mt,mli_mt,fath,ances)
       |None ->
        Fw_compute_first_dependencies.main (fw.Fw_with_dependencies_t.parent) ;;

    let empty_one config = 
       let current = Without_dependencies.empty_one config in 
       let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching)
       ([],[],[],[],[],[],[],[]) in 
       current ;;

    let forget_modules fw mod_names = 
        let current = Without_dependencies.forget_modules fw mod_names in 
        let (old_mods,old_su,old_pr_end,old_mli,old_pr_mt,old_mli_mt,old_fath,old_ances) = get fw in 
        let indexed_mods = Ennig.index_everything mods in 
        let bad_indices 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let inspect_and_update fw arg = 
        let current = Without_dependencies.inspect_and_update fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let of_concrete_object fw arg = 
        let current = Without_dependencies.of_concrete_object fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let of_configuration fw arg = 
        let current = Without_dependencies.of_configuration fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let of_configuration_and_list fw arg = 
        let current = Without_dependencies.of_configuration_and_list fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let overwrite_file_if_it_exists fw arg = 
        let current = Without_dependencies.overwrite_file_if_it_exists fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let register_rootless_paths fw arg = 
        let current = Without_dependencies.register_rootless_paths fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let relocate_module_to fw arg = 
        let current = Without_dependencies.relocate_module_to fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let remove_files fw arg = 
        let current = Without_dependencies.remove_files fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let rename_module_on_filename_level_and_in_files fw arg = 
        let current = Without_dependencies.rename_module_on_filename_level_and_in_files fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let rename_subdirectory_as fw arg = 
        let current = Without_dependencies.rename_subdirectory_as fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let replace_string fw arg = 
        let current = Without_dependencies.replace_string fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let replace_value fw arg = 
        let current = Without_dependencies.replace_value fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let set_gitpush_after_backup fw arg = 
        let current = Without_dependencies.set_gitpush_after_backup fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;

      let set_last_noticed_changes fw arg = 
        let current = Without_dependencies.set_last_noticed_changes fw arg in 
        let _ =Hashtbl.add the_hashtbl (current.Fw_with_dependencies_t.index_for_caching) 
        () in 
        current ;;    
  
  
    end ;;   
  

    module Whole = struct 

      let empty_one                   = First_dependencies.empty_one ;;
      let forget_modules              = First_dependencies.forget_modules ;;
      let inspect_and_update          = First_dependencies.inspect_and_update ;;
      let of_concrete_object          = First_dependencies.of_concrete_object ;;
      let of_configuration            = First_dependencies.of_configuration ;;
      let of_configuration_and_list   = First_dependencies.of_configuration_and_list ;;
      let overwrite_file_if_it_exists = First_dependencies.overwrite_file_if_it_exists;;
      let register_rootless_paths     = First_dependencies.register_rootless_paths ;;  
      let relocate_module_to          = First_dependencies.relocate_module_to ;;
      let remove_files                = First_dependencies.remove_files ;;
      let rename_module_on_filename_level_and_in_files = First_dependencies.rename_module_on_filename_level_and_in_files;;  
      let rename_subdirectory_as = First_dependencies.rename_subdirectory_as;;
      let replace_string = First_dependencies.replace_string ;;
      let replace_value = First_dependencies.replace_value ;;
      let set_gitpush_after_backup = First_dependencies.set_gitpush_after_backup ;;
      let set_last_noticed_changes = First_dependencies.set_last_noticed_changes ;;
    
      end ;;  

  end ;;   
  
let configuration = Private.getter Fw_with_small_details.configuration ;;
let empty_one = Private.Whole.empty_one;;
let forget_modules = Private.Whole.forget_modules ;;
let get_content = Private.getter Fw_with_small_details.get_content ;;  
let get_mtime = Private.getter Fw_with_small_details.get_mtime ;;    
let get_mtime_or_zero_if_file_is_nonregistered = Private.getter Fw_with_small_details.get_mtime_or_zero_if_file_is_nonregistered ;;  
let inspect_and_update = Private.Whole.inspect_and_update;;
let last_noticed_changes = Private.getter Fw_with_small_details.last_noticed_changes ;;
let noncompilable_files = Private.getter Fw_with_small_details.noncompilable_files ;;
let of_concrete_object = Private.Whole.of_concrete_object ;;
let of_configuration = Private.Whole.of_configuration ;;
let of_configuration_and_list = Private.Whole.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.Whole.overwrite_file_if_it_exists;;
let reflect_latest_changes_in_github = Private.univar Fw_with_small_details.reflect_latest_changes_in_github ;;
let register_rootless_paths = Private.Whole.register_rootless_paths ;;
let relocate_module_to = Private.Whole.relocate_module_to ;;
let remove_files = Private.Whole.remove_files ;;
let rename_module_on_filename_level_and_in_files = Private.Whole.rename_module_on_filename_level_and_in_files;;
let rename_subdirectory_as = Private.Whole.rename_subdirectory_as;;
let replace_string = Private.Whole.replace_string ;;
let replace_value = Private.Whole.replace_value ;;
let root = Private.getter Fw_with_small_details.root ;;
let set_gitpush_after_backup = Private.univar Fw_with_small_details.set_gitpush_after_backup ;;
let set_last_noticed_changes = Private.univar Fw_with_small_details.set_last_noticed_changes ;;
let to_concrete_object = Private.getter Fw_with_small_details.to_concrete_object ;;
let usual_compilable_files = Private.getter Fw_with_small_details.usual_compilable_files ;;

*)

