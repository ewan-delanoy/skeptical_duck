(*

#use"Filewatching/fw_with_dependencies.ml";;

*)


(*
module Private = struct 

  let index fw = fw.Fw_with_dependencies_t.index_for_caching ;;  
  let parent fw = fw.Fw_with_dependencies_t.parent ;;
  
  let new_state (instance,state) = (instance,Fw_indexer.new_state instance) ;;
  
  let getter f fw = f (parent fw) ;;
  
  type fw_method_label =
    Constructor
   |Univariate 
   |Zerovariate_producer 
   |Univariate_producer ;;   
  
  type ('fw,'a,'b) fw_method = FWM of fw_method_label * ( 'fw -> 'a -> ('fw * 'b) );;

  let constructor f = FWM(Constructor,(fun fw arg->(f arg, ()) )) ;;
  let univariate f =  FWM(Univariate,(fun fw arg->(f fw arg, ()) )) ;;
  let zerovariate_producer f =  FWM(Zerovariate_producer,(fun fw arg->(f fw) )) ;;  
  let univariate_producer f =  FWM(Univariate_producer,(fun fw arg->(f fw arg) )) ;; 

  
  let transpose (local_hashtbl,local_get) (FWM(fw_label,f)) opt_g =
    FWM(fw_label,
     fun old_fw arg ->
      let visible_result = f old_fw arg in 
      let (new_fw,additional_data) = visible_result in 
      let new_idx = new_fw.Fw_with_dependencies_t.index_for_caching in
      let old_dep_val = local_get old_fw in 
      let new_dep_val = 
      (match opt_g with 
      None -> local_get new_fw
      |Some(transposer) ->transposer new_fw old_dep_val arg additional_data) in 
      let _ = (Hashtbl.add local_hashtbl new_idx new_dep_val) in
      visible_result   
    );; 
  

  module Enter = struct 
    
    let empty_one = constructor Fw_with_small_details.empty_one;;
    let forget_modules = univariate Fw_with_small_details.forget_modules ;;
    let inspect_and_update = zerovariate_producer Fw_with_small_details.inspect_and_update;;
    let of_concrete_object = constructor Fw_with_small_details.of_concrete_object ;;
    let of_configuration = constructor Fw_with_small_details.of_configuration ;;
    let of_configuration_and_list = constructor Fw_with_small_details.of_configuration_and_list ;;
    let overwrite_file_if_it_exists =univariate Fw_with_small_details.overwrite_file_if_it_exists;;
    let register_rootless_paths = univariate_producer Fw_with_small_details.register_rootless_paths ;;  
    let relocate_module_to = univariate Fw_with_small_details.relocate_module_to ;;
    let remove_files = univariate Fw_with_small_details.remove_files ;;
    let rename_module_on_filename_level_and_in_files = univariate_producer Fw_with_small_details.rename_module_on_filename_level_and_in_files;;  
    let rename_subdirectory_as = univariate Fw_with_small_details.rename_subdirectory_as;;
    let replace_string = univariate_producer Fw_with_small_details.replace_string ;;
    let replace_value = univariate_producer Fw_with_small_details.replace_value ;;
    let set_gitpush_after_backup = univariate Fw_with_small_details.set_gitpush_after_backup ;;
    let set_last_noticed_changes = univariate Fw_with_small_details.set_last_noticed_changes ;;
    
  end ;;  

  module Cached = struct 
    
    let add_caching (FWM(fw_label,f)) =
      FWM(fw_label,
        fun fw arg ->
        let old_parent = parent fw in 
        let (new_parent,additional_data) = f old_parent arg in 
        ({
          Fw_with_dependencies_t.parent = new_parent ;
          index_for_caching = Fw_indexer.new_instance ();
        },additional_data)
      ) ;;
      
    let empty_one = add_caching Enter.empty_one ;;
    let forget_modules = add_caching Enter.forget_modules ;;
    let inspect_and_update = add_caching Enter.inspect_and_update ;;
    let of_concrete_object = add_caching Enter.of_concrete_object ;;
    let of_configuration = add_caching Enter.of_configuration ;;
    let of_configuration_and_list = add_caching Enter.of_configuration_and_list ;;
    let overwrite_file_if_it_exists = add_caching Enter.overwrite_file_if_it_exists ;;
    let register_rootless_paths = add_caching Enter.register_rootless_paths ;;
    let relocate_module_to = add_caching Enter.relocate_module_to ;;
    let remove_files = add_caching Enter.remove_files ;;
    let rename_module_on_filename_level_and_in_files = add_caching Enter.rename_module_on_filename_level_and_in_files ;;
    let rename_subdirectory_as = add_caching Enter.rename_subdirectory_as ;;
    let replace_string = add_caching Enter.replace_string ;;
    let replace_value = add_caching Enter.replace_value ;;
    let set_gitpush_after_backup = add_caching Enter.set_gitpush_after_backup ;;
    let set_last_noticed_changes = add_caching Enter.set_last_noticed_changes ;;


  end ;;  
*)


  (* 
  module Modularized_details = struct

    let the_hashtbl = ((Hashtbl.create 10));;
    let force_get fw = Fw_compute_first_dependencies.main (fw.Fw_with_dependencies_t.parent) ;;
    let get fw =
      let idx = fw.Fw_with_dependencies_t.index_for_caching in 
      match Hashtbl.find_opt the_hashtbl idx with 
      Some(l)-> l
      |None ->
       let answer = force_get fw in 
       let _ = (Hashtbl.add the_hashtbl idx answer) in 
       answer ;;
    let trsp fwm opt_g = transpose (the_hashtbl,get) fwm opt_g ;; 
    let empty_one = trsp Cached.empty_one (Some(fun new_fw old_dep_val arg ad -> [])) ;;
    let forget_modules = trsp Cached.forget_modules (Some(fun new_fw old_dep_val mod_names ad->
       let selector1 = List.filter (fun mn->not(List.mem mn mod_names)) in 
       Option.filter_and_unpack (
          fun  (mn,su,pr_end,pr_details,opt_mli_data,fath,ances) ->
            if List.mem mn mod_names
            then None 
            else Some(mn,su,pr_end,pr_details,opt_mli_data,selector1 fath,selector1 ances)  
       )  old_dep_val
      )) ;;
    let inspect_and_update = trsp Cached.inspect_and_update (Some(fun new_fw old_dep_val arg ad->
      let (a_files,u_files,nc_files) = ad in
      Option.filter_and_unpack (
        (* order is important when recomputing coatoms *)
        fun  (mn,su,pr_end,pr_details,opt_mli_data,fath,ances) ->
          if List.mem mn u_files
          then None 
          else Some(mn,su,pr_end,pr_details,opt_mli_data,selector1 fath,selector1 ances)  
      )  old_dep_val
      )) ;;

  end ;;  
  *)
end;;

  (*

  (mn,Dfn_rootless.to_subdirectory pr_rless,
       Dfn_rootless.to_ending pr_rless,pr_details,opt_mli_rless,
       fathers,ancestors)  

  (mods,su,pr_end,opt_mli_data,fath,ances)

e

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

