(*

#use"Filewatching/fw_with_dependencies.ml";;

*)

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
  let zerovariate_producer f =  FWM(Zerovariate_producer,(fun fw (arg:unit)->(f fw) )) ;;  
  let univariate_producer f =  FWM(Univariate_producer,(fun fw arg->(f fw arg) )) ;; 

  (* This dummy stuff is sub-optimal and ugly *)
  let dummy_fw = {
    Fw_with_dependencies_t.parent = Fw_with_small_details.dummy ;
    index_for_caching = (Fw_instance_index_t.I 0,Fw_state_index_t.I 0) ; 
   } ;; 

  let extract_constructor (FWM(fw_label,f))          = (fun arg->fst(f dummy_fw arg)) ;;
  let extract_univariate (FWM(fw_label,f))           = (fun fw arg->fst(f fw arg)) ;;
  let extract_zerovariate_producer (FWM(fw_label,f)) = (fun fw ->f fw ()) ;;
  let extract_univariate_producer  (FWM(fw_label,f)) = (fun fw arg->(f fw arg) ) ;;  
  
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
    let overwrite_file_if_it_exists = univariate_producer Fw_with_small_details.overwrite_file_if_it_exists;;
    let register_rootless_paths = univariate_producer Fw_with_small_details.register_rootless_paths ;;  
    let relocate_module_to = univariate_producer Fw_with_small_details.relocate_module_to ;;
    let remove_files = univariate_producer Fw_with_small_details.remove_files ;;
    let rename_module_on_filename_level_and_in_files = univariate_producer Fw_with_small_details.rename_module_on_filename_level_and_in_files;;  
    let rename_subdirectory_as = univariate_producer Fw_with_small_details.rename_subdirectory_as;;
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



  
  module Modularized_details = struct

    let the_hashtbl = ((Hashtbl.create 10));;
    let force_get fw = Fw_module_small_details.modularize_details (fw.Fw_with_dependencies_t.parent) ;;
    let get fw =
      let idx = fw.Fw_with_dependencies_t.index_for_caching in 
      match Hashtbl.find_opt the_hashtbl idx with 
      Some(l)-> l
      |None ->
       let answer = force_get fw in 
       let _ = (Hashtbl.add the_hashtbl idx answer) in 
       answer ;;
    let trsp fwm opt_g = transpose (the_hashtbl,get) fwm opt_g ;; 
    let usual_trsp older_f = trsp older_f (Some(fun new_fw old_dep_val arg ad->
      let tempf = (
        fun old_pair -> 
          let (mn,details) = old_pair in 
          let temp1 = List.filter (fun (rl,new_pair_for_rl)->
             (Dfn_rootless.to_module rl)= mn
            ) ad in
          if temp1 <> []
          then let new_parent = parent new_fw in 
               (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
          else old_pair 
      ) in 
      Image.image tempf old_dep_val
    )) ;;  
    let passive_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
      old_dep_val
    )) ;;  

    let empty_one = trsp Cached.empty_one (Some(fun new_fw old_dep_val arg ad -> [])) ;;
    let forget_modules = trsp Cached.forget_modules (Some(fun new_fw old_dep_val mod_names ad->
       List.filter (fun (mn,_)->not(List.mem mn mod_names)) old_dep_val
      )) ;; 
    let inspect_and_update = trsp Cached.inspect_and_update (Some(fun new_fw old_dep_val arg ad->
        let ((a_files,u_files,nc_files),changed_u_files) = ad in 
        let tempf = (
          fun old_pair -> 
            let (mn,details) = old_pair in 
            let temp1 = List.filter (fun (rl,details2)->
               (Dfn_rootless.to_module rl)= mn
              ) changed_u_files in
            if temp1 <> []
            then (mn, Fw_module_small_details.compute_details_from_acolytes_list_for_one_module temp1)
            else old_pair 
        ) in 
        Image.image tempf old_dep_val
        )) ;; 
    let empty_one = trsp Cached.empty_one (Some(fun new_fw old_dep_val arg ad -> [])) ;;  
    let of_concrete_object = trsp Cached.of_concrete_object None ;;   
    let of_configuration = trsp Cached.of_configuration None ;;  
    let of_configuration_and_list = trsp Cached.of_configuration_and_list None ;;   

    
    let overwrite_file_if_it_exists = trsp Cached.overwrite_file_if_it_exists (Some(fun new_fw old_dep_val arg ad->
      match ad with 
      None -> old_dep_val 
      |Some(change) ->
      let tempf = (
        fun old_pair -> 
          let (mn,details) = old_pair in 
          let temp1 = List.filter (fun (rl,details2)->
             (Dfn_rootless.to_module rl)= mn
            ) [change] in
          if temp1 <> []
          then let new_parent = parent new_fw in 
               (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
          else old_pair 
      ) in 
      Image.image tempf old_dep_val
      )) ;;
      
    let register_rootless_paths = trsp Cached.register_rootless_paths (Some(fun new_fw old_dep_val arg ad->
        let ((a_files,u_files,nc_files),new_details) = ad in
        let old_details = [] in
        let (overlapping,nonoverlapping) = List.partition (
           fun (rl,_) -> (List.assoc_opt rl old_details)<>None 
        ) new_details in 
        let tempf1 = (
          fun old_pair -> 
            let (mn,details) = old_pair in 
            let temp1 = Option.filter_and_unpack (fun (rl,details2)->
               if (Dfn_rootless.to_module rl)= mn
               then Some(rl,Some(rl,details2))
               else None 
              ) overlapping in
            if temp1 <> []
            then let new_parent = parent new_fw in 
                 (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
            else old_pair 
        ) in 
        (Image.image tempf1 old_dep_val)@
        (Fw_module_small_details.compute_details_from_acolytes_list_for_several_modules nonoverlapping)
        )) ;; 

    let relocate_module_to = usual_trsp Cached.relocate_module_to  ;;  
    let remove_files = usual_trsp Cached.remove_files  ;;
    let rename_module_on_filename_level_and_in_files = usual_trsp Cached.rename_module_on_filename_level_and_in_files  ;; 
    let rename_subdirectory_as = usual_trsp Cached.rename_subdirectory_as  ;;     
    let replace_string = usual_trsp Cached.replace_string  ;;   
    let replace_value = usual_trsp Cached.replace_value  ;;    
    let set_gitpush_after_backup = passive_trsp Cached.set_gitpush_after_backup ;;
    let set_last_noticed_changes = passive_trsp Cached.set_last_noticed_changes ;;
   
         
        
  end ;;  

  module Order = struct 

    let the_hashtbl = ((Hashtbl.create 10));;
    let force_get fw = Fw_determine_order.main 
      (Modularized_details.get fw) ;;
    let get fw =
      let idx = fw.Fw_with_dependencies_t.index_for_caching in 
      match Hashtbl.find_opt the_hashtbl idx with 
      Some(l)-> l
      |None ->
       let answer = force_get fw in 
       let _ = (Hashtbl.add the_hashtbl idx answer) in 
       answer ;;
    let trsp fwm opt_g = transpose (the_hashtbl,get) fwm opt_g ;;   
    let usual_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
        let modules_in_old_order = Image.image fst old_dep_val in 
        let details_in_old_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
         (Modularized_details.get new_fw) modules_in_old_order in 
         Fw_determine_order.main  details_in_old_order
      )) ;;
    let passive_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
        old_dep_val
      )) ;;  

    let empty_one  = trsp Modularized_details.empty_one (Some(fun new_fw old_dep_val arg ad -> 
      []
    ));;
    let forget_modules = trsp Modularized_details.forget_modules (Some(fun new_fw old_dep_val mod_names ad -> 
      List.filter (fun (mn,_)->not(List.mem mn mod_names)) old_dep_val 
    ));;
    let inspect_and_update = usual_trsp Modularized_details.inspect_and_update ;;
    let of_concrete_object          = trsp Modularized_details.of_concrete_object None ;;
    let of_configuration            = trsp Modularized_details.of_configuration None ;;
    let of_configuration_and_list   = trsp Modularized_details.of_configuration_and_list None ;;
    let overwrite_file_if_it_exists = usual_trsp Modularized_details.overwrite_file_if_it_exists;;
    let register_rootless_paths     = trsp Modularized_details.register_rootless_paths 
     (Some(fun new_fw old_dep_val arg ad -> 
      let extended_details_list = Modularized_details.get new_fw in 
      let new_details = Listennou.big_tail (List.length old_dep_val) extended_details_list in
      let new_modules_in_order = Image.image fst (Fw_determine_order.main new_details) in 
      let new_details_in_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
          new_details new_modules_in_order in 
          Fw_determine_order.compute_coatoms_and_ancestors_in_small_extension
      old_dep_val new_details_in_order        
     ));;;;  
    let relocate_module_to          = passive_trsp Modularized_details.relocate_module_to ;;
    let remove_files                = trsp Modularized_details.remove_files None;;
    let rename_module_on_filename_level_and_in_files = trsp Modularized_details.rename_module_on_filename_level_and_in_files
    (Some(fun new_fw old_dep_val arg ad -> 
      let (old_mname,new_mname,_) = arg in
      let rep = (fun mn->if mn = old_mname then new_mname else mn) in  
      Image.image (fun (mn2,(coat_mn2,ancestors_mn2)) ->
          (rep mn2,(Image.image rep coat_mn2,Image.image rep ancestors_mn2))
        ) old_dep_val
    ));;  
    let rename_subdirectory_as = passive_trsp Modularized_details.rename_subdirectory_as;;
    let replace_string = usual_trsp Modularized_details.replace_string ;;
    let replace_value = usual_trsp Modularized_details.replace_value ;;
    let set_gitpush_after_backup = passive_trsp Modularized_details.set_gitpush_after_backup ;;
    let set_last_noticed_changes = passive_trsp Modularized_details.set_last_noticed_changes ;;

  end ;;  

  module Needed_dirs = struct 

    let the_hashtbl = ((Hashtbl.create 10));;
    let force_get fw =  
      let details = Modularized_details.get fw in 
      let subdir_at_module = (fun mn->
        Fw_module_small_details.subdirectory(List.assoc mn details) 
      ) in
      Image.image (
         fun (mn,(_,ancestors)) ->
          let temp1 = Image.image subdir_at_module (mn::ancestors) in
          (mn,Ordered.sort Total_ordering.standard temp1) 
      ) (Order.get fw) ;;
    let get fw =
      let idx = fw.Fw_with_dependencies_t.index_for_caching in 
      match Hashtbl.find_opt the_hashtbl idx with 
      Some(l)-> l
      |None ->
       let answer = force_get fw in 
       let _ = (Hashtbl.add the_hashtbl idx answer) in 
       answer ;;
    let trsp fwm opt_g = transpose (the_hashtbl,get) fwm opt_g ;;   
    (*
    let usual_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
      )) ;;
    *)  
    let passive_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
        old_dep_val
      )) ;; 
    
      let empty_one = trsp Order.empty_one (Some(fun new_fw old_dep_val arg ad -> 
        []
      ));;
      let forget_modules = trsp Order.forget_modules None ;;
      let inspect_and_update = trsp Order.inspect_and_update None ;;
      let of_concrete_object = trsp Order.of_concrete_object None ;;
      let of_configuration = trsp Order.of_configuration None ;;
      let of_configuration_and_list = trsp Order.of_configuration_and_list None ;;
      let overwrite_file_if_it_exists = trsp Order.overwrite_file_if_it_exists None ;;
      let register_rootless_paths = trsp Order.register_rootless_paths None ;;  
      let relocate_module_to = trsp Order.relocate_module_to None ;;
      let remove_files = trsp Order.remove_files None ;;
      let rename_module_on_filename_level_and_in_files = passive_trsp Order.rename_module_on_filename_level_and_in_files;;  
      let rename_subdirectory_as = trsp Order.rename_subdirectory_as (Some(fun new_fw old_dep_val arg ad -> 
        let rep = (fun sdir ->
          match Dfa_subdirectory.soak arg sdir with 
          None -> sdir 
          |Some new_sdir -> new_sdir   
        ) in 
        Image.image (fun (mn,sdirs)->(mn,Image.image rep sdirs) ) old_dep_val
      ));;
      let replace_string = trsp Order.replace_string None ;;
      let replace_value = trsp Order.replace_value None ;;
      let set_gitpush_after_backup = passive_trsp Order.set_gitpush_after_backup ;;
      let set_last_noticed_changes = passive_trsp Order.set_last_noticed_changes ;;
        

  end ;;  
  


  module Needed_libs = struct 

    let the_hashtbl = ((Hashtbl.create 10));;
    let force_get fw =  
      let details = Modularized_details.get fw in 
      let needed_libs_at_module = (fun mn->
        Fw_module_small_details.used_libraries (List.assoc mn details) 
      ) in
      Image.image (
         fun (mn,(_,ancestors)) ->
          let temp1 = Image.image needed_libs_at_module (mn::ancestors) in
          (mn,Ordered.sort Total_ordering.standard temp1) 
      ) (Order.get fw) ;;
    let get fw =
      let idx = fw.Fw_with_dependencies_t.index_for_caching in 
      match Hashtbl.find_opt the_hashtbl idx with 
      Some(l)-> l
      |None ->
       let answer = force_get fw in 
       let _ = (Hashtbl.add the_hashtbl idx answer) in 
       answer ;;
    let trsp fwm opt_g = transpose (the_hashtbl,get) fwm opt_g ;;   
    (*
    let usual_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
      )) ;;
    *)  
    let passive_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
        old_dep_val
      )) ;; 
    
    let empty_one = trsp Needed_dirs.empty_one (Some(fun new_fw old_dep_val arg ad -> 
      []
    ));;
    let forget_modules = trsp Needed_dirs.forget_modules None ;;
    let inspect_and_update = trsp Needed_dirs.inspect_and_update None ;;
    let of_concrete_object = trsp Needed_dirs.of_concrete_object None ;;
    let of_configuration = trsp Needed_dirs.of_configuration None ;;
    let of_configuration_and_list = trsp Needed_dirs.of_configuration_and_list None ;;
    let overwrite_file_if_it_exists = trsp Needed_dirs.overwrite_file_if_it_exists None ;;
    let register_rootless_paths = trsp Needed_dirs.register_rootless_paths None ;;  
    let relocate_module_to = passive_trsp Needed_dirs.relocate_module_to ;;
    let remove_files = trsp Needed_dirs.remove_files None ;;
    let rename_module_on_filename_level_and_in_files = trsp Needed_dirs.rename_module_on_filename_level_and_in_files
    (Some(fun new_fw old_dep_val arg ad -> 
      let (old_mname,new_mname,_) = arg in
      let rep = (fun mn->if mn = old_mname then new_mname else mn) in 
      Image.image (fun (mn,libs)->(rep mn,libs) ) old_dep_val
    ));; 
    let rename_subdirectory_as = passive_trsp Needed_dirs.rename_subdirectory_as ;;
    let replace_string = trsp Needed_dirs.replace_string None ;;
    let replace_value = trsp Needed_dirs.replace_value None ;;
    let set_gitpush_after_backup = passive_trsp Needed_dirs.set_gitpush_after_backup ;;
    let set_last_noticed_changes = passive_trsp Needed_dirs.set_last_noticed_changes ;;
        
  
  end ;;  
  

  module All_subdirectories = struct 

    let the_hashtbl = ((Hashtbl.create 10));;
    let force_get fw =  
      let details = Modularized_details.get fw in 
      Ordered.sort Total_ordering.standard (Image.image (
         fun (mn,details_on_mn) ->
          Fw_module_small_details.subdirectory(details_on_mn)
      ) details) ;;
    let get fw =
      let idx = fw.Fw_with_dependencies_t.index_for_caching in 
      match Hashtbl.find_opt the_hashtbl idx with 
      Some(l)-> l
      |None ->
       let answer = force_get fw in 
       let _ = (Hashtbl.add the_hashtbl idx answer) in 
       answer ;;
    let trsp fwm opt_g = transpose (the_hashtbl,get) fwm opt_g ;;   
    (*
    let usual_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
      )) ;;
    *)  
    let passive_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
        old_dep_val
      )) ;; 
    
    let empty_one = trsp Needed_libs.empty_one (Some(fun new_fw old_dep_val arg ad -> 
        []
      ));;
    let forget_modules = trsp Needed_libs.forget_modules None ;;
    let inspect_and_update = trsp Needed_libs.inspect_and_update None ;;
    let of_concrete_object = trsp Needed_libs.of_concrete_object None ;;
    let of_configuration = trsp Needed_libs.of_configuration None ;;
    let of_configuration_and_list = trsp Needed_libs.of_configuration_and_list None ;;
    let overwrite_file_if_it_exists = trsp Needed_libs.overwrite_file_if_it_exists None ;;
    let register_rootless_paths = trsp Needed_libs.register_rootless_paths 
        (Some(fun new_fw old_dep_val arg ad -> 
          let (_,novelties) = ad in 
          let possibly_new = Ordered.sort Total_ordering.standard 
            (Image.image (fun (rl,dets)->Dfn_rootless.to_subdirectory rl  ) novelties) in 
          Ordered.merge Total_ordering.standard possibly_new old_dep_val 
        ));;  
    let relocate_module_to = trsp Needed_libs.relocate_module_to None ;;
    let remove_files = trsp Needed_libs.remove_files None ;;
    let rename_module_on_filename_level_and_in_files = passive_trsp Needed_libs.rename_module_on_filename_level_and_in_files;;  
    let rename_subdirectory_as = trsp Needed_libs.rename_subdirectory_as (Some(fun new_fw old_dep_val arg ad -> 
        let rep = (fun sdir ->
          match Dfa_subdirectory.soak arg sdir with 
          None -> sdir 
          |Some new_sdir -> new_sdir   
        ) in 
        (Image.image rep old_dep_val)
      ));;
    let replace_string = trsp Needed_libs.replace_string None ;;
    let replace_value = trsp Needed_libs.replace_value None ;;
    let set_gitpush_after_backup = passive_trsp Needed_libs.set_gitpush_after_backup ;;
    let set_last_noticed_changes = passive_trsp Needed_libs.set_last_noticed_changes ;;
        
  
  end ;;  
  

module All_printables = struct 

  let the_hashtbl = ((Hashtbl.create 10));;
  let force_get fw =  
    let mods_without_subdirs = Option.filter_and_unpack (
       fun (mn,details) ->
        if Fw_module_small_details.has_printer details
        then Some mn 
        else None  
    ) (Modularized_details.get fw) 
    and main_table = Modularized_details.get fw in 
    Image.image (
       fun mn ->
         let details = List.assoc mn main_table in 
         let subdir = Fw_module_small_details.subdirectory details in 
         Dfn_join.subdirectory_to_module subdir mn 
    ) mods_without_subdirs;;
  let get fw =
    let idx = fw.Fw_with_dependencies_t.index_for_caching in 
    match Hashtbl.find_opt the_hashtbl idx with 
    Some(l)-> l
    |None ->
     let answer = force_get fw in 
     let _ = (Hashtbl.add the_hashtbl idx answer) in 
     answer ;;
  let trsp fwm opt_g = transpose (the_hashtbl,get) fwm opt_g ;;   
  (*
  let usual_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
    )) ;;
  *)  
  let passive_trsp fw = trsp fw (Some(fun new_fw old_dep_val arg ad -> 
      old_dep_val
    )) ;; 
  
    let empty_one = trsp All_subdirectories.empty_one (Some(fun new_fw old_dep_val arg ad -> 
      []
    ));;
    let forget_modules = trsp All_subdirectories.forget_modules (Some(fun new_fw old_dep_val arg ad -> 
      List.filter (fun middle->not(List.mem (Dfn_middle.to_module middle) arg)) old_dep_val
    ));;;;
    let inspect_and_update = trsp All_subdirectories.inspect_and_update None ;;
    let of_concrete_object = trsp All_subdirectories.of_concrete_object None ;;
    let of_configuration = trsp All_subdirectories.of_configuration None ;;
    let of_configuration_and_list = trsp All_subdirectories.of_configuration_and_list None ;;
    let overwrite_file_if_it_exists = trsp All_subdirectories.overwrite_file_if_it_exists None ;;
    let register_rootless_paths = trsp All_subdirectories.register_rootless_paths None ;;  
    let relocate_module_to = trsp All_subdirectories.relocate_module_to None ;;
    let remove_files = trsp All_subdirectories.remove_files None ;;
    let rename_module_on_filename_level_and_in_files = trsp All_subdirectories.rename_module_on_filename_level_and_in_files
    (Some(fun new_fw old_dep_val arg ad -> 
      let (old_mname,new_mname,_) = arg in
      let rep = (fun mn->if mn = old_mname then new_mname else mn) in 
      Image.image rep old_dep_val
    ));;  
    let rename_subdirectory_as = passive_trsp All_subdirectories.rename_subdirectory_as ;;
    let replace_string = trsp All_subdirectories.replace_string None ;;
    let replace_value = trsp All_subdirectories.replace_value None ;;
    let set_gitpush_after_backup = passive_trsp All_subdirectories.set_gitpush_after_backup ;;
    let set_last_noticed_changes = passive_trsp All_subdirectories.set_last_noticed_changes ;;
      

  end ;;  

  
  module Exit = struct
     
  
    let empty_one = extract_constructor All_printables.empty_one  ;;
    let forget_modules = extract_univariate All_printables.forget_modules ;;
    let inspect_and_update = extract_zerovariate_producer All_printables.inspect_and_update  ;;
    let of_concrete_object = extract_constructor All_printables.of_concrete_object  ;;
    let of_configuration = extract_constructor All_printables.of_configuration  ;;
    let of_configuration_and_list = extract_constructor All_printables.of_configuration_and_list  ;;
    let overwrite_file_if_it_exists = extract_univariate_producer All_printables.overwrite_file_if_it_exists ;;
    let register_rootless_paths = extract_univariate_producer All_printables.register_rootless_paths ;;
    let relocate_module_to = extract_univariate_producer All_printables.relocate_module_to ;;
    let remove_files = extract_univariate_producer All_printables.remove_files ;;
    let rename_module_on_filename_level_and_in_files = extract_univariate_producer All_printables.rename_module_on_filename_level_and_in_files ;;
    let rename_subdirectory_as = extract_univariate_producer All_printables.rename_subdirectory_as ;;
    let replace_string = extract_univariate_producer All_printables.replace_string ;;
    let replace_value = extract_univariate_producer All_printables.replace_value ;;
    let set_gitpush_after_backup = extract_univariate All_printables.set_gitpush_after_backup ;;
    let set_last_noticed_changes = extract_univariate All_printables.set_last_noticed_changes ;;
  

  end ;;  
  
  let details_for_module  fw mn = List.assoc mn (Modularized_details.get fw) ;;

end;;

let all_subdirectories fw = Private.All_subdirectories.get fw;;
let ancestors_for_module fw mn = snd (List.assoc mn (Private.Order.get fw)) ;;
let dep_ordered_modules fw = Image.image fst (Private.Order.get fw);;
let direct_fathers_for_module fw mn = fst (List.assoc mn (Private.Order.get fw)) ;;
let empty_one = Private.Exit.empty_one ;;
let forget_modules = Private.Exit.forget_modules ;;
let inspect_and_update = Private.Exit.inspect_and_update ;;
let mli_mt_for_module fw mn = match Fw_module_small_details.opt_mli_modification_time (Private.details_for_module fw mn) with 
                              None -> "0." |Some(fl)->fl ;;
let mli_presence_for_module fw mn = Fw_module_small_details.mli_present (Private.details_for_module fw mn) ;;
let needed_dirs_for_module fw mn = List.assoc mn (Private.Needed_dirs.get fw) ;;
let needed_libs_for_module fw mn = List.assoc mn (Private.Needed_libs.get fw) ;;
let of_concrete_object = Private.Exit.of_concrete_object ;;
let of_configuration = Private.Exit.of_configuration ;;
let of_configuration_and_list = Private.Exit.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.Exit.overwrite_file_if_it_exists ;;
let principal_ending_for_module fw mn = Fw_module_small_details.principal_ending (Private.details_for_module fw mn) ;;
let principal_mt_for_module fw mn = Fw_module_small_details.principal_modification_time (Private.details_for_module fw mn) ;;
let printer_equipped_types fw = Private.All_printables.get fw;;
let register_rootless_paths = Private.Exit.register_rootless_paths ;;
let relocate_module_to = Private.Exit.relocate_module_to ;;
let remove_files = Private.Exit.remove_files ;;
let rename_module_on_filename_level_and_in_files = Private.Exit.rename_module_on_filename_level_and_in_files ;;
let rename_subdirectory_as = Private.Exit.rename_subdirectory_as ;;
let replace_string = Private.Exit.replace_string ;;
let replace_value = Private.Exit.replace_value ;;
let set_gitpush_after_backup = Private.Exit.set_gitpush_after_backup ;;
let set_last_noticed_changes = Private.Exit.set_last_noticed_changes ;;
let subdir_for_module fw mn = Fw_module_small_details.subdirectory (Private.details_for_module fw mn) ;;


  

