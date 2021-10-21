(*

#use"Filewatching/fw_with_dependencies.ml";;

*)


module Private = struct

 let expand_index idx = (idx,Fw_indexer.get_state idx) ;;
 let index fw = fw.Fw_with_dependencies_t.index_for_caching ;; 
 let parent fw = fw.Fw_with_dependencies_t.parent ;;

module Cached = struct 

let empty_one config =  
 let new_parent = Fw_with_small_details.empty_one config in 
 let instance_idx = Fw_indexer.create_new_instance () in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; 

let forget_modules old_fw mods_to_be_erased =  
 let old_parent = parent old_fw in 
 let new_parent = Fw_with_small_details.forget_modules old_parent mods_to_be_erased in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; 

let inspect_and_update old_fw  =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.inspect_and_update old_parent  in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let of_concrete_object crobj =  
 let new_parent = Fw_with_small_details.of_concrete_object crobj in 
 let instance_idx = Fw_indexer.create_new_instance () in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; 

let of_configuration config =  
 let new_parent = Fw_with_small_details.of_configuration config in 
 let instance_idx = Fw_indexer.create_new_instance () in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; 

let of_configuration_and_list pair =  
 let new_parent = Fw_with_small_details.of_configuration_and_list pair in 
 let instance_idx = Fw_indexer.create_new_instance () in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; 

let overwrite_file_if_it_exists old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.overwrite_file_if_it_exists old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let reflect_latest_changes_in_github old_fw opt_msg =  
 let old_parent = parent old_fw in 
 let new_parent = Fw_with_small_details.reflect_latest_changes_in_github old_parent opt_msg in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; 

let register_rootless_paths old_fw rootlesses =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.register_rootless_paths old_parent rootlesses in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let relocate_module_to old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.relocate_module_to old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let remove_files old_fw files_to_be_removed =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.remove_files old_parent files_to_be_removed in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.rename_module_on_filename_level_and_in_files old_parent triple in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let rename_subdirectory_as old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.rename_subdirectory_as old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let replace_string old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.replace_string old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let replace_value old_fw pair =  
 let old_parent = parent old_fw in 
 let (new_parent,extra) = Fw_with_small_details.replace_value old_parent pair in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 ({ 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 },extra ) ;; 

let set_gitpush_after_backup old_fw yes_or_no =  
 let old_parent = parent old_fw in 
 let new_parent = Fw_with_small_details.set_gitpush_after_backup old_parent yes_or_no in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; 

let set_last_noticed_changes old_fw diff =  
 let old_parent = parent old_fw in 
 let new_parent = Fw_with_small_details.set_last_noticed_changes old_parent diff in 
 let instance_idx = fst( index old_fw ) in 
 let _ = Fw_indexer.push_state instance_idx in 
 { 
   Fw_with_dependencies_t.parent = new_parent ;
   index_for_caching = expand_index instance_idx ;
 } ;; end ;;


module Modularized_details = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = Fw_module_small_details.modularize_details (parent fw)
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let empty_one config =  
 let new_fw = Cached.empty_one config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let forget_modules old_fw mods_to_be_erased =  
 let new_fw = Cached.forget_modules old_fw mods_to_be_erased in 
 let old_val = get old_fw in 
 let answer = List.filter (fun (mn,_)->not(List.mem mn mods_to_be_erased)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let inspect_and_update old_fw  =  
 let visible = Cached.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let ((a_files,u_files,nc_files),changed_u_files) = extra in 
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
 let answer = Image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_concrete_object crobj =  
 let new_fw = Cached.of_concrete_object crobj in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration config =  
 let new_fw = Cached.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Cached.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Cached.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let answer = ( match extra with 
      None -> old_val 
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
 Image.image tempf old_val) in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let reflect_latest_changes_in_github old_fw opt_msg =  
 let new_fw = Cached.reflect_latest_changes_in_github old_fw opt_msg in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Cached.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
  let old_val = get old_fw in 
  let ((a_files,u_files,nc_files),new_details) = extra in
  let involved_mods = Image.image Dfn_rootless.to_module rootlesses in
  let (overlapping,nonoverlapping) = List.partition (
     fun (rl,_) -> List.mem (Dfn_rootless.to_module rl) involved_mods 
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
  let answer = (Image.image tempf1 old_val)@
  (Fw_module_small_details.compute_details_from_acolytes_list_for_several_modules nonoverlapping) in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Cached.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Dfn_rootless.to_module rl)= mn
       ) extra in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Cached.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Dfn_rootless.to_module rl)= mn
       ) extra in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Cached.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Dfn_rootless.to_module rl)= mn
       ) extra in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Cached.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Dfn_rootless.to_module rl)= mn
       ) extra in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Cached.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Dfn_rootless.to_module rl)= mn
       ) extra in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Cached.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let tempf = (
   fun old_pair -> 
     let (mn,details) = old_pair in 
     let temp1 = List.filter (fun (rl,new_pair_for_rl)->
        (Dfn_rootless.to_module rl)= mn
       ) extra in
     if temp1 <> []
     then let new_parent = parent new_fw in 
          (mn, Fw_module_small_details.recompute_module_details_from_list_of_changes new_parent mn temp1)
     else old_pair 
 ) in 
 let answer = Image.image tempf old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let set_gitpush_after_backup old_fw yes_or_no =  
 let new_fw = Cached.set_gitpush_after_backup old_fw yes_or_no in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let set_last_noticed_changes old_fw diff =  
 let new_fw = Cached.set_last_noticed_changes old_fw diff in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

end ;;


module Order = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = Fw_determine_order.main (Modularized_details.get fw)
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let empty_one config =  
 let new_fw = Modularized_details.empty_one config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let forget_modules old_fw mods_to_be_erased =  
 let new_fw = Modularized_details.forget_modules old_fw mods_to_be_erased in 
 let old_val = get old_fw in 
 let answer = List.filter (fun (mn,_)->not(List.mem mn mods_to_be_erased)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let inspect_and_update old_fw  =  
 let visible = Modularized_details.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Image.image fst old_val in 
 let details_in_old_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_concrete_object crobj =  
 let new_fw = Modularized_details.of_concrete_object crobj in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration config =  
 let new_fw = Modularized_details.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Modularized_details.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Modularized_details.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Image.image fst old_val in 
 let details_in_old_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let reflect_latest_changes_in_github old_fw opt_msg =  
 let new_fw = Modularized_details.reflect_latest_changes_in_github old_fw opt_msg in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Modularized_details.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let extended_details_list = Modularized_details.get new_fw in 
 let new_details = Listennou.big_tail (List.length old_val) extended_details_list in
 let new_modules_in_order = Image.image fst (Fw_determine_order.main new_details) in 
 let new_details_in_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
     new_details new_modules_in_order in 
 let answer = Fw_determine_order.compute_coatoms_and_ancestors_in_small_extension
      old_val new_details_in_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Modularized_details.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Modularized_details.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Modularized_details.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = (fun mn->if mn = old_mname then new_mname else mn) in  
 let answer = Image.image (fun (mn2,(coat_mn2,ancestors_mn2)) ->
     (rep mn2,(Image.image rep coat_mn2,Image.image rep ancestors_mn2))
 ) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Modularized_details.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Modularized_details.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Image.image fst old_val in 
 let details_in_old_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Modularized_details.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let modules_in_old_order = Image.image fst old_val in 
 let details_in_old_order = Ordered_misc.reorder_list_of_pairs_using_list_of_singles
 (Modularized_details.get new_fw) modules_in_old_order in 
 let answer = Fw_determine_order.main  details_in_old_order in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let set_gitpush_after_backup old_fw yes_or_no =  
 let new_fw = Modularized_details.set_gitpush_after_backup old_fw yes_or_no in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let set_last_noticed_changes old_fw diff =  
 let new_fw = Modularized_details.set_last_noticed_changes old_fw diff in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

end ;;


module Needed_dirs = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = let details = Modularized_details.get fw in 
 let subdir_at_module = (fun mn->
   Fw_module_small_details.subdirectory(List.assoc mn details)
 ) in 
 Image.image (
  fun (mn,(_,ancestors)) ->
   let temp1 = Image.image subdir_at_module (mn::ancestors) in 
   (mn,Ordered.sort Total_ordering.standard temp1)
) (Order.get fw) ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let empty_one config =  
 let new_fw = Order.empty_one config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let forget_modules old_fw mods_to_be_erased =  
 let new_fw = Order.forget_modules old_fw mods_to_be_erased in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let inspect_and_update old_fw  =  
 let visible = Order.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_concrete_object crobj =  
 let new_fw = Order.of_concrete_object crobj in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration config =  
 let new_fw = Order.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Order.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Order.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let reflect_latest_changes_in_github old_fw opt_msg =  
 let new_fw = Order.reflect_latest_changes_in_github old_fw opt_msg in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Order.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Order.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Order.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Order.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = (fun mn->if mn = old_mname then new_mname else mn) in 
 let answer = Image.image (fun (mn2,sdirs) -> (rep mn2,sdirs)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Order.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let rep = (fun sdir ->
   match Dfa_subdirectory.soak pair sdir with 
   None -> sdir 
   |Some new_sdir -> new_sdir   
 ) in 
 let answer = Image.image (fun (mn,sdirs)->(mn,Image.image rep sdirs) ) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Order.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Order.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let set_gitpush_after_backup old_fw yes_or_no =  
 let new_fw = Order.set_gitpush_after_backup old_fw yes_or_no in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let set_last_noticed_changes old_fw diff =  
 let new_fw = Order.set_last_noticed_changes old_fw diff in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

end ;;

module Needed_libs = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw = let details = Modularized_details.get fw in 
 let needed_libs_at_module = (fun mn->
   Fw_module_small_details.used_libraries(List.assoc mn details)
 ) in 
 Image.image (
  fun (mn,(_,ancestors)) ->
   let temp1 = List.flatten(Image.image needed_libs_at_module (mn::ancestors)) in 
   (mn,Ordered.sort Total_ordering.standard temp1)
) (Order.get fw) ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let empty_one config =  
 let new_fw = Needed_dirs.empty_one config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let forget_modules old_fw mods_to_be_erased =  
 let new_fw = Needed_dirs.forget_modules old_fw mods_to_be_erased in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let inspect_and_update old_fw  =  
 let visible = Needed_dirs.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_concrete_object crobj =  
 let new_fw = Needed_dirs.of_concrete_object crobj in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration config =  
 let new_fw = Needed_dirs.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Needed_dirs.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Needed_dirs.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let reflect_latest_changes_in_github old_fw opt_msg =  
 let new_fw = Needed_dirs.reflect_latest_changes_in_github old_fw opt_msg in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Needed_dirs.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Needed_dirs.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Needed_dirs.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Needed_dirs.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = (fun mn->if mn = old_mname then new_mname else mn) in 
 let answer = Image.image (fun (mn2,libs) -> (rep mn2,libs)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Needed_dirs.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Needed_dirs.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Needed_dirs.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let set_gitpush_after_backup old_fw yes_or_no =  
 let new_fw = Needed_dirs.set_gitpush_after_backup old_fw yes_or_no in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let set_last_noticed_changes old_fw diff =  
 let new_fw = Needed_dirs.set_last_noticed_changes old_fw diff in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

end ;;


module All_subdirectories = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw =  let details = Modularized_details.get fw in 
 Ordered.sort Total_ordering.standard (Image.image (
  fun (mn,details_on_mn) ->
  Fw_module_small_details.subdirectory(details_on_mn)
) details) ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let empty_one config =  
 let new_fw = Needed_libs.empty_one config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let forget_modules old_fw mods_to_be_erased =  
 let new_fw = Needed_libs.forget_modules old_fw mods_to_be_erased in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let inspect_and_update old_fw  =  
 let visible = Needed_libs.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_concrete_object crobj =  
 let new_fw = Needed_libs.of_concrete_object crobj in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration config =  
 let new_fw = Needed_libs.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = Needed_libs.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = Needed_libs.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let reflect_latest_changes_in_github old_fw opt_msg =  
 let new_fw = Needed_libs.reflect_latest_changes_in_github old_fw opt_msg in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = Needed_libs.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (_,novelties) = extra in 
 let possibly_new = Ordered.sort Total_ordering.standard 
   (Image.image (fun (rl,dets)->Dfn_rootless.to_subdirectory rl  ) novelties) in 
 let answer = Ordered.merge Total_ordering.standard possibly_new old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = Needed_libs.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = Needed_libs.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = Needed_libs.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = Needed_libs.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let rep = (fun sdir ->
   match Dfa_subdirectory.soak pair sdir with 
   None -> sdir 
   |Some new_sdir -> new_sdir   
 ) in 
 let answer = Image.image rep old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = Needed_libs.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = Needed_libs.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let set_gitpush_after_backup old_fw yes_or_no =  
 let new_fw = Needed_libs.set_gitpush_after_backup old_fw yes_or_no in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let set_last_noticed_changes old_fw diff =  
 let new_fw = Needed_libs.set_last_noticed_changes old_fw diff in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

end ;;


module All_printables = struct 

 let the_hashtbl = ((Hashtbl.create 10)) ;; 
 let force_get fw =  let mods_without_subdirs = Option.filter_and_unpack (
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
 ) mods_without_subdirs ;;
 let get fw = 
   let idx = index fw in 
   match Hashtbl.find_opt the_hashtbl idx with 
      Some(old_answer)-> old_answer 
     | None -> 
   let answer = force_get fw in 
   let _ = (Hashtbl.add the_hashtbl idx answer) in 
   answer ;; 

let empty_one config =  
 let new_fw = All_subdirectories.empty_one config in 
 let answer = [] in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let forget_modules old_fw mods_to_be_erased =  
 let new_fw = All_subdirectories.forget_modules old_fw mods_to_be_erased in 
 let old_val = get old_fw in 
 let answer = List.filter (fun middle->
    not(List.mem (Dfn_middle.to_module middle) mods_to_be_erased)) old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let inspect_and_update old_fw  =  
 let visible = All_subdirectories.inspect_and_update old_fw  in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let of_concrete_object crobj =  
 let new_fw = All_subdirectories.of_concrete_object crobj in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration config =  
 let new_fw = All_subdirectories.of_configuration config in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let of_configuration_and_list pair =  
 let new_fw = All_subdirectories.of_configuration_and_list pair in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let overwrite_file_if_it_exists old_fw pair =  
 let visible = All_subdirectories.overwrite_file_if_it_exists old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let reflect_latest_changes_in_github old_fw opt_msg =  
 let new_fw = All_subdirectories.reflect_latest_changes_in_github old_fw opt_msg in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let register_rootless_paths old_fw rootlesses =  
 let visible = All_subdirectories.register_rootless_paths old_fw rootlesses in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let relocate_module_to old_fw pair =  
 let visible = All_subdirectories.relocate_module_to old_fw pair in 
 let (new_fw,extra) = visible in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let remove_files old_fw files_to_be_removed =  
 let visible = All_subdirectories.remove_files old_fw files_to_be_removed in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_module_on_filename_level_and_in_files old_fw triple =  
 let visible = All_subdirectories.rename_module_on_filename_level_and_in_files old_fw triple in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_mname,new_mname,_) = triple in
 let rep = Dfn_middle.rename_module (old_mname,new_mname) in 
 let answer = Image.image rep old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let rename_subdirectory_as old_fw pair =  
 let visible = All_subdirectories.rename_subdirectory_as old_fw pair in 
 let (new_fw,extra) = visible in 
 let old_val = get old_fw in 
 let (old_sdir,new_sdir) = pair in
 let s_new_sdir = Dfa_subdirectory.without_trailing_slash new_sdir in 
 let rep = Dfn_middle.rename_endsubdirectory (old_sdir,s_new_sdir) in 
 let answer = Image.image rep old_val in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_string old_fw pair =  
 let visible = All_subdirectories.replace_string old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let replace_value old_fw pair =  
 let visible = All_subdirectories.replace_value old_fw pair in 
 let (new_fw,extra) = visible in 
 let answer = force_get new_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 visible ;;

let set_gitpush_after_backup old_fw yes_or_no =  
 let new_fw = All_subdirectories.set_gitpush_after_backup old_fw yes_or_no in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

let set_last_noticed_changes old_fw diff =  
 let new_fw = All_subdirectories.set_last_noticed_changes old_fw diff in 
  let answer = get old_fw in 
 let _ = Hashtbl.add the_hashtbl (index new_fw) answer in 
 new_fw ;;

end ;;



  let details_for_module  fw mn = List.assoc mn (Modularized_details.get fw) ;;
  module Exit = All_printables ;; 
end;;

let all_subdirectories fw = Private.All_subdirectories.get fw;;
let ancestors_for_module fw mn = snd (List.assoc mn (Private.Order.get fw)) ;;
let configuration fw = Fw_with_small_details.configuration (Private.parent fw) ;;
let dep_ordered_modules fw = Image.image fst (Private.Order.get fw);;
let direct_fathers_for_module fw mn = fst (List.assoc mn (Private.Order.get fw)) ;;
let empty_one = Private.Exit.empty_one ;;
let forget_modules = Private.Exit.forget_modules ;;
let get_mtime fw rl = Fw_with_small_details.get_mtime (Private.parent fw) rl ;;
let get_mtime_or_zero_if_file_is_nonregistered fw rl = Fw_with_small_details.get_mtime_or_zero_if_file_is_nonregistered (Private.parent fw) rl ;;
let inspect_and_update = Private.Exit.inspect_and_update ;;
let last_noticed_changes fw = Fw_with_small_details.last_noticed_changes (Private.parent fw) ;;
let mli_mt_for_module fw mn = match Fw_module_small_details.opt_mli_modification_time (Private.details_for_module fw mn) with 
                              None -> "0." |Some(fl)->fl ;;
let mli_presence_for_module fw mn = Fw_module_small_details.mli_present (Private.details_for_module fw mn) ;;
let needed_dirs_for_module fw mn = List.assoc mn (Private.Needed_dirs.get fw) ;;
let needed_libs_for_module fw mn = List.assoc mn (Private.Needed_libs.get fw) ;;
let noncompilable_files fw = Fw_with_small_details.noncompilable_files (Private.parent fw) ;;
let of_concrete_object = Private.Exit.of_concrete_object ;;
let of_configuration = Private.Exit.of_configuration ;;
let of_configuration_and_list = Private.Exit.of_configuration_and_list ;;
let overwrite_file_if_it_exists = Private.Exit.overwrite_file_if_it_exists ;;
let principal_ending_for_module fw mn = Fw_module_small_details.principal_ending (Private.details_for_module fw mn) ;;
let principal_mt_for_module fw mn = Fw_module_small_details.principal_modification_time (Private.details_for_module fw mn) ;;
let printer_equipped_types fw = Private.All_printables.get fw;;
let reflect_latest_changes_in_github = Private.Exit.reflect_latest_changes_in_github ;;
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
let to_concrete_object fw = Fw_with_small_details.to_concrete_object (Private.parent fw) ;;
let usual_compilable_files fw = Fw_with_small_details.usual_compilable_files (Private.parent fw) ;;



