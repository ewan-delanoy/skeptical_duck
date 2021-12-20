(* 

#use "Compilation_management/coma_state.ml";;

*)

module Private = struct

let qarent cs = {
   Fw_with_batch_compilation_t.parent = cs.Coma_state_t.frontier_with_unix_world ;
   last_compilation_result_for_module = cs.Coma_state_t.last_compilation_result_for_module;
} ;;

let tneraq fw = {
   Coma_state_t.frontier_with_unix_world = fw.Fw_with_batch_compilation_t.parent ;
   last_compilation_result_for_module = fw.Fw_with_batch_compilation_t.last_compilation_result_for_module ;
} ;;

let below cs mn = Fw_with_batch_compilation.below (qarent cs) mn ;;
let default_constructor = tneraq ;;
let directly_below cs mn = Fw_with_batch_compilation.directly_below (qarent cs) mn ;;
let root cs = Fw_with_batch_compilation.root (qarent cs) ;;

let usual_batch cs modnames = 
  let (new_parent,rejected_ones,accepted_ones) = Fw_with_batch_compilation.usual_batch (qarent cs) modnames in 
  (tneraq new_parent,rejected_ones,accepted_ones) ;; 


  let salt = "Coma_"^"state_field.";;
  
  let frontier_with_unix_world_label      = salt ^ "frontier_with_unix_world";;
  let last_compilation_result_for_module_label = salt ^ "last_compilation_result_for_module";;
  
  let cr_of_pair f l= Crobj_converter_combinator.of_pair_list  Dfa_module.to_concrete_object f l;;
  let cr_to_pair f crobj= Crobj_converter_combinator.to_pair_list  Dfa_module.of_concrete_object f crobj;;
  

  let of_concrete_object ccrt_obj = 
     let g=Concrete_object.get_record ccrt_obj in
     {
        Coma_state_t.frontier_with_unix_world = Fw_with_dependencies.of_concrete_object (g frontier_with_unix_world_label);
        last_compilation_result_for_module = cr_to_pair Crobj_converter.bool_of_concrete_object (g last_compilation_result_for_module_label);
     };; 
  
  let to_concrete_object cs=
     let items= 
     [
      frontier_with_unix_world_label, Fw_with_dependencies.to_concrete_object cs.Coma_state_t.frontier_with_unix_world;
      last_compilation_result_for_module_label, cr_of_pair Crobj_converter.bool_to_concrete_object cs.Coma_state_t.last_compilation_result_for_module;    
     ]  in
     Concrete_object_t.Record items;;
  
    


  
  let empty_one config=
    {
      Coma_state_t.frontier_with_unix_world= Fw_with_dependencies.empty_one config;
      last_compilation_result_for_module = [];
    };;
  
  

let read_persistent_version x=
  let full_path=Dfn_join.root_to_rootless (root x)  Coma_constant.rootless_path_for_targetfile in
  let ap= Dfn_full.to_absolute_path full_path in
  let the_archive=Io.read_whole_file ap in
  let archived_object = Crobj_parsing.parse the_archive in 
  of_concrete_object archived_object;;      


end ;; 

let all_endinglesses cs = Fw_with_batch_compilation.all_endinglesses (Private.qarent cs) ;;
let all_ml_absolute_paths cs = Fw_with_batch_compilation.all_ml_absolute_paths (Private.qarent cs) ;;
let all_mlx_files cs = Fw_with_batch_compilation.all_mlx_files (Private.qarent cs) ;;
let all_subdirectories cs = Fw_with_batch_compilation.all_subdirectories (Private.qarent cs) ;;
let ancestors_for_module cs mn = Fw_with_batch_compilation.ancestors_for_module (Private.qarent cs) mn ;;
let below cs mn = Fw_with_batch_compilation.below (Private.qarent cs) mn ;;
let census_of_foreigners cs = Fw_with_batch_compilation.census_of_foreigners (Private.qarent cs) ;;
let check_module_sequence_for_forgettability cs = Fw_with_batch_compilation.check_module_sequence_for_forgettability (Private.qarent cs) ;;
let check_that_no_change_has_occurred cs =
  Fw_with_batch_compilation.check_that_no_change_has_occurred (Private.qarent cs) ;; 
let clean_debug_dir cs = Fw_with_batch_compilation.clean_debug_dir (Private.qarent cs) ;;
let clean_exec_dir cs = Fw_with_batch_compilation.clean_exec_dir (Private.qarent cs) ;;
let configuration cs= Fw_with_batch_compilation.configuration (Private.qarent cs) ;;
let dep_ordered_modules cs mn = Fw_with_batch_compilation.dep_ordered_modules (Private.qarent cs) ;;
let direct_fathers_for_module cs mn = Fw_with_batch_compilation.direct_fathers_for_module (Private.qarent cs) mn ;;
let directly_below cs mn = Fw_with_batch_compilation.directly_below (Private.qarent cs) mn ;;
let duplicate_module cs  vague_mname1 vague_mname2 = Fw_with_batch_compilation.duplicate_module (Private.qarent cs) vague_mname1 vague_mname2 ;;
let empty_one = Private.empty_one ;;
let endingless_at_module cs mn = Fw_with_batch_compilation.endingless_at_module (Private.qarent cs) mn ;;
let find_subdir_from_suffix cs = Fw_with_batch_compilation.find_subdir_from_suffix (Private.qarent cs) ;;
let forget_modules cs mods = 
  let new_parent = Fw_with_batch_compilation.forget_modules (Private.qarent cs) mods in 
  Private.tneraq new_parent ;; 
let gitpush_after_backup cs=(configuration cs).Fw_configuration_t.gitpush_after_backup;;   
let inspect_and_update cs  = 
    let (new_parent,changed_usual_compilables) = Fw_with_batch_compilation.inspect_and_update (Private.qarent cs)  in 
    (Private.tneraq new_parent,changed_usual_compilables) ;;   
let latest_changes fw = Fw_with_batch_compilation.latest_changes (Private.qarent fw)  ;;      
let list_values_from_module cs mn = 
  Fw_with_batch_compilation.list_values_from_module  (Private.qarent cs) mn ;;
let modern_recompile cs changed_modules_in_any_order = 
  let new_parent = Fw_with_batch_compilation.modern_recompile (Private.qarent cs) changed_modules_in_any_order in 
  Private.tneraq new_parent ;; 
let modules_using_value cs module_name =
    Fw_with_batch_compilation.modules_using_value (Private.qarent cs) module_name ;;  
let number_of_modules fw = Fw_with_batch_compilation.number_of_modules (Private.qarent fw) ;;    
let of_configuration config = 
    let new_parent = Fw_with_batch_compilation.of_configuration config in 
    Private.tneraq new_parent ;;   
let of_concrete_object = Private.of_concrete_object ;;  
let of_fw_with_batch_compilation = Private.tneraq ;;    
let preq_types_with_extra_info cs = 
  Fw_with_batch_compilation.preq_types_with_extra_info (Private.qarent cs) ;; 
let read_persistent_version = Private.read_persistent_version ;;
let reflect_latest_changes_in_github cs opt_msg=
    let new_parent = Fw_with_batch_compilation.reflect_latest_changes_in_github (Private.qarent cs) opt_msg in 
    Private.tneraq new_parent ;;   
let register_rootless_paths cs mod_names = 
    let new_parent = Fw_with_batch_compilation.register_rootless_paths (Private.qarent cs) mod_names in 
    Private.tneraq new_parent ;;      
let relocate_module_to cs mod_name new_subdir = 
    let new_parent = Fw_with_batch_compilation.relocate_module_to (Private.qarent cs) mod_name new_subdir in 
    Private.tneraq new_parent ;;  
let remove_files cs rps = 
    let new_parent = Fw_with_batch_compilation.remove_files (Private.qarent cs) rps in 
    Private.tneraq new_parent ;;        
let rename_module cs old_middle_name new_nonslashed_name = 
    let (new_parent,extra) = Fw_with_batch_compilation.rename_module (Private.qarent cs) old_middle_name new_nonslashed_name in 
    (Private.tneraq new_parent,extra) ;;   
let rename_string_or_value cs old_sov new_sov = 
    let (new_parent,changed_modules_in_any_order) = Fw_with_batch_compilation.rename_string_or_value (Private.qarent cs) old_sov new_sov  in 
    (Private.tneraq new_parent,changed_modules_in_any_order) ;; 
let rename_subdirectory_as cs (old_subdir,new_subdir) = 
    let new_parent = Fw_with_batch_compilation.rename_subdirectory_as (Private.qarent cs) (old_subdir,new_subdir) in 
    Private.tneraq new_parent ;;      
let root = Private.root ;;
let set_gitpush_after_backup cs bowl = 
  let new_parent = Fw_with_batch_compilation.set_gitpush_after_backup (Private.qarent cs) bowl in 
  Private.tneraq new_parent ;;   
let show_value_occurrences cs t = 
  Fw_with_batch_compilation.show_value_occurrences  (Private.qarent cs) t ;;  
let start_debugging cs = Fw_with_batch_compilation.start_debugging (Private.qarent cs) ;; 
let start_executing cs short_path = Fw_with_batch_compilation.start_executing (Private.qarent cs) short_path;;  
let to_concrete_object = Private.to_concrete_object ;;
let up_to_date_elesses cs = 
  Fw_with_batch_compilation.up_to_date_elesses (Private.qarent cs) ;; 

  