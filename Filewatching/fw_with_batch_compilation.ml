(* 

#use"Filewatching/fw_with_batch_compilation.ml";;

*)

   
exception Rename_string_or_value_exn of string ;;

module Private = struct 
  
  let ocamldebug_printersfile_path root= 
             (Dfa_root.connectable_to_subpath root)^
             (Dfa_subdirectory.connectable_to_subpath(Coma_constant.utility_files_subdir)) ^
               "cmos_for_ocamldebug.txt";;
  let parent fw = fw.Fw_with_batch_compilation_t.parent ;;
  let get_cmpl_results fw = fw.Fw_with_batch_compilation_t.last_compilation_result_for_module ;;
  let set_cmpl_results fw new_list = {fw with 
    Fw_with_batch_compilation_t.last_compilation_result_for_module = new_list ;
  } ;; 
  let set_parent fw new_parent = {fw with 
    Fw_with_batch_compilation_t.parent = new_parent ;
  } ;;  

  let above fw mn = Fw_with_dependencies.above (parent fw) mn ;;
  let ancestors_for_module fw mn = Fw_with_dependencies.ancestors_for_module (parent fw) mn ;;
  let dep_ordered_modules fw = Fw_with_dependencies.dep_ordered_modules (parent fw) ;;
  let printer_equipped_types fw = Fw_with_dependencies.printer_equipped_types (parent fw) ;;   
  let last_compilation_result_for_module fw mn = 
    List.assoc mn (get_cmpl_results fw) ;;
  let modules_with_their_ancestors fw l=
    Fw_with_dependencies.modules_with_their_ancestors
     (parent fw) l ;;
  let root fw = Fw_with_dependencies.root (parent fw) ;;   
  let set_cmpl_result_at_module fw mn0 new_res = 
    let old_list_of_cmpl_results= get_cmpl_results fw in 
    let new_list_of_cmpl_results = Image.image (
      fun old_pair ->
        let mn = fst old_pair in  
        if mn = mn0 
        then (mn,new_res)
        else old_pair  
    ) old_list_of_cmpl_results in 
    set_cmpl_results fw new_list_of_cmpl_results ;;
  let subdir_for_module fw mn= Fw_with_dependencies.subdir_for_module (parent fw) mn ;;  
  
  let endingless_at_module fw mn=
    Dfn_endingless_t.J(root fw,subdir_for_module fw mn,mn);;
  
  
  let preq_types_with_extra_info fw =
      let root = root fw  in 
      Image.image (fun middle->
       let mn = Dfn_middle.to_module middle in 
       (Dfn_join.root_to_middle root middle,last_compilation_result_for_module fw mn)
      ) (printer_equipped_types fw) ;;

  let all_subdirectories fw = Fw_with_dependencies.all_subdirectories (parent fw) ;;    
  
  let salt = "Fw_"^"with_batch_compilation.";;
    
  let frontier_with_unix_world_label      = salt ^ "frontier_with_unix_world";;
  let last_compilation_result_for_module_label = salt ^ "last_compilation_result_for_module";;
      
  let cr_of_pair f l= Crobj_converter_combinator.of_pair_list  Dfa_module.to_concrete_object f l;;
  let cr_to_pair f crobj= Crobj_converter_combinator.to_pair_list  Dfa_module.of_concrete_object f crobj;;
      
    
  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in
    {
      Fw_with_batch_compilation_t.parent = Fw_with_dependencies.of_concrete_object (g frontier_with_unix_world_label);
      last_compilation_result_for_module = cr_to_pair Crobj_converter.bool_of_concrete_object (g last_compilation_result_for_module_label);
    };; 
      
  let to_concrete_object fw=
    let items= 
    [
      frontier_with_unix_world_label, Fw_with_dependencies.to_concrete_object fw.Fw_with_batch_compilation_t.parent;
      last_compilation_result_for_module_label, cr_of_pair Crobj_converter.bool_to_concrete_object fw.Fw_with_batch_compilation_t.last_compilation_result_for_module;    
    ]  in
    Concrete_object_t.Record items;;
  
  
  module Command = struct 
  
      let module_separate_compilation cmod fw eless =
       Commands_for_batch_compilation.module_separate_compilation 
         cmod (parent fw) eless;;
      
      let predebuggable fw short_path =
        Commands_for_batch_compilation.predebuggable 
          (parent fw) short_path ;; 
      
      let debuggable_or_executable cmod fw rootless_path =
        Commands_for_batch_compilation.debuggable_or_executable
          cmod (parent fw) rootless_path ;; 
  end;;
  
  
  module Ocaml_target_making=struct
  
  exception Failed_during_compilation of (Dfa_module_t.t*Dfn_endingless_t.t*string);;
  
  let rec helper_for_feydeau  (cmod:Compilation_mode_t.t) fw (rejected,treated,to_be_treated)=
       match to_be_treated with 
       []->(fw,rejected,List.rev treated)
       |triple::other_triples->
         let (nm,eless,cmd)=triple in
         if (Unix_command.uc cmd)=0
         then let fw2= set_cmpl_result_at_module fw nm true in 
              helper_for_feydeau cmod fw2 (rejected,(nm,eless)::treated,other_triples)
         else if (cmod<>Compilation_mode_t.Usual)
              then raise(Failed_during_compilation(triple))
              else 
              let triples_after=snd(Prepared.partition_in_two_parts (fun (nm2,_,_)->nm2<>nm) other_triples) in 
              let (rejected_siblings_as_triples,survivors)=List.partition
             (
                fun (nm2,_,_)->
                  List.mem nm (ancestors_for_module fw nm2)
             ) triples_after in 
             let rejected_siblings_with_redundancies =  
                Image.image (fun (nm2,eless2,_)->(nm2,eless2) ) rejected_siblings_as_triples in 
             let rejected_siblings = Listennou.nonredundant_version rejected_siblings_with_redundancies in    
             let newly_rejected = (nm,eless)::rejected_siblings in 
             let newly_rejected_mods = Image.image fst newly_rejected in 
             let old_list_of_cmpl_results= get_cmpl_results fw in 
             let new_list_of_cmpl_results = Image.image (
                fun old_pair ->
                  let mn = fst old_pair in  
                  if List.mem mn newly_rejected_mods
                  then (mn,false)
                  else old_pair  
             ) old_list_of_cmpl_results in 
             let fw2 = set_cmpl_results fw new_list_of_cmpl_results in 
             helper_for_feydeau cmod fw2 (rejected@newly_rejected,treated,survivors) ;;
           
  
  let prepare_pretty_printers_for_ocamldebug fw deps = 
    let temp1 = "load_printer str.cma"::(Image.image (fun mname->
      let s= Dfa_module.to_line mname in 
      "load_printer "^s^".cmo"
    ) deps) 
    and printer_equipped_types = preq_types_with_extra_info fw  in 
    let printable_deps = List.filter (
      fun mn -> let eless = endingless_at_module fw mn in 
      List.mem (eless,true) printer_equipped_types
    ) deps in 
    let temp2 = Image.image (fun mname->
      let s= Dfa_module.to_line mname in 
      "install_printer "^(String.capitalize_ascii s)^".print_out"
    ) printable_deps in 
    let full_text = String.concat "\n" (temp1@temp2) in 
    let ppodbg_path = ocamldebug_printersfile_path (root fw) in 
    Io.overwrite_with (Absolute_path.of_string ppodbg_path) full_text;;
  
  let dependencies_inside_shaft cmod fw (opt_modnames,opt_rootless_path)=
     match cmod with 
     Compilation_mode_t.Usual->Option.unpack opt_modnames
     |_->let rootless_path=Option.unpack opt_rootless_path in 
         let full_path=Absolute_path.of_string(
          (Dfa_root.connectable_to_subpath (root fw))^rootless_path) in 
         let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
         let nm_deps=modules_with_their_ancestors fw nm_direct_deps in 
         let deps =List.filter (fun mn->List.mem mn nm_deps) (dep_ordered_modules fw) in 
         let _=(if cmod = Compilation_mode_t.Debug 
                then prepare_pretty_printers_for_ocamldebug fw deps) in 
         deps;;
  
  let list_of_commands_for_shaft_part_of_feydeau cmod fw (opt_modulenames,opt_rootless_path)=
     let l=dependencies_inside_shaft cmod fw (opt_modulenames,opt_rootless_path) in 
     let temp1=Image.image (fun mn->
       let eless=endingless_at_module fw mn in 
       let cmds=Command.module_separate_compilation cmod fw eless in 
      Image.image (fun cmd->(mn,endingless_at_module fw mn,cmd) ) cmds ) l in 
      List.flatten temp1;;
  
  let list_of_commands_for_connecting_part_of_feydeau cmod fw (_,opt_rootless_path)=
     let cmds=(
     match cmod with 
      Compilation_mode_t.Usual
     |Compilation_mode_t.Executable ->[] 
     |_->
        let rootless_path=Option.unpack opt_rootless_path in 
        Command.predebuggable fw rootless_path) in 
     cmds;;
  
  
  let list_of_commands_for_end_part_of_feydeau cmod fw (_,opt_rootless_path)= 
     let cmds=(
     match cmod with 
     Compilation_mode_t.Usual->[] 
     |_->
        let rootless_path=Option.unpack opt_rootless_path in 
        Command.debuggable_or_executable cmod fw rootless_path) in 
     cmds;;   
  
  let list_of_commands_for_ternary_feydeau cmod fw short_path=
     let pair = (None,Some(short_path)) in 
     let pre_cmds1=list_of_commands_for_shaft_part_of_feydeau cmod fw pair in 
     let cmds1=Image.image (fun (_,_,cmd)->cmd) pre_cmds1
     and cmds2=list_of_commands_for_connecting_part_of_feydeau cmod fw pair
     and cmds3=list_of_commands_for_end_part_of_feydeau cmod fw pair in 
     cmds1@cmds2@cmds3;;
  
  let shaft_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path)=
    let cmds=list_of_commands_for_shaft_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path) in  
    helper_for_feydeau cmod fw ([],[],cmds);; 
  
  let end_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path)=
    match cmod with 
     Compilation_mode_t.Usual->()
     |_->
       let all_cmds=
         (list_of_commands_for_connecting_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path))@
         (list_of_commands_for_end_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path)) in 
       let _=Image.image  Unix_command.hardcore_uc all_cmds in 
       () ;;
  
  
  
  let feydeau cmod fw (opt_modnames,opt_rootless_path)=
    let answer=shaft_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path) in 
    let _=end_part_of_feydeau cmod fw (opt_modnames,opt_rootless_path) in 
    answer;; 
  
  
  let usual_feydeau fw modnames = feydeau Compilation_mode_t.Usual fw (Some(modnames),None);;
  
  end;;  
  
  
  
  
  let clean_debug_dir fw=
    let s_root=Dfa_root.connectable_to_subpath(root fw) in
    let s_debug_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.debug_build_subdir)) in 
    Unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
     
  let name_element_for_debugged_file = "debugged" ;;
  let debugged_file_path = (Dfa_subdirectory.connectable_to_subpath(Coma_constant.utility_files_subdir))
               ^ name_element_for_debugged_file ^ ".ml" ;;  
  
  let start_debugging fw=
    let  _=clean_debug_dir fw in
    let ppodbg_path = ocamldebug_printersfile_path (root fw) in 
    let _= Io.overwrite_with (Absolute_path.of_string ppodbg_path) "" in   
    let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau Compilation_mode_t.Debug fw debugged_file_path in 
    let answer=Unix_command.conditional_multiple_uc cmds in 
    let dbgbuild_path =  Dfa_subdirectory.connectable_to_subpath(Coma_constant.debug_build_subdir) in 
    let msg=(
      if answer
      then "\n\n Now, start \n\nocamldebug "^dbgbuild_path^name_element_for_debugged_file^
           ".ocaml_debuggable\n\nin another terminal.\n\n"^
           "If you need to use pretty printers, from inside ocamldebug do \n\n"^ 
           "source "^ppodbg_path^" \n\n"
      else "\n\n Something went wrong, see above. \n\n"
    ) in
    let _=(
      print_string msg;
      flush stdout
    ) in
    answer;;   
     
  let clean_exec_dir fw=
    let s_root=Dfa_root.connectable_to_subpath(root fw) in
    let s_exec_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.exec_build_subdir)) in 
    Unix_command.uc("rm -f "^s_exec_dir^"*.cm*"^" "^s_exec_dir^"*.ocaml_executable"^" "^s_exec_dir^"*.o");;
     
  
  let start_executing fw short_path=
    let  _=clean_exec_dir fw in
    let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau 
      Compilation_mode_t.Executable fw short_path in 
    Unix_command.conditional_multiple_uc cmds;;   
  
  let list_values_from_module fw mn = Fw_with_dependencies.list_values_from_module (parent fw) mn ;;
  let show_value_occurrences fw mn = Fw_with_dependencies.show_value_occurrences (parent fw) mn ;;

  let up_to_date_elesses fw =
    Option.filter_and_unpack (
      fun mn->
        if last_compilation_result_for_module fw mn
        then Some(endingless_at_module fw mn)
        else None
    )(dep_ordered_modules fw);;

  let number_of_modules fw = Fw_with_dependencies.number_of_modules (parent fw) ;;  
  
  let set_gitpush_after_backup fw bowl = 
    let new_parent = Fw_with_dependencies.set_gitpush_after_backup (parent fw) bowl in 
    {fw with Fw_with_batch_compilation_t.parent = new_parent} ;; 

  
  let modern_recompile fw changed_modules_in_any_order = 
      if changed_modules_in_any_order=[] then fw else
      let (all_deps,new_deps,changed_modules) = 
        Fw_with_dependencies.below_several (parent fw) changed_modules_in_any_order in     
      let _ = Strung.announce 
      ~trailer:("The following modules need to be recompiled \n"^
      "because they depend on directly changed modules :")
         ~printer:Dfa_module.to_line ~items:new_deps 
         ~separator: ", " in 
      let (fw2,rejected_pairs,accepted_pairs)=
        Ocaml_target_making.usual_feydeau fw all_deps in 
      fw2 ;;

   let forget_modules fw mod_names=
      let new_parent = Fw_with_dependencies.forget_modules (parent fw) mod_names in  
     let temp1 = Image.image Dfa_module.to_line mod_names in 
     let temp2 = Cartesian.product temp1 [".cm*";".d.cm*";".caml_debuggable"] in 
     let _=Image.image
                      (fun (mname,edg)->
                       let cmd="rm -f _build/"^mname^edg in
                       Unix_command.uc(cmd))
                      temp2 in
      set_parent fw new_parent;;
   
   let remove_files fw rootless_paths=
      let (new_parent,_)=Fw_with_dependencies.remove_files (parent fw) rootless_paths in   
      set_parent fw new_parent ;;   
   
   let inspect_and_update fw =
      let (new_parent,((changed_archived_compilables,changed_usual_compilables),_,changed_files))
         =Fw_with_dependencies.inspect_and_update (parent fw) in   
      (set_parent fw new_parent,(changed_usual_compilables,changed_files));;

   let of_fw_with_dependencies fw = {
    Fw_with_batch_compilation_t.parent = fw ;
    last_compilation_result_for_module = Image.image (
        fun mn -> (mn,false)
    ) (Fw_with_dependencies.dep_ordered_modules fw);
   };;   

   let of_configuration config =
      let root = config.Fw_configuration_t.root in 
      let _=(More_unix.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) in 
      let initial_parent = Fw_with_dependencies.of_configuration config in 
      let fw = of_fw_with_dependencies initial_parent in 
      let mods = Fw_with_dependencies.dep_ordered_modules initial_parent in 
      let (fw2,rejected_pairs,accepted_pairs) = Ocaml_target_making.usual_feydeau fw mods in 
        let cmpl_results = Image.image (
             fun mn -> (mn,List.exists (fun (mn2,_)->mn2 = mn) accepted_pairs)
           ) mods in 
      {fw2 with Fw_with_batch_compilation_t.last_compilation_result_for_module = cmpl_results  };;
  
   
   let register_rootless_paths fw rps=
      let (new_parent,((ac_paths,uc_paths,nc_paths),_))=
       Fw_with_dependencies.register_rootless_paths (parent fw) rps in   
      let old_list_of_cmpl_results= fw.Fw_with_batch_compilation_t.last_compilation_result_for_module in 
     let new_list_of_cmpl_results = Image.image (
        fun mn -> 
          match List.assoc_opt mn old_list_of_cmpl_results with 
          None -> (mn,false)
          |Some(old_res) -> (mn,old_res)
     ) (Fw_with_dependencies.dep_ordered_modules new_parent) in 
     let fw2 = { 
      Fw_with_batch_compilation_t.parent = new_parent ;
       last_compilation_result_for_module = new_list_of_cmpl_results
     } in 
     let unordered_mods = Image.image Dfn_rootless.to_module uc_paths in  
     modern_recompile  fw2 unordered_mods;;
   
  
   let relocate_module_to fw mod_name new_subdir=
      let (new_parent,_)=Fw_with_dependencies.relocate_module_to (parent fw) (mod_name,new_subdir) in   
      set_parent fw new_parent ;;
   
   let rename_module fw old_middle_name new_nonslashed_name=
     let old_nm=Dfn_middle.to_module old_middle_name in
     let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) in  
     let old_parent = parent fw in 
     let separated_acolytes_below=Option.filter_and_unpack(
       fun mn->
        if List.mem old_nm (Fw_with_dependencies.ancestors_for_module old_parent mn)
       then Some(Image.image (Dfn_full.to_rootless) (Fw_with_dependencies.acolytes_at_module old_parent mn))
       else None
   ) (Fw_with_dependencies.dep_ordered_modules old_parent) in
     let all_acolytes_below=List.flatten separated_acolytes_below in
     let (new_parent,changes) = Fw_with_dependencies.rename_module_on_filename_level_and_in_files 
      old_parent (old_nm,new_nm,all_acolytes_below) in 
    let old_list_of_cmpl_results= fw.Fw_with_batch_compilation_t.last_compilation_result_for_module in 
    let new_list_of_cmpl_results = Image.image (
         fun old_pair -> 
           let (mn,cmpl_result) = old_pair in 
           if mn = old_nm 
           then (new_nm,false)
           else old_pair    
      ) old_list_of_cmpl_results in   
      let fw2 = { 
        Fw_with_batch_compilation_t.parent = new_parent ;
        last_compilation_result_for_module = new_list_of_cmpl_results;
      } in 
      let root_dir=root fw in 
      let s_root=Dfa_root.connectable_to_subpath root_dir in   
      let s_build_dir=Dfa_subdirectory.connectable_to_subpath (Coma_constant.usual_build_subdir) in  
      let _=Unix_command.uc
             ("rm -f "^s_root^s_build_dir^
             (Dfa_module.to_line old_nm)^
             ".cm* ") in            
      let fw3=modern_recompile fw2 [new_nm] in 
      (fw3,changes) ;;
      
    
    ;;
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=
      let (new_parent,extra)=Fw_with_dependencies.rename_subdirectory_as 
         (parent fw) (old_subdir,new_subdir) in   
         (set_parent fw new_parent,extra) ;;
   
   let rename_string_or_value fw old_sov new_sov =
      let old_parent = parent fw in 
      let (new_parent,changed_rootlesses)=(
         if not(String.contains old_sov '.')
         then let (parent1,changes1) = Fw_with_dependencies.replace_string old_parent (old_sov,new_sov) in 
              (parent1,Image.image fst changes1)
         else 
              let j=Substring.leftmost_index_of_in "." old_sov in
              if j<0 
              then raise(Rename_string_or_value_exn(old_sov))
              else let module_name=Cull_string.beginning (j-1) old_sov in
                   let endingless=Fw_with_dependencies.decipher_module old_parent  module_name 
                   and path=Fw_with_dependencies.decipher_path old_parent  module_name in 
                   let nm=Dfn_endingless.to_module endingless in
                   let pre_temp2=(Fw_with_dependencies.ancestors_for_module old_parent nm)@[nm] in
                   let temp2=Image.image (Fw_with_dependencies.endingless_at_module old_parent) pre_temp2 in
                   let preceding_files=Image.image  (fun eless2->
                        Dfn_full.to_absolute_path(Dfn_join.to_ending eless2 Dfa_ending.ml)
                   ) temp2 in
                   let (parent2,changes2) = Fw_with_dependencies.replace_value old_parent ((preceding_files,path),(old_sov,new_sov)) in 
                   (parent2,Image.image fst changes2) 
      ) in 
      let changed_modules_in_any_order = Image.image Dfn_rootless.to_module changed_rootlesses in 
      (set_parent fw new_parent,changed_modules_in_any_order);;       
  
  
   
    let replace_string fw old_s new_s =
         let old_parent = parent fw in 
         let (new_parent,changes1) = Fw_with_dependencies.replace_string old_parent (old_s,new_s) in 
         let changed_rootlesses = Image.image fst changes1 in 
         let changed_modules_in_any_order = Image.image Dfn_rootless.to_module changed_rootlesses in 
         (set_parent fw new_parent,changed_modules_in_any_order);; 
    
    
    let replace_value fw ((preceding_files,path),(old_v,new_v)) =
          let old_parent = parent fw in 
          let (new_parent,changes1) = Fw_with_dependencies.replace_value old_parent ((preceding_files,path),(old_v,new_v)) in 
          let changed_rootlesses = Image.image fst changes1 in 
          let changed_modules_in_any_order = Image.image Dfn_rootless.to_module changed_rootlesses in 
          (set_parent fw new_parent,changed_modules_in_any_order);;      


  let check_that_no_change_has_occurred fw =
        Fw_with_dependencies.check_that_no_change_has_occurred (parent fw) ;; 

        let reflect_latest_changes_in_github fw opt_msg = 
          let old_parent = parent fw in 
          let new_parent = Fw_with_dependencies.reflect_latest_changes_in_github old_parent opt_msg in 
          set_parent fw new_parent ;;       
  
  let modules_using_value fw module_name =
        Fw_with_dependencies.modules_using_value (parent fw) module_name;;       

  let latest_changes fw = Fw_with_dependencies.latest_changes (parent fw)  ;;   

  let below fw mn = Fw_with_dependencies.below (parent fw) mn ;;

  let configuration fw = Fw_with_dependencies.configuration (parent fw) ;;

  let directly_below fw mn = Fw_with_dependencies.below (parent fw) mn ;;

  let direct_fathers_for_module fw mn = Fw_with_dependencies.direct_fathers_for_module (parent fw) mn ;;

  let endingless_at_module fw mn = Fw_with_dependencies.endingless_at_module (parent fw) mn ;;

  let find_subdir_from_suffix fw suffix =
    Fw_with_dependencies.find_subdir_from_suffix (parent fw) suffix;; 

  let duplicate_module fw vague_mname1 vague_mname2 = 
    Fw_with_dependencies.duplicate_module (parent fw) vague_mname1 vague_mname2 ;;

  let dep_ordered_modules fw = Fw_with_dependencies.dep_ordered_modules (parent fw) ;;

  let all_ml_absolute_paths fw = Fw_with_dependencies.all_ml_absolute_paths (parent fw) ;;

  let all_mlx_files fw = Fw_with_dependencies.all_mlx_files (parent fw) ;;

  let all_endinglesses fw = Fw_with_dependencies.all_endinglesses (parent fw) ;;

  let census_of_foreigners fw = Fw_with_dependencies.census_of_foreigners (parent fw) ;;

  let check_module_sequence_for_forgettability fw = Fw_with_dependencies.check_module_sequence_for_forgettability (parent fw) ;;
  
  let noncompilable_files fw = Fw_with_dependencies.noncompilable_files (parent fw) ;;
  
  let usual_compilable_files fw = Fw_with_dependencies.usual_compilable_files (parent fw) ;;   

  let empty_one config = {
     Fw_with_batch_compilation_t.parent = Fw_with_dependencies.empty_one config ;
     last_compilation_result_for_module = [] ;
  } ;;

  let usual_recompile fw = 
    let (fw1,(changed_uc,changed_files)) = inspect_and_update fw  in 
    let unordered_mods = Image.image Dfn_rootless.to_module changed_uc in  
    let fw2 = modern_recompile fw1 unordered_mods  in 
    (fw2,(changed_uc,changed_files)) ;;   

  
  let decipher_module fw = Fw_with_dependencies.decipher_module (parent fw) ;;  

  let decipher_path fw = Fw_with_dependencies.decipher_path (parent fw) ;;  

  end ;;
  
let all_endinglesses = Private.all_endinglesses ;;  
let all_ml_absolute_paths = Private.all_ml_absolute_paths ;;  
let all_mlx_files = Private.all_mlx_files ;;
let all_subdirectories = Private.all_subdirectories ;;
let ancestors_for_module = Private.ancestors_for_module ;;
let below = Private.below ;;
let census_of_foreigners = Private.census_of_foreigners ;;
let check_module_sequence_for_forgettability = Private.check_module_sequence_for_forgettability ;;
let check_that_no_change_has_occurred = Private.check_that_no_change_has_occurred;;
let clean_debug_dir = Private.clean_debug_dir;;
let clean_exec_dir = Private.clean_exec_dir;;
let configuration = Private.configuration ;;
let decipher_module = Private.decipher_module ;;
let decipher_path = Private.decipher_path ;;
let dep_ordered_modules = Private.dep_ordered_modules ;;
let direct_fathers_for_module = Private.direct_fathers_for_module ;;
let directly_below = Private.directly_below ;;
let duplicate_module = Private.duplicate_module ;;
let empty_one = Private.empty_one ;;
let endingless_at_module = Private.endingless_at_module ;;
let find_subdir_from_suffix = Private.find_subdir_from_suffix ;;
let forget_modules = Private.forget_modules ;;
let inspect_and_update = Private.inspect_and_update ;;
let latest_changes = Private.latest_changes ;;
let list_values_from_module = Private.list_values_from_module ;;
let modern_recompile = Private.modern_recompile ;;
let modules_using_value = Private.modules_using_value ;;
let noncompilable_files = Private.noncompilable_files ;;  
let number_of_modules = Private.number_of_modules ;;
let of_concrete_object = Private.of_concrete_object ;;
let of_configuration = Private.of_configuration ;;
let of_fw_with_dependencies = Private.of_fw_with_dependencies ;;
let preq_types_with_extra_info = Private.preq_types_with_extra_info ;;
let reflect_latest_changes_in_github = Private.reflect_latest_changes_in_github ;;
let register_rootless_paths = Private.register_rootless_paths ;;
let relocate_module_to = Private.relocate_module_to ;;
let remove_files = Private.remove_files ;;
let rename_module = Private.rename_module ;;
let rename_string_or_value = Private.rename_string_or_value ;;
let rename_subdirectory_as = Private.rename_subdirectory_as ;;
let replace_string = Private.replace_string ;;
let replace_value = Private.replace_value ;;
let root = Private.root ;;
let set_gitpush_after_backup = Private.set_gitpush_after_backup ;;
let show_value_occurrences = Private.show_value_occurrences ;;
let start_debugging = Private.start_debugging;;
let start_executing = Private.start_executing ;;
let to_concrete_object = Private.to_concrete_object ;;  
let up_to_date_elesses = Private.up_to_date_elesses ;;
let usual_batch = Private.Ocaml_target_making.usual_feydeau ;;
let usual_compilable_files = Private.usual_compilable_files ;;  
let usual_recompile = Private.usual_recompile ;;  
  
  
  