(* 

#use"Compilation_management/fw_with_batch_compilation.ml";;

*)


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
       ()
  
  
  
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
  
  end ;;
  
let clean_debug_dir = Private.clean_debug_dir;;
let clean_exec_dir = Private.clean_exec_dir;;
let of_concrete_object = Private.of_concrete_object ;;
let start_debugging = Private.start_debugging;;
let start_executing = Private.start_executing ;;
let to_concrete_object = Private.to_concrete_object ;;  
let usual_batch = Private.Ocaml_target_making.usual_feydeau ;;
  
  
  
  
  