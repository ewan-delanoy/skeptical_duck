(* 

#use"lib/Filewatching/Fw_classes/fwc_with_batch_compilation.ml";;

*)
   
type t = Fwg_with_batch_compilation.t ;;

exception Rename_string_or_value_exn of string ;;

module Inherited = struct

  module Parent = Fwc_with_dependencies ;;

  module Ancestry = Parent.Inherited ;;
  let parent = Fwg_with_batch_compilation.parent ;;

  let all_endinglesses fw = Parent.all_endinglesses (parent fw) ;;
  let all_moduled_mlx_files fw = Parent.all_moduled_mlx_files (parent fw) ;;
  let all_subdirectories fw = Parent.all_subdirectories (parent fw) ;;
  let ancestors_for_module fw = Parent.ancestors_for_module (parent fw) ;;
  let below fw = Parent.below (parent fw) ;;
 

  let check_module_sequence_for_forgettability fw =
    Parent.check_module_sequence_for_forgettability (parent fw) ;;

  let check_that_no_change_has_occurred fw = Ancestry.check_that_no_change_has_occurred(parent fw)  ;;  
  

  let decipher_module fw = Parent.decipher_module (parent fw) ;;
  let decipher_path fw = Parent.decipher_path (parent fw) ;;
  let dep_ordered_modules fw = Parent.dep_ordered_modules (parent fw) ;;
  let directly_below fw = Parent.directly_below (parent fw) ;;
  let direct_fathers_for_module fw = Parent.direct_fathers_for_module (parent fw) ;;
  let duplicate_module fw = Parent.duplicate_module (parent fw) ;;
  let endingless_at_module fw = Parent.endingless_at_module (parent fw) ;;
  let find_subdir_from_suffix fw = Parent.find_subdir_from_suffix (parent fw) ;;
  
  
  let ignored_files fw = Ancestry.ignored_files (parent fw) ;;
  let ignored_subdirectories fw = Ancestry.ignored_subdirectories (parent fw) ;;
  
  
  let  latest_changes fw = Ancestry.latest_changes(parent fw)  ;;   
  
  let list_values_from_module fw =
      Parent.list_values_from_module (parent fw) ;;
      
  let modules_using_value fw =
        Parent.modules_using_value (parent fw) ;;    
  
  let  noncompilable_files fw = Ancestry.noncompilable_files(parent fw)  ;;  
  
  
  let number_of_modules fw =
      Parent.number_of_modules (parent fw) ;;
  
  let root fw = Ancestry.root (parent fw) ;;
  
  let set_fw_with_dependencies fw fw_with_deps =
    Fwg_with_batch_compilation.make fw_with_deps 
       (Fwg_with_batch_compilation.last_compilation_result_for_module fw) ;;
  let set_parent  = set_fw_with_dependencies ;;
  
  let set_last_compilation_result fw new_lcr =
    Fwg_with_batch_compilation.make (Fwg_with_batch_compilation.parent fw)
       new_lcr ;;    
  
  let show_value_occurrences fw =
    Parent.show_value_occurrences (parent fw) ;;
    
    let test_equality fw1 fw2 = 
      (
        Parent.Inherited.test_equality (parent fw1) (parent fw2)
      )
      @
      (
        List.filter_map (fun (fld,is_ok)->if is_ok then None else Some fld)
        [
          "last_compilation_result_for_module",(
            (Fwg_with_batch_compilation.last_compilation_result_for_module fw1)=
          (Fwg_with_batch_compilation.last_compilation_result_for_module fw2))
        ]
      ) ;;
  
  let test_for_admissibility fw = Ancestry.test_for_admissibility (parent fw) ;;
  
  let to_fw_configuration fw = Ancestry.to_fw_configuration (parent fw) ;;
  
  let to_fw_with_archives fw = Ancestry.to_fw_with_archives (parent fw) ;;
  let to_fw_with_dependencies = parent ;;

  let  usual_compilable_files fw = Ancestry.usual_compilable_files(parent fw)  ;; 
  
  

end ;;  

  
module Crobj = struct 
  let salt = "Fwc_with_batch_compilation." ;;
  let label_for_parent = salt ^ "parent" ;;
  let label_for_last_compilation_result  = salt ^ "last_compilation_result" ;;
      
      
  let of_concrete_object ccrt_obj = 
    let g=Concrete_object.get_record ccrt_obj in 
    Fwg_with_batch_compilation.make 
    (Fwc_with_dependencies.Crobj.of_concrete_object (g label_for_parent))
    (Crobj_converter_combinator.to_pair_list Dfa_module.of_concrete_object Crobj_converter.bool_of_concrete_object  (g label_for_last_compilation_result))
    ;;
      
  let to_concrete_object fw = 
    let items =  
    [
         label_for_parent, Fwc_with_dependencies.Crobj.to_concrete_object ( Fwg_with_batch_compilation.parent fw ) ;
         label_for_last_compilation_result, Crobj_converter_combinator.of_pair_list Dfa_module.to_concrete_object Crobj_converter.bool_to_concrete_object 
          ( Fwg_with_batch_compilation.last_compilation_result_for_module fw ) ;
    ] in 
    Concrete_object_t.Record items ;;
      
      
end;; 

module Private = struct 



  let ocamldebug_printersfile_path root= 
             (Dfa_root.connectable_to_subpath root)^
             (Dfa_subdirectory.connectable_to_subpath
               (Coma_constant.nongithubbed_nonml_files_subdir)) ^
               "cmos_for_ocamldebug.txt";;
  let parent fw = Fwg_with_batch_compilation.parent fw;;
  let get_cmpl_results fw = Fwg_with_batch_compilation.last_compilation_result_for_module fw ;;
  let set_cmpl_results fw new_list = Inherited.set_last_compilation_result fw new_list ;; 
  
     
  let last_compilation_result_for_module fw mn = 
    List.assoc mn (get_cmpl_results fw) ;;

  let modules_with_their_ancestors fw l=
    Fwc_with_dependencies.modules_with_their_ancestors
     (parent fw) l ;;
  
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
  
  let preq_types_with_extra_info fw =
      let root = Inherited.root fw  in 
      Image.image (fun middle->
       let mn = Dfn_middle.to_module middle in 
       (Dfn_join.root_to_middle root middle,last_compilation_result_for_module fw mn)
      ) (Fwc_with_dependencies.printer_equipped_types (parent fw)) ;;  
  
  
  module Command = struct 
  
      let module_separate_compilation cmod fw eless pr_ending=
       Commands_for_batch_compilation.module_separate_compilation 
         cmod (parent fw) eless pr_ending;;
      
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
              let triples_after=snd(Hurried.partition_in_two_parts (fun (nm2,_,_)->nm2<>nm) other_triples) in 
              let (rejected_siblings_as_triples,survivors)=List.partition
             (
                fun (nm2,_,_)->
                  List.mem nm (Fwc_with_dependencies.ancestors_for_module (parent fw) nm2)
             ) triples_after in 
             let rejected_siblings_with_redundancies =  
                Image.image (fun (nm2,eless2,_)->(nm2,eless2) ) rejected_siblings_as_triples in 
             let rejected_siblings = List_again.nonredundant_version rejected_siblings_with_redundancies in    
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
      fun mn -> let eless = Fwc_with_dependencies.endingless_at_module (parent fw) mn in 
      List.mem (eless,true) printer_equipped_types
    ) deps in 
    let temp2 = Image.image (fun mname->
      let s= Dfa_module.to_line mname in 
      "install_printer "^(String.capitalize_ascii s)^".print_out"
    ) printable_deps in 
    let full_text = String.concat "\n" (temp1@temp2) in 
    let ppodbg_path = ocamldebug_printersfile_path (Inherited.root fw) in 
    Io.overwrite_with (Absolute_path.of_string ppodbg_path) full_text;;
  
  let dependencies_inside_shaft cmod fw (opt_modnames,opt_rootless_path)=
     match cmod with 
     Compilation_mode_t.Usual->Option.get opt_modnames
     |Compilation_mode_t.Debug
     |Compilation_mode_t.Executable->let rootless_path=Option.get opt_rootless_path in 
         let full_path=Absolute_path.of_string(
          (Dfa_root.connectable_to_subpath (Inherited.root fw))^rootless_path) in 
         let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
         let nm_deps=modules_with_their_ancestors fw nm_direct_deps in 
         let deps =List.filter (fun mn->List.mem mn nm_deps) (Fwc_with_dependencies.dep_ordered_modules (parent fw)) in 
         let _=(if cmod = Compilation_mode_t.Debug 
                then prepare_pretty_printers_for_ocamldebug fw deps) in 
         deps;;
  
  let list_of_commands_for_shaft_part_of_feydeau cmod fw (opt_modulenames,opt_rootless_path)=
     let pfw = parent fw in 
     let l=dependencies_inside_shaft cmod fw (opt_modulenames,opt_rootless_path) in 
     let temp1=Image.image (fun mn->
       let eless=Fwc_with_dependencies.endingless_at_module pfw mn 
       and pr_ending = Fwc_with_dependencies.principal_ending_for_module pfw mn in 
       let cmds=Command.module_separate_compilation cmod fw eless pr_ending in 
      Image.image (fun cmd->(mn,Fwc_with_dependencies.endingless_at_module pfw mn,cmd) ) cmds ) l in 
      List.flatten temp1;;
  
  let list_of_commands_for_connecting_part_of_feydeau cmod fw (_,opt_rootless_path)=
     let cmds=(
     match cmod with 
      Compilation_mode_t.Usual
     |Compilation_mode_t.Executable ->[] 
     |Compilation_mode_t.Debug->
        let rootless_path=Option.get opt_rootless_path in 
        Command.predebuggable fw rootless_path) in 
     cmds;;
  
  
  let list_of_commands_for_end_part_of_feydeau cmod fw (_,opt_rootless_path)= 
     let cmds=(
     match cmod with 
     Compilation_mode_t.Usual->[] 
     |Compilation_mode_t.Debug
     |Compilation_mode_t.Executable->
        let rootless_path=Option.get opt_rootless_path in 
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
     |Compilation_mode_t.Debug
     |Compilation_mode_t.Executable->
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
    let s_root=Dfa_root.connectable_to_subpath(Inherited.root fw) in
    let s_debug_dir=s_root^(Dfa_subdirectory.connectable_to_subpath
       (Coma_constant.debug_build_subdir)) in 
    Unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
     
  let name_element_for_debugged_file = "debugged" ;;
  let debugged_file_path = 
    (Dfa_subdirectory.connectable_to_subpath(Coma_constant.debugging_subdir))
               ^ name_element_for_debugged_file ^ ".ml" ;;  
  
  let start_debugging fw=
    let  _=clean_debug_dir fw in
    let ppodbg_path = ocamldebug_printersfile_path (Inherited.root fw) in 
    let _= Io.overwrite_with (Absolute_path.of_string ppodbg_path) "" in   
    let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau Compilation_mode_t.Debug fw debugged_file_path in 
    let answer=Unix_command.conditional_multiple_uc cmds in 
    let dbgbuild_path =  Dfa_subdirectory.connectable_to_subpath(Coma_constant.debug_build_subdir) in 
    let msg=(
      if answer
      then "\n\n The debugging-friendly executable has been created. \n"^
          "Now, go to "^dbgbuild_path^" and start \n\nocamldebug "^name_element_for_debugged_file^
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
    let s_root=Dfa_root.connectable_to_subpath(Inherited.root fw) in
    let s_exec_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.exec_build_subdir)) in 
    Unix_command.uc("rm -f "^s_exec_dir^"*.cm*"^" "^s_exec_dir^"*.ocaml_executable"^" "^s_exec_dir^"*.o");;
     
  
  let start_executing fw short_path=
    let  _=clean_exec_dir fw in
    let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau 
      Compilation_mode_t.Executable fw short_path in 
    Unix_command.conditional_multiple_uc cmds;;   
  

  let up_to_date_elesses fw =
    let pfw = parent fw in 
    List.filter_map (
      fun mn->
        if last_compilation_result_for_module fw mn
        then Some(Fwc_with_dependencies.endingless_at_module pfw mn)
        else None
    )(Fwc_with_dependencies.dep_ordered_modules pfw);;

  let number_of_modules fw = Fwc_with_dependencies.number_of_modules (parent fw) ;;  
  
  
  let modern_recompile fw changed_modules_in_any_order = 
      if changed_modules_in_any_order=[] then fw else
      let (all_deps,new_deps,_changed_modules) = 
        Fwc_with_dependencies.below_several (parent fw) changed_modules_in_any_order in     
      let _ = Strung.announce 
      ~trailer:("The following modules need to be recompiled \n"^
      "because they depend on directly changed modules :")
         ~printer:Dfa_module.to_line ~items:new_deps 
         ~separator: ", " in 
      let (fw2,_,_)=
        Ocaml_target_making.usual_feydeau fw all_deps in 
      fw2 ;;

   let forget_modules fw mod_names=
      let (new_parent,removed_files) = Fwc_with_dependencies.forget_modules (parent fw) mod_names in  
     let temp1 = Image.image Dfa_module.to_line mod_names in 
     let temp2 = Cartesian.product temp1 [".cm*";".d.cm*";".caml_debuggable"] in 
     let s_root=Dfa_root.connectable_to_subpath(Inherited.root fw) in
     let s_build_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.usual_build_subdir)) in 
     let _=Image.image
                      (fun (mname,edg)->
                       let cmd="rm -f "^s_build_dir^mname^edg in
                       Unix_command.uc(cmd))
                      temp2 in
      (Inherited.set_parent fw new_parent,removed_files);;
   
   let remove_files fw rootless_paths=
      let (new_parent,_)=Fwc_with_dependencies.remove_files (parent fw) rootless_paths in   
      Inherited.set_parent fw new_parent ;;   
   
   let inspect_and_update fw =
      let (new_parent,((_changed_archived_compilables,changed_usual_compilables),_,changed_files))
         =Fwc_with_dependencies.inspect_and_update (parent fw) in   
      (Inherited.set_parent fw new_parent,(changed_usual_compilables,changed_files));;

   let of_fw_with_dependencies fw_with_deps = 
    Fwg_with_batch_compilation.make fw_with_deps 
    (Image.image (
        fun mn -> (mn,false)
    ) (Fwc_with_dependencies.dep_ordered_modules fw_with_deps));;   

   let of_configuration config =
      let root = Fwc_configuration.root config in 
      let proj_name = Cull_string.after_rightmost (Dfa_root.without_trailing_slash root) '/' in
      let _=(Unix_again.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           (Coma_constant.conventional_files_with_minimal_content proj_name)) in 
      let initial_parent = Fwc_with_dependencies.of_configuration config in 
      let fw = of_fw_with_dependencies initial_parent in 
      let mods = Fwc_with_dependencies.dep_ordered_modules initial_parent in 
      let (fw2,_rejected_pairs,accepted_pairs) = Ocaml_target_making.usual_feydeau fw mods in 
        let cmpl_results = Image.image (
             fun mn -> (mn,List.exists (fun (mn2,_)->mn2 = mn) accepted_pairs)
           ) mods in 
      set_cmpl_results fw2 cmpl_results ;; 
   
   let register_rootless_paths fw rps=
      let (new_parent,((_ac_paths,uc_paths,_nc_paths),_))=
       Fwc_with_dependencies.register_rootless_paths (parent fw) rps in   
      let old_list_of_cmpl_results= get_cmpl_results fw in 
     let new_list_of_cmpl_results = Image.image (
        fun mn -> 
          match List.assoc_opt mn old_list_of_cmpl_results with 
          None -> (mn,false)
          |Some(old_res) -> (mn,old_res)
     ) (Fwc_with_dependencies.dep_ordered_modules new_parent) in 
     let fw2 = Fwg_with_batch_compilation.make new_parent new_list_of_cmpl_results in 
     let unordered_mods = Image.image Dfn_rootless.to_module uc_paths in  
     modern_recompile  fw2 unordered_mods;;
   
  
   let relocate_module_to fw mod_name new_subdir=
      let (new_parent,replacements)=Fwc_with_dependencies.relocate_module_to (parent fw) (mod_name,new_subdir) in   
      (Inherited.set_parent fw new_parent,replacements) ;;
   
   let rename_module fw old_middle_name new_nonslashed_name=
     let old_nm=Dfn_middle.to_module old_middle_name in
     let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) in  
     let old_parent = parent fw in 
     let separated_acolytes_below=List.filter_map(
       fun mn->
        if List.mem old_nm (Fwc_with_dependencies.ancestors_for_module old_parent mn)
       then Some(Image.image (Dfn_full.to_rootless) (Fwc_with_dependencies.acolytes_at_module old_parent mn))
       else None
   ) (Fwc_with_dependencies.dep_ordered_modules old_parent) in
     let all_acolytes_below=List.flatten separated_acolytes_below in
     let (new_parent,changes) = Fwc_with_dependencies.rename_module_on_filename_level_and_in_files 
      old_parent (old_nm,new_nm,all_acolytes_below) in 
    let old_list_of_cmpl_results= get_cmpl_results fw in 
    let new_list_of_cmpl_results = Image.image (
         fun old_pair -> 
           let (mn,_cmpl_result) = old_pair in 
           if mn = old_nm 
           then (new_nm,false)
           else old_pair    
      ) old_list_of_cmpl_results in   
      let fw2 = Fwg_with_batch_compilation.make new_parent new_list_of_cmpl_results in 
      let root_dir=Inherited.root fw in 
      let s_root=Dfa_root.connectable_to_subpath root_dir in   
      let s_build_dir=Dfa_subdirectory.connectable_to_subpath (Coma_constant.usual_build_subdir) in  
      let _=Unix_command.uc
             ("rm -f "^s_root^s_build_dir^
             (Dfa_module.to_line old_nm)^
             ".cm* ") in            
      let fw3=modern_recompile fw2 [new_nm] in 
      (fw3,changes) ;;
      
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=
      let (new_parent,extra)=Fwc_with_dependencies.rename_subdirectory_as 
         (parent fw) (old_subdir,new_subdir) in   
         (Inherited.set_parent fw new_parent,extra) ;;
   
    let replace_string fw old_s new_s =
         let old_parent = parent fw in 
         let (new_parent,(changes1,all_changed_files)) = Fwc_with_dependencies.replace_string old_parent (old_s,new_s) in 
         let changed_rootlesses = Image.image fst changes1 in 
         let changed_modules_in_any_order = Image.image Dfn_rootless.to_module changed_rootlesses in 
         (Inherited.set_parent fw new_parent,(changed_modules_in_any_order,all_changed_files));; 
    
    
    let replace_value fw ((preceding_files,path),(old_v,new_v)) =
          let old_parent = parent fw in 
          let (new_parent,(u_changes,all_changes)) = Fwc_with_dependencies.replace_value old_parent ((preceding_files,path),(old_v,new_v)) in 
          let changed_rootlesses = Image.image fst u_changes in 
          let changed_modules_in_any_order = Image.image Dfn_rootless.to_module changed_rootlesses in 
          (Inherited.set_parent fw new_parent,(changed_modules_in_any_order,all_changes));;      


     

  let plunge_fw_configuration config = 
    Fwg_with_batch_compilation.make (Fwc_with_dependencies.plunge_fw_configuration config) [];;

  let usual_recompile fw = 
    let (fw1,(changed_uc,changed_files)) = inspect_and_update fw  in 
    let fw2 = fw1 in 
    (*
    When not using dune, replace the above line with : 
    let unordered_mods = Image.image Dfn_rootless.to_module changed_uc in  
    let fw2 = modern_recompile fw1 unordered_mods  in 
    *)
    (fw2,(changed_uc,changed_files)) ;;   

  
end ;;
  

let clean_debug_dir = Private.clean_debug_dir;;
let clean_exec_dir = Private.clean_exec_dir;;
let forget_modules = Private.forget_modules ;;
let modern_recompile = Private.modern_recompile ;;
let number_of_modules = Private.number_of_modules ;;
let of_configuration = Private.of_configuration ;;
let of_fw_with_dependencies = Private.of_fw_with_dependencies ;;
let plunge_fw_configuration = Private.plunge_fw_configuration ;;
let preq_types_with_extra_info = Private.preq_types_with_extra_info ;;
let register_rootless_paths = Private.register_rootless_paths ;;
let relocate_module_to = Private.relocate_module_to ;;
let remove_files = Private.remove_files ;;
let rename_module = Private.rename_module ;;
let rename_subdirectory_as = Private.rename_subdirectory_as ;;
let replace_string = Private.replace_string ;;
let replace_value = Private.replace_value ;;
let start_debugging = Private.start_debugging;;
let start_executing = Private.start_executing ;;
let to_fw_configuration = Inherited.to_fw_configuration;;
let up_to_date_elesses = Private.up_to_date_elesses ;;
let usual_batch = Private.Ocaml_target_making.usual_feydeau ;; 
let usual_recompile = Private.usual_recompile ;;  
  
  
  