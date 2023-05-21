(* 

#use"lib/Filewatching/gw_with_batch_compilation.ml";;

*)

(* Beginning of POR(Polymorphic Ocaml Record)-related code *)

module Background = struct 

let fw_space = Por_space_example.filewatching ;; 

let last_compilation_result_for_module_field =
  {Por_field_t.field_name = "last_compilation_result_for_module";
  field_type = "(Dfa_module_t.t * bool) list";
  var_name = "compilation_results"; 
  default_value = "[]";
  crobj_converters =
   Some
    ("Crobj_converter_combinator.to_pair_list Dfa_module.of_concrete_object Crobj_converter.bool_of_concrete_object",
     "Crobj_converter_combinator.of_pair_list Dfa_module.to_concrete_object Crobj_converter.bool_to_concrete_object");
  }
;;

let gw_with_batch_compilation_extension =
  {Por_subclass_t.subclass_name = "gw_with_batch_compilation";
  subclass_fields = [last_compilation_result_for_module_field];
  parent = Some "gw_with_dependencies"; 
  extensions_leading_here = [];
  has_restriction = false; 
  has_constructor = false} ;;
  
Por_space.add_extension fw_space "gw_with_dependencies" gw_with_batch_compilation_extension;;  

end ;;  

(* End of POR(Polymorphic Ocaml Record)-related code *)


exception Rename_string_or_value_exn of string ;;

module Private = struct 
  
  let ocamldebug_printersfile_path root= 
             (Dfa_root.connectable_to_subpath root)^
             (Dfa_subdirectory.connectable_to_subpath
               (Coma_constant.nongithubbed_nonml_files_subdir)) ^
               "cmos_for_ocamldebug.txt";;
  let parent fw = Gw_poly.parent fw;;
  let get_cmpl_results fw = Gw_poly.last_compilation_result_for_module fw ;;
  let set_cmpl_results fw new_list = 
    Gw_poly.set_last_compilation_result_for_module fw new_list ;; 
  let set_parent fw new_parent = 
      Gw_poly.set_parent ~child:fw ~new_parent:new_parent ;;
  let usual_extension fw cmpl_results =
      Gw_poly.extend_gw_with_dependencies_to_gw_with_batch_compilation    
       fw ~last_compilation_result_for_module:cmpl_results ;;
     
  let last_compilation_result_for_module fw mn = 
    List.assoc mn (get_cmpl_results fw) ;;

  let modules_with_their_ancestors fw l=
    Gw_with_dependencies.modules_with_their_ancestors
     (parent fw) l ;;
  let root fw = Gw_poly.root (parent fw) ;;   
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
      let root = root fw  in 
      Image.image (fun middle->
       let mn = Dfn_middle.to_module middle in 
       (Dfn_join.root_to_middle root middle,last_compilation_result_for_module fw mn)
      ) (Gw_with_dependencies.printer_equipped_types fw) ;;  
  
  module Curcuma = struct 
    
  
module Private = struct

  let needed_dirs_and_libs_in_command cmod fw mn=
    let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
    let s_root=Dfa_root.connectable_to_subpath(Gw_with_dependencies.root fw) in
    let dirs=
    "-I "^s_root^(Dfa_subdirectory.connectable_to_subpath(Compilation_mode.workspace cmod))
   and libs=String.concat(" ")
     (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
     (Gw_with_dependencies.needed_libs_for_module fw mn)) in
     String.concat " " ["";dirs;libs;""];;
  
  
  let command_for_cmi (cmod:Compilation_mode_t.t) dir fw hm=
      let nm=Dfn_endingless.to_module hm in
      let s_root=Dfa_root.connectable_to_subpath(dir) in
      let s_fhm=Dfn_endingless.to_line hm in
      let mli_reg=Gw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli  nm in
      let ending=(if mli_reg then ".mli" else ".ml") in
      let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if (cmod=Compilation_mode_t.Executable)&&(not(mli_reg)) 
                         then Some("mv "^s_fhm^".o "^s_root^workdir) 
                         else None) in 
      let central_cmd=
          (Compilation_mode.executioner cmod)^
          (needed_dirs_and_libs_in_command cmod fw nm)^
              " -c "^s_fhm^ending in
              let full_mli=s_fhm^".mli" in
              let almost_full_answer=(
              if (not mli_reg)
                 &&(Sys.file_exists(full_mli))
              then (* 
                     in this situation the mli file exists but is not registered.
                     So the compilation manager must treat it as though it didn't
                     exist. We temporarily rename it so that ocamlc will ignore it.
                    *)
                    let dummy_mli=s_root^"uvueaoqhkt" in
                    [
                     "mv "^full_mli^" "^dummy_mli;
                     central_cmd;
                     "mv "^s_fhm^".cm* "^s_root^workdir;
                     "mv "^dummy_mli^" "^full_mli
                    ] 
              else  [
                       central_cmd;
                       "mv "^s_fhm^".cm* "^s_root^workdir
                     ]
              ) in 
              Option_again.argument_on_the_right (fun x y->x@[y]) almost_full_answer opt_exec_move;;
     
    let hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds =
       (* 
          in this situation the mli file exists but is not registered.
          So the compilation manager must treat it as though it didn't
          exist. We temporarily rename it so that ocamlc will ignore it.
        *)
        let dummy_mli=s_root^"uvueaoqhkt" in
        [
          "mv "^full_mli^" "^dummy_mli
        ]
        @ 
          central_cmds
        @ 
        [ 
          "mv "^dummy_mli^" "^full_mli
        ] ;;



    let command_for_cmo_from_mll (cmod:Compilation_mode_t.t) dir fw eless=
      let nm=Dfn_endingless.to_module eless in
      let s_root=Dfa_root.connectable_to_subpath(dir) in
      let s_eless=Dfn_endingless.to_line eless in
      let dir_and_libs=needed_dirs_and_libs_in_command cmod fw nm in
      let mli_reg=Gw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in 
      let full_mli=s_eless^".mli" in
      let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if cmod=Compilation_mode_t.Executable 
                         then Some("mv "^s_eless^".o "^s_root^workdir) 
                         else None) in 
      let ml_in_workplace = s_root^workdir ^ (Dfa_module.to_line nm) ^ ".ml" in                   
      let central_cmds=
      [ 
        "ocamllex "^s_eless^".mll";
        "mv "^s_eless^".ml "^s_root^workdir;
        (Compilation_mode.executioner cmod)^dir_and_libs^" -c "^ml_in_workplace;
      ] in 
      let almost_full_answer= 
      (if (not mli_reg) &&(Sys.file_exists(full_mli))
      then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds 
      else central_cmds)
      in Option_again.argument_on_the_right (fun x y->x@[y]) almost_full_answer opt_exec_move;; 

    let command_for_cmo_from_mly (cmod:Compilation_mode_t.t) dir fw eless=
      let nm=Dfn_endingless.to_module eless in
      let s_root=Dfa_root.connectable_to_subpath(dir) in
      let s_eless=Dfn_endingless.to_line eless in
      let dir_and_libs=needed_dirs_and_libs_in_command cmod fw nm in
      let mli_reg=Gw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in 
      let full_mli=s_eless^".mli" in
      let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if cmod=Compilation_mode_t.Executable 
                         then Some("mv "^s_eless^".o "^s_root^workdir) 
                         else None) in 
      let ml_in_workplace = s_root^workdir ^ (Dfa_module.to_line nm) ^ ".ml" in                   
      let central_cmds=
      [ 
        "ocamlyacc "^s_eless^".mly";
        "mv "^s_eless^".ml "^s_root^workdir;
        (Compilation_mode.executioner cmod)^dir_and_libs^" -c "^ml_in_workplace;
      ] in 
      let almost_full_answer= 
      (if (not mli_reg) &&(Sys.file_exists(full_mli))
      then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds 
      else central_cmds)
      in Option_again.argument_on_the_right (fun x y->x@[y]) almost_full_answer opt_exec_move;; 



    let command_for_cmo (cmod:Compilation_mode_t.t) dir fw eless=
      let nm=Dfn_endingless.to_module eless in
      let s_root=Dfa_root.connectable_to_subpath(dir) in
      let s_eless=Dfn_endingless.to_line eless in
      let dir_and_libs=needed_dirs_and_libs_in_command cmod fw nm in
      let mli_reg=Gw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in 
      let full_mli=s_eless^".mli" in
      let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
      let opt_exec_move=(if cmod=Compilation_mode_t.Executable 
                         then Some("mv "^s_eless^".o "^s_root^workdir) 
                         else None) in 
      let central_cmds=
      [ 
        (Compilation_mode.executioner cmod)^dir_and_libs^" -c "^s_eless^".ml";
        "mv "^s_eless^".cm* "^s_root^workdir
      ] in 
      let almost_full_answer= 
      (if (not mli_reg) &&(Sys.file_exists(full_mli))
      then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds
      else central_cmds)
      in Option_again.argument_on_the_right (fun x y->x@[y]) almost_full_answer opt_exec_move;; 


  


  let command_for_mli_module_separate_compilation cmod fw eless =
      let dir = Gw_with_dependencies.root fw in 
      command_for_cmi cmod dir fw eless;;

  let command_for_ml_module_separate_compilation cmod fw eless =
      let dir = Gw_with_dependencies.root fw in 
      let nm=Dfn_endingless.to_module eless in
      let mli_reg=Gw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in
      let temp2=(
      let co=command_for_cmo cmod dir fw eless in 
      if mli_reg
      then let ci=command_for_cmi cmod dir fw eless in 
           [ci;co]
      else [co]) in 
      List.flatten temp2;;

  let command_for_mll_module_separate_compilation cmod fw eless =
      let dir = Gw_with_dependencies.root fw in 
      let nm=Dfn_endingless.to_module eless in
      let mli_reg=Gw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in
      let temp2=(
      let co=command_for_cmo_from_mll cmod dir fw eless in 
      if mli_reg
      then let ci=command_for_cmi cmod dir fw eless in 
           [ci;co]
      else [co]) in 
      List.flatten temp2;;

  let command_for_mly_module_separate_compilation cmod fw eless =
      let dir = Gw_with_dependencies.root fw in 
      let nm=Dfn_endingless.to_module eless in
      let mli_reg=Gw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in
      let temp2=(
      let co=command_for_cmo_from_mly cmod dir fw eless in 
      if mli_reg
      then let ci=command_for_cmi cmod dir fw eless in 
           [ci;co]
      else [co]) in 
      List.flatten temp2;;

  let command_for_module_separate_compilation cmod fw eless pr_ending = 
     match pr_ending with 
      Dfa_ocaml_ending_t.Mli -> command_for_mli_module_separate_compilation cmod fw eless
     |Dfa_ocaml_ending_t.Ml -> command_for_ml_module_separate_compilation cmod fw eless
     |Dfa_ocaml_ending_t.Mll -> command_for_mll_module_separate_compilation cmod fw eless
     |Dfa_ocaml_ending_t.Mly -> command_for_mly_module_separate_compilation cmod fw eless
    ;;


  
  exception  Command_for_predebuggable_or_preexecutable_exn;;
  
  let command_for_predebuggable fw short_path=
      let root = Gw_with_dependencies.root fw in 
      let s_root=Dfa_root.connectable_to_subpath root  in
      let cmod = Compilation_mode_t.Debug in 
      let full_path=Absolute_path.of_string(
          s_root^short_path) in 
      let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
      let nm_deps = Gw_with_dependencies.modules_with_their_ancestors fw nm_direct_deps in 
      let nm_deps_with_subdirs = Image.image (
         fun nm->
                 let subdir=Gw_with_dependencies.subdir_for_module fw nm in 
          (subdir,nm)
      ) nm_deps in 
      let workdir=
        (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) in
      let unpointed_short_path = Cull_string.before_rightmost short_path '.' in 
      let libs_for_prow = 
        Set_of_polys.sort(
        Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
          (Image.image Dfa_module.to_line nm_direct_deps)) in 
      let pre_libs1=Image.image 
       (fun (_,nm) -> Set_of_polys.sort(Gw_with_dependencies.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
      let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
      let extension=".cma" in
      let libs=String.concat(" ")
        (Image.image(fun z->Ocaml_library.file_for_library(z)^extension) pre_libs2) in 
        Option_again.argument_on_the_right (fun x y->x@[y])  
      [ 
        (Compilation_mode.executioner cmod)^
        " -I "^s_root^workdir^" "^
        libs^" -c "^s_root^unpointed_short_path^".ml";
      ] 
      (Unix_command.mv (s_root^unpointed_short_path^".cm*") (s_root^workdir) )
      ;;          
  
  
  
  
  exception  Command_for_debuggable_or_executable_exn;;
  
  let command_for_debuggable_or_executable cmod fw rootless_path=
    let root = Gw_with_dependencies.root fw in 
    let s_root=Dfa_root.connectable_to_subpath root  in
      if cmod=Compilation_mode_t.Usual then raise(Command_for_debuggable_or_executable_exn) else 
      let full_path=Absolute_path.of_string(s_root^rootless_path) in 
      let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
      let nm_deps =Gw_with_dependencies.modules_with_their_ancestors fw nm_direct_deps in 
      let nm_deps_with_subdirs = Image.image (
         fun nm->let subdir=Gw_with_dependencies.subdir_for_module fw nm in 
          (subdir,nm)
      ) nm_deps in 
      let workdir=
        (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) 
      and ending=Compilation_mode.ending_for_nonlast_module cmod 
      and last_ending=Compilation_mode.ending_for_last_module cmod 
      and product_ending=Compilation_mode.ending_for_final_product cmod  in
      let cm_elements_but_the_last = Image.image (
        fun (_subdir,nm)->(Dfa_module.to_line nm)^ending
      ) nm_deps_with_subdirs in 
      let unpointed_short_path = Cull_string.before_rightmost rootless_path '.' in 
      let nm_name = (Cull_string.after_rightmost unpointed_short_path '/') in 
      let last_cm_element=nm_name^last_ending in 
      let all_cm_elements= cm_elements_but_the_last @ [last_cm_element] in 
      let libs_for_prow = 
        Set_of_polys.sort(
        Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
          (Image.image Dfa_module.to_line nm_direct_deps)) in 
      let pre_libs1=Image.image 
       (fun (_,nm) -> Set_of_polys.sort(Gw_with_dependencies.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
      let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
      let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
      let libs=String.concat(" ")
        (Image.image(fun z->Ocaml_library.file_for_library(z)^extension) pre_libs2) in 
        Option_again.argument_on_the_right (fun x y->x@[y])  
      [ 
        ((Compilation_mode.executioner cmod)^
         " -I "^s_root^workdir^" "^
         libs^" -o "^nm_name^product_ending^
          (String.concat " " all_cm_elements));
      ]
      (
        Unix_command.mv ((Sys.getcwd())^"/"^nm_name^product_ending) (s_root^workdir)
      )
      ;;          
  
  end ;; 
  
let module_separate_compilation = Private.command_for_module_separate_compilation ;;
     
let predebuggable = Private.command_for_predebuggable ;; 
     
let debuggable_or_executable  = Private.command_for_debuggable_or_executable ;;   
  
    

  end ;;  
  
  module Command = struct 
  
      let module_separate_compilation cmod fw eless pr_ending=
       Curcuma.module_separate_compilation 
         cmod (parent fw) eless pr_ending;;
      
      let predebuggable fw short_path =
        Curcuma.predebuggable 
          (parent fw) short_path ;; 
      
      let debuggable_or_executable cmod fw rootless_path =
        Curcuma.debuggable_or_executable
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
                  List.mem nm (Gw_with_dependencies.ancestors_for_module fw nm2)
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
      fun mn -> let eless = Gw_with_dependencies.endingless_at_module fw mn in 
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
     Compilation_mode_t.Usual->Option.get opt_modnames
     |Compilation_mode_t.Debug
     |Compilation_mode_t.Executable->let rootless_path=Option.get opt_rootless_path in 
         let full_path=Absolute_path.of_string(
          (Dfa_root.connectable_to_subpath (root fw))^rootless_path) in 
         let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
         let nm_deps=modules_with_their_ancestors fw nm_direct_deps in 
         let deps =List.filter (fun mn->List.mem mn nm_deps) (Gw_with_dependencies.dep_ordered_modules fw) in 
         let _=(if cmod = Compilation_mode_t.Debug 
                then prepare_pretty_printers_for_ocamldebug fw deps) in 
         deps;;
  
  let list_of_commands_for_shaft_part_of_feydeau cmod fw (opt_modulenames,opt_rootless_path)=
     let l=dependencies_inside_shaft cmod fw (opt_modulenames,opt_rootless_path) in 
     let temp1=Image.image (fun mn->
       let eless=Gw_with_dependencies.endingless_at_module fw mn 
       and pr_ending = Gw_with_dependencies.principal_ending_for_module fw mn in 
       let cmds=Command.module_separate_compilation cmod fw eless pr_ending in 
      Image.image (fun cmd->(mn,Gw_with_dependencies.endingless_at_module fw mn,cmd) ) cmds ) l in 
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
    let s_root=Dfa_root.connectable_to_subpath(root fw) in
    let s_debug_dir=s_root^(Dfa_subdirectory.connectable_to_subpath
       (Coma_constant.debug_build_subdir)) in 
    Unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
     
  let name_element_for_debugged_file = "debugged" ;;
  let debugged_file_path = 
    (Dfa_subdirectory.connectable_to_subpath(Coma_constant.debugging_subdir))
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
    let s_root=Dfa_root.connectable_to_subpath(root fw) in
    let s_exec_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.exec_build_subdir)) in 
    Unix_command.uc("rm -f "^s_exec_dir^"*.cm*"^" "^s_exec_dir^"*.ocaml_executable"^" "^s_exec_dir^"*.o");;
     
  
  let start_executing fw short_path=
    let  _=clean_exec_dir fw in
    let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau 
      Compilation_mode_t.Executable fw short_path in 
    Unix_command.conditional_multiple_uc cmds;;   
  
  let list_values_from_module fw mn = Gw_with_dependencies.list_values_from_module (parent fw) mn ;;
  let show_value_occurrences fw mn = Gw_with_dependencies.show_value_occurrences (parent fw) mn ;;

  let up_to_date_elesses fw =
    List.filter_map (
      fun mn->
        if last_compilation_result_for_module fw mn
        then Some(Gw_with_dependencies.endingless_at_module fw mn)
        else None
    )(Gw_with_dependencies.dep_ordered_modules fw);;

  let number_of_modules fw = Gw_with_dependencies.number_of_modules (parent fw) ;;  
  
  
  let modern_recompile fw changed_modules_in_any_order = 
      if changed_modules_in_any_order=[] then fw else
      let (all_deps,new_deps,_changed_modules) = 
        Gw_with_dependencies.below_several (parent fw) changed_modules_in_any_order in     
      let _ = Strung.announce 
      ~trailer:("The following modules need to be recompiled \n"^
      "because they depend on directly changed modules :")
         ~printer:Dfa_module.to_line ~items:new_deps 
         ~separator: ", " in 
      let (fw2,_,_)=
        Ocaml_target_making.usual_feydeau fw all_deps in 
      fw2 ;;

   let forget_modules fw mod_names=
      let (new_parent,removed_files) = Gw_with_dependencies.forget_modules (parent fw) mod_names in  
     let temp1 = Image.image Dfa_module.to_line mod_names in 
     let temp2 = Cartesian.product temp1 [".cm*";".d.cm*";".caml_debuggable"] in 
     let s_root=Dfa_root.connectable_to_subpath(root fw) in
     let s_build_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.usual_build_subdir)) in 
     let _=Image.image
                      (fun (mname,edg)->
                       let cmd="rm -f "^s_build_dir^mname^edg in
                       Unix_command.uc(cmd))
                      temp2 in
      (set_parent fw new_parent,removed_files);;
   
   let remove_files fw rootless_paths=
      let (new_parent,_)=Gw_with_dependencies.remove_files (parent fw) rootless_paths in   
      set_parent fw new_parent ;;   
   
   let inspect_and_update fw =
      let (new_parent,((_changed_archived_compilables,changed_usual_compilables),_,changed_files))
         =Gw_with_dependencies.inspect_and_update (parent fw) in   
      (set_parent fw new_parent,(changed_usual_compilables,changed_files));;

   let of_gw_with_dependencies fw = usual_extension fw 
    (Image.image (
        fun mn -> (mn,false)
    ) (Gw_with_dependencies.dep_ordered_modules fw));;   

   let of_configuration config =
      let root = Gw_poly.root config in 
      let _=(Unix_again.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) in 
      let initial_parent = Gw_with_dependencies.of_configuration config in 
      let fw = of_gw_with_dependencies initial_parent in 
      let mods = Gw_with_dependencies.dep_ordered_modules initial_parent in 
      let (fw2,_rejected_pairs,accepted_pairs) = Ocaml_target_making.usual_feydeau fw mods in 
        let cmpl_results = Image.image (
             fun mn -> (mn,List.exists (fun (mn2,_)->mn2 = mn) accepted_pairs)
           ) mods in 
      set_cmpl_results fw2 cmpl_results ;; 
   
   let register_rootless_paths fw rps=
      let (new_parent,((_ac_paths,uc_paths,_nc_paths),_))=
       Gw_with_dependencies.register_rootless_paths (parent fw) rps in   
      let old_list_of_cmpl_results= get_cmpl_results fw in 
     let new_list_of_cmpl_results = Image.image (
        fun mn -> 
          match List.assoc_opt mn old_list_of_cmpl_results with 
          None -> (mn,false)
          |Some(old_res) -> (mn,old_res)
     ) (Gw_with_dependencies.dep_ordered_modules new_parent) in 
     let fw2 = usual_extension new_parent new_list_of_cmpl_results in 
     let unordered_mods = Image.image Dfn_rootless.to_module uc_paths in  
     modern_recompile  fw2 unordered_mods;;
   
  
   let relocate_module_to fw mod_name new_subdir=
      let (new_parent,replacements)=Gw_with_dependencies.relocate_module_to (parent fw) (mod_name,new_subdir) in   
      (set_parent fw new_parent,replacements) ;;
   
   let rename_module fw old_middle_name new_nonslashed_name=
     let old_nm=Dfn_middle.to_module old_middle_name in
     let new_nm=Dfa_module.of_line (No_slashes.to_string new_nonslashed_name) in  
     let old_parent = parent fw in 
     let separated_acolytes_below=List.filter_map(
       fun mn->
        if List.mem old_nm (Gw_with_dependencies.ancestors_for_module old_parent mn)
       then Some(Image.image (Dfn_full.to_rootless) (Gw_with_dependencies.acolytes_at_module old_parent mn))
       else None
   ) (Gw_with_dependencies.dep_ordered_modules old_parent) in
     let all_acolytes_below=List.flatten separated_acolytes_below in
     let (new_parent,changes) = Gw_with_dependencies.rename_module_on_filename_level_and_in_files 
      old_parent (old_nm,new_nm,all_acolytes_below) in 
    let old_list_of_cmpl_results= get_cmpl_results fw in 
    let new_list_of_cmpl_results = Image.image (
         fun old_pair -> 
           let (mn,_cmpl_result) = old_pair in 
           if mn = old_nm 
           then (new_nm,false)
           else old_pair    
      ) old_list_of_cmpl_results in   
      let fw2 = usual_extension new_parent new_list_of_cmpl_results in 
      let root_dir=root fw in 
      let s_root=Dfa_root.connectable_to_subpath root_dir in   
      let s_build_dir=Dfa_subdirectory.connectable_to_subpath (Coma_constant.usual_build_subdir) in  
      let _=Unix_command.uc
             ("rm -f "^s_root^s_build_dir^
             (Dfa_module.to_line old_nm)^
             ".cm* ") in            
      let fw3=modern_recompile fw2 [new_nm] in 
      (fw3,changes) ;;
      
   
   let rename_subdirectory_as fw (old_subdir,new_subdir)=
      let (new_parent,extra)=Gw_with_dependencies.rename_subdirectory_as 
         (parent fw) (old_subdir,new_subdir) in   
         (set_parent fw new_parent,extra) ;;
   
    let replace_string fw old_s new_s =
         let old_parent = parent fw in 
         let (new_parent,(changes1,all_changed_files)) = Gw_with_dependencies.replace_string old_parent (old_s,new_s) in 
         let changed_rootlesses = Image.image fst changes1 in 
         let changed_modules_in_any_order = Image.image Dfn_rootless.to_module changed_rootlesses in 
         (set_parent fw new_parent,(changed_modules_in_any_order,all_changed_files));; 
    
    
    let replace_value fw ((preceding_files,path),(old_v,new_v)) =
          let old_parent = parent fw in 
          let (new_parent,(u_changes,all_changes)) = Gw_with_dependencies.replace_value old_parent ((preceding_files,path),(old_v,new_v)) in 
          let changed_rootlesses = Image.image fst u_changes in 
          let changed_modules_in_any_order = Image.image Dfn_rootless.to_module changed_rootlesses in 
          (set_parent fw new_parent,(changed_modules_in_any_order,all_changes));;      


     

  let plunge_gw_configuration config = 
    usual_extension (Gw_with_dependencies.plunge_gw_configuration config) [];;

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
let list_values_from_module = Private.list_values_from_module ;;
let modern_recompile = Private.modern_recompile ;;
let number_of_modules = Private.number_of_modules ;;
let of_configuration = Private.of_configuration ;;
let of_gw_with_dependencies = Private.of_gw_with_dependencies ;;
let plunge_gw_configuration = Private.plunge_gw_configuration ;;
let preq_types_with_extra_info = Private.preq_types_with_extra_info ;;
let register_rootless_paths = Private.register_rootless_paths ;;
let relocate_module_to = Private.relocate_module_to ;;
let remove_files = Private.remove_files ;;
let rename_module = Private.rename_module ;;
let rename_subdirectory_as = Private.rename_subdirectory_as ;;
let replace_string = Private.replace_string ;;
let replace_value = Private.replace_value ;;
let root = Private.root ;;
let show_value_occurrences = Private.show_value_occurrences ;;
let start_debugging = Private.start_debugging;;
let start_executing = Private.start_executing ;;
let up_to_date_elesses = Private.up_to_date_elesses ;;
let usual_batch = Private.Ocaml_target_making.usual_feydeau ;; 
let usual_recompile = Private.usual_recompile ;;  
  
  
  