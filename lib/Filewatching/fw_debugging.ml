(*

#use"lib/Filewatching/fw_debugging.ml";;

*)


module Private = struct 

  let name_element_for_debugged_file = "debugged" ;;
  let debugged_file_path = 
   (Dfa_subdirectory.connectable_to_subpath(Fw_constant.debugging_subdir))
              ^ name_element_for_debugged_file ^ ".ml" ;;  

  let ocamldebug_printersfile_path root= 
            (Dfa_root.connectable_to_subpath root)^
            (Dfa_subdirectory.connectable_to_subpath
              (Fw_constant.nongithubbed_nonml_files_subdir)) ^
              "cmos_for_ocamldebug.txt";;

  let clean_debug_dir fw=
   let s_root=Dfa_root.connectable_to_subpath(Fwc_with_modular_infrastructure.Inherited.root fw) in
   let s_debug_dir=s_root^(Dfa_subdirectory.connectable_to_subpath
      (Fw_constant.debug_build_subdir)) in 
   Unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;

   

   module Command = struct 
      
  let line_for_libs lib_names = 
        let extension=".cma" in
        (* Before OCaml 5.0, this used to be : *)
          String.concat(" ") (Image.image(fun z->
            let lib = Ocaml_library.file_for_library(z)in 
            "-I +"^lib^" "^lib^extension) lib_names) 
        
        (* Since OCaml 5.0 *)
        (* String.concat(" ") (Image.image(fun z->"-I +"^(Ocaml_library.file_for_library(z))) lib_names *);;
    

  let needed_dirs_and_libs_in_dbg_command fw mn=
     let s_root=Dfa_root.connectable_to_subpath(Fwc_with_modular_infrastructure.Inherited.root fw) in
     let dirs=
     "-I "^s_root^(Dfa_subdirectory.connectable_to_subpath(Fw_constant.debug_build_subdir))
    and prelibs = Fwc_with_modular_infrastructure.needed_libs_for_module fw mn in 
    let libs=line_for_libs prelibs in 
     String.concat " " ["";dirs;libs;""];;

   let cmi_in_dbg dir fw hm=
     let nm=Dfn_endingless.to_module hm in
     let s_root=Dfa_root.connectable_to_subpath(dir) in
     let s_fhm=Dfn_endingless.to_line hm in
     let mli_reg=Fwc_with_modular_infrastructure.check_ending_on_module fw Dfa_ocaml_ending_t.Mli  nm in
     let ending=(if mli_reg then ".mli" else ".ml") in
     let workdir = Dfa_subdirectory.connectable_to_subpath (Fw_constant.debug_build_subdir) in 
     let central_cmd=
         "ocamlc -g"^
         (needed_dirs_and_libs_in_dbg_command fw nm)^
             " -c "^s_fhm^ending in
     let full_mli=s_fhm^".mli" in
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
     else  
     [
        central_cmd;
        "mv "^s_fhm^".cm* "^s_root^workdir
     ];;
    
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

         let cmo_from_mll_in_dbg dir fw eless=
         let nm=Dfn_endingless.to_module eless in
         let s_root=Dfa_root.connectable_to_subpath(dir) in
         let s_eless=Dfn_endingless.to_line eless in
         let dir_and_libs=needed_dirs_and_libs_in_dbg_command fw nm in
         let mli_reg=Fwc_with_modular_infrastructure.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in 
         let full_mli=s_eless^".mli" in
         let workdir = Dfa_subdirectory.connectable_to_subpath Fw_constant.debug_build_subdir in 
         let ml_in_workplace = s_root^workdir ^ (Dfa_module.to_line nm) ^ ".ml" in                   
         let central_cmds=
         [ 
           "ocamllex "^s_eless^".mll";
           "mv "^s_eless^".ml "^s_root^workdir;
           "ocamlc -g "^dir_and_libs^" -c "^ml_in_workplace;
         ] in 
         if (not mli_reg) &&(Sys.file_exists(full_mli))
         then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds 
         else central_cmds ;; 
         
         let cmo_from_mly_in_dbg  dir fw eless=
         let nm=Dfn_endingless.to_module eless in
         let s_root=Dfa_root.connectable_to_subpath(dir) in
         let s_eless=Dfn_endingless.to_line eless in
         let dir_and_libs=needed_dirs_and_libs_in_dbg_command fw nm in
         let mli_reg=Fwc_with_modular_infrastructure.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in 
         let full_mli=s_eless^".mli" in
         let workdir = Dfa_subdirectory.connectable_to_subpath Fw_constant.debug_build_subdir in 
         let ml_in_workplace = s_root^workdir ^ (Dfa_module.to_line nm) ^ ".ml" in                   
         let central_cmds=
         [ 
           "ocamlyacc "^s_eless^".mly";
           "mv "^s_eless^".ml "^s_root^workdir;
           "ocamlc -g "^dir_and_libs^" -c "^ml_in_workplace;
         ] in 
         if (not mli_reg) &&(Sys.file_exists(full_mli))
         then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds 
         else central_cmds;; 
         
         
         
         let cmo_in_dbg dir fw eless=
         let nm=Dfn_endingless.to_module eless in
         let s_root=Dfa_root.connectable_to_subpath(dir) in
         let s_eless=Dfn_endingless.to_line eless in
         let dir_and_libs=needed_dirs_and_libs_in_dbg_command fw nm in
         let mli_reg=Fwc_with_modular_infrastructure.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in 
         let full_mli=s_eless^".mli" in
         let workdir = Dfa_subdirectory.connectable_to_subpath Fw_constant.debug_build_subdir in 
         let central_cmds=
         [ 
           "ocamlc -g "^dir_and_libs^" -c "^s_eless^".ml";
           "mv "^s_eless^".cm* "^s_root^workdir
         ] in 
         if (not mli_reg) &&(Sys.file_exists(full_mli))
         then hack_to_ignore_present_but_unregistered_mli s_root full_mli central_cmds
         else central_cmds;; 
         
         

     let mli_module_separate_compilation_in_dbg fw eless =
        let dir = Fwc_with_modular_infrastructure.Inherited.root fw in 
        cmi_in_dbg dir fw eless;;
      
      let ml_module_separate_compilation_in_dbg fw eless =
        let dir = Fwc_with_modular_infrastructure.Inherited.root fw in 
        let nm=Dfn_endingless.to_module eless in
        let mli_reg=Fwc_with_modular_infrastructure.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in
        let temp2=(
        let co=cmo_in_dbg dir fw eless in 
        if mli_reg
        then let ci=cmi_in_dbg dir fw eless in 
             [ci;co]
        else [co]) in 
        List.flatten temp2;;
      
      let mll_module_separate_compilation_in_dbg fw eless =
        let dir = Fwc_with_modular_infrastructure.Inherited.root fw in 
        let nm=Dfn_endingless.to_module eless in
        let mli_reg=Fwc_with_modular_infrastructure.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in
        let temp2=(
        let co=cmo_from_mll_in_dbg dir fw eless in 
        if mli_reg
        then let ci=cmi_in_dbg dir fw eless in 
             [ci;co]
        else [co]) in 
        List.flatten temp2;;
      
      let mly_module_separate_compilation_in_dbg fw eless =
        let dir = Fwc_with_modular_infrastructure.Inherited.root fw in 
        let nm=Dfn_endingless.to_module eless in
        let mli_reg=Fwc_with_modular_infrastructure.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in
        let temp2=(
        let co=cmo_from_mly_in_dbg dir fw eless in 
        if mli_reg
        then let ci=cmi_in_dbg dir fw eless in 
             [ci;co]
        else [co]) in 
        List.flatten temp2;;
       

     let module_separate_compilation_in_dbg fw eless pr_ending = 
        match pr_ending with 
         Dfa_ocaml_ending_t.Mli -> mli_module_separate_compilation_in_dbg fw eless
        |Dfa_ocaml_ending_t.Ml -> ml_module_separate_compilation_in_dbg fw eless
        |Dfa_ocaml_ending_t.Mll -> mll_module_separate_compilation_in_dbg fw eless
        |Dfa_ocaml_ending_t.Mly -> mly_module_separate_compilation_in_dbg fw eless
       ;;
   
     let predebuggable fw short_path=
        let root = Fwc_with_modular_infrastructure.Inherited.root fw in 
        let s_root=Dfa_root.connectable_to_subpath root  in
        let full_path=Absolute_path.of_string(
            s_root^short_path) in 
        let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
        let nm_deps = Fwc_with_modular_infrastructure.modules_with_their_ancestors fw nm_direct_deps in 
        let nm_deps_with_subdirs = Image.image (
           fun nm->
                   let subdir=Fwc_with_modular_infrastructure.subdir_for_module fw nm in 
            (subdir,nm)
        ) nm_deps in 
        let workdir=
          (Dfa_subdirectory.connectable_to_subpath (Fw_constant.debug_build_subdir)) in
        let unpointed_short_path = Cull_string.before_rightmost short_path '.' in 
        let libs_for_prow = 
          Set_of_polys.sort(
          Ocaml_library.compute_needed_libraries_from_uncapitalized_modules_list
            (Image.image Dfa_module.to_line nm_direct_deps)) in 
        let pre_libs1=Image.image 
         (fun (_,nm) -> Set_of_polys.sort(Fwc_with_modular_infrastructure.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
        let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
        let libs=line_for_libs pre_libs2 in 
          Option_again.argument_on_the_right (fun x y->x@[y])  
        [ 
          "ocamlc -g -I "^s_root^workdir^" "^
          libs^" -c "^s_root^unpointed_short_path^".ml";
        ] 
        (Unix_command.mv (s_root^unpointed_short_path^".cm*") (s_root^workdir) )
        ;;          
    

     let debuggable fw rootless_path=
        let root = Fwc_with_modular_infrastructure.Inherited.root fw in 
        let s_root=Dfa_root.connectable_to_subpath root  in
        let full_path=Absolute_path.of_string(s_root^rootless_path) in 
        let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
        let nm_deps =Fwc_with_modular_infrastructure.modules_with_their_ancestors fw nm_direct_deps in 
        let nm_deps_with_subdirs = Image.image (
          fun nm->let subdir=Fwc_with_modular_infrastructure.subdir_for_module fw nm in 
           (subdir,nm)
        ) nm_deps in 
        let workdir=
         (Dfa_subdirectory.connectable_to_subpath 
         Fw_constant.debug_build_subdir) 
        and ending=".cmo" 
        and last_ending=".cmo"
        and product_ending=".ocaml_debuggable "  in
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
        (fun (_,nm) -> Set_of_polys.sort(Fwc_with_modular_infrastructure.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
       let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
       let libs=line_for_libs pre_libs2 in 
         Option_again.argument_on_the_right (fun x y->x@[y])  
       [ 
         (
          "ocamlc -g  -I "^s_root^workdir^" "^
          libs^" -o "^nm_name^product_ending^
           (String.concat " " all_cm_elements));
       ]
       (
         Unix_command.mv ((Sys.getcwd())^"/"^nm_name^product_ending) (s_root^workdir)
       )
       ;;       


   end;;  

   module Ocaml_target_making = struct 

     let prepare_pretty_printers_for_ocamldebug fw deps = 
        let temp1 = "load_printer str.cma"::(Image.image (fun mname->
          let s= Dfa_module.to_line mname in 
          "load_printer "^s^".cmo"
        ) deps) 
        and registered_printers = Fwc_with_modular_infrastructure.registered_printers fw  in 
        let temp2 = Image.image (fun (_idx,printer_path)->
          "install_printer "^printer_path
        ) registered_printers in 
        let full_text = String.concat "\n" (temp1@temp2) in 
        let ppodbg_path = ocamldebug_printersfile_path (Fwc_with_modular_infrastructure.Inherited.root fw) in 
        Io.overwrite_with (Absolute_path.of_string ppodbg_path) full_text;;

     let dependencies_inside_shaft_of_dbg fw (_opt_modnames,opt_rootless_path)=
        let rootless_path=Option.get opt_rootless_path in 
        let full_path=Absolute_path.of_string(
         (Dfa_root.connectable_to_subpath (Fwc_with_modular_infrastructure.Inherited.root fw))^rootless_path) in 
        let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
        let nm_deps=Fwc_with_modular_infrastructure.modules_with_their_ancestors fw nm_direct_deps in 
        let deps =List.filter (fun mn->List.mem mn nm_deps) (Fwc_with_modular_infrastructure.dep_ordered_modules fw) in 
        let _= prepare_pretty_printers_for_ocamldebug fw deps in 
        deps;;

     let list_of_commands_for_shaft_part_of_dbg fw (opt_modulenames,opt_rootless_path)=
       let l=dependencies_inside_shaft_of_dbg fw (opt_modulenames,opt_rootless_path) in 
      let temp1=Image.image (fun mn->
      let eless=Fwc_with_modular_infrastructure.endingless_at_module fw mn 
      and pr_ending = Fwc_with_modular_infrastructure.principal_ending_for_module fw mn in 
      let cmds=Command.module_separate_compilation_in_dbg fw eless pr_ending in 
     Image.image (fun cmd->(mn,Fwc_with_modular_infrastructure.endingless_at_module fw mn,cmd) ) cmds ) l in 
     List.flatten temp1;;

     let list_of_commands_for_connecting_part_of_dbg fw (_,opt_rootless_path)=
       let rootless_path=Option.get opt_rootless_path in 
       Command.predebuggable fw rootless_path ;;

     let list_of_commands_for_end_part_of_dbg fw (_,opt_rootless_path)= 
       let rootless_path=Option.get opt_rootless_path in 
       Command.debuggable fw rootless_path;;   

     let list_of_commands_for_dbg fw short_path=
     let pair = (None,Some(short_path)) in 
     let pre_cmds1=list_of_commands_for_shaft_part_of_dbg fw pair in 
     let cmds1=Image.image (fun (_,_,cmd)->cmd) pre_cmds1
     and cmds2=list_of_commands_for_connecting_part_of_dbg fw pair
     and cmds3=list_of_commands_for_end_part_of_dbg fw pair in 
     cmds1@cmds2@cmds3;;  

   end ;;  

   

let start_debugging fw=
   let  _=clean_debug_dir fw in
   let ppodbg_path = ocamldebug_printersfile_path 
          (Fwc_with_modular_infrastructure.Inherited.root fw) in 
   let _= Io.overwrite_with (Absolute_path.of_string ppodbg_path) "" in   
   let cmds=Ocaml_target_making.list_of_commands_for_dbg
     fw debugged_file_path in 
   let answer=Unix_command.conditional_multiple_uc cmds in 
   let dbgbuild_path =  Dfa_subdirectory.connectable_to_subpath(Fw_constant.debug_build_subdir) in 
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

end ;;

let clean_debug_dir = Private.clean_debug_dir ;;
let start_debugging = Private.start_debugging ;;