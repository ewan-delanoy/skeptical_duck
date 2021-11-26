(* 

#use "Compilation_management/commands_for_batch_compilation.ml";;

*)


module Private = struct

let needed_dirs_and_libs_in_command cmod fw mn=
  let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
  let s_root=Dfa_root.connectable_to_subpath(Fw_with_dependencies.root fw) in
  let dirs=
  "-I "^s_root^(Dfa_subdirectory.connectable_to_subpath(Compilation_mode.workspace cmod))
 and libs=String.concat(" ")
   (Image.image(fun z->Ocaml_library.file_for_library(z)^extension)
   (Fw_with_dependencies.needed_libs_for_module fw mn)) in
   String.concat " " ["";dirs;libs;""];;

end ;;  


(*
exception Unregistered_cmi of Dfn_endingless_t.t;;
exception Unregistered_cmo of Dfn_endingless_t.t;;
*)
let command_for_cmi (cmod:Compilation_mode_t.t) dir fw hm=
    let nm=Dfn_endingless.to_module hm in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_fhm=Dfn_endingless.to_line hm in
    let mli_reg=Fw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli  nm in
    let ending=(if mli_reg then ".mli" else ".ml") in
    let workdir = Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod ) in 
    let opt_exec_move=(if (cmod=Compilation_mode_t.Executable)&&(not(mli_reg)) 
                       then Some("mv "^s_fhm^".o "^s_root^workdir) 
                       else None) in 
    let central_cmd=
        (Compilation_mode.executioner cmod)^
        (Private.needed_dirs_and_libs_in_command cmod fw nm)^
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
            Option.add_element_on_the_right almost_full_answer opt_exec_move;;
   
  let command_for_cmo (cmod:Compilation_mode_t.t) dir fw eless=
    let nm=Dfn_endingless.to_module eless in
    let s_root=Dfa_root.connectable_to_subpath(dir) in
    let s_eless=Dfn_endingless.to_line eless in
    let dir_and_libs=Private.needed_dirs_and_libs_in_command cmod fw nm in
    let mli_reg=Fw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm in 
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
    then 
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
                  ] 
    else central_cmds)
    in Option.add_element_on_the_right almost_full_answer opt_exec_move;; 

exception  Unregistered_element of Dfn_endingless_t.t;;   

let command_for_module_separate_compilation cmod fw eless=
    let dir = Fw_with_dependencies.root fw in 
    let nm=Dfn_endingless.to_module eless in
    let mli_reg=Fw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Mli nm
    and ml_reg=Fw_with_dependencies.check_ending_on_module fw Dfa_ocaml_ending_t.Ml nm in
    let temp2=(
    let co=command_for_cmo cmod dir fw eless in 
    if mli_reg
    then let ci=command_for_cmi cmod dir fw eless in 
         if ml_reg
         then [ci;co]
         else [ci]
    else [co]) in 
    List.flatten temp2;;

exception  Command_for_predebuggable_or_preexecutable_exn;;

let command_for_predebuggable fw short_path=
    let root = Fw_with_dependencies.root fw in 
    let s_root=Dfa_root.connectable_to_subpath root  in
    let cmod = Compilation_mode_t.Debug in 
    let full_path=Absolute_path.of_string(
        s_root^short_path) in 
    let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
    let nm_deps = Fw_with_dependencies.modules_with_their_ancestors fw nm_direct_deps in 
    let nm_deps_with_subdirs = Image.image (
       fun nm->
               let subdir=Fw_with_dependencies.subdir_for_module fw nm in 
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
     (fun (_,nm) -> Set_of_polys.sort(Fw_with_dependencies.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
    let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let extension=".cma" in
    let libs=String.concat(" ")
      (Image.image(fun z->Ocaml_library.file_for_library(z)^extension) pre_libs2) in 
    Option.add_element_on_the_right   
    [ 
      (Compilation_mode.executioner cmod)^
      " -I "^s_root^workdir^" "^
      libs^" -c "^s_root^unpointed_short_path^".ml";
    ] 
    (Unix_command.mv (s_root^unpointed_short_path^".cm*") (s_root^workdir) )
    ;;          




exception  Command_for_debuggable_or_executable_exn;;

let command_for_debuggable_or_executable cmod fw rootless_path=
  let root = Fw_with_dependencies.root fw in 
  let s_root=Dfa_root.connectable_to_subpath root  in
    if cmod=Compilation_mode_t.Usual then raise(Command_for_debuggable_or_executable_exn) else 
    let full_path=Absolute_path.of_string(s_root^rootless_path) in 
    let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
    let nm_deps =Fw_with_dependencies.modules_with_their_ancestors fw nm_direct_deps in 
    let nm_deps_with_subdirs = Image.image (
       fun nm->let subdir=Fw_with_dependencies.subdir_for_module fw nm in 
        (subdir,nm)
    ) nm_deps in 
    let workdir=
      (Dfa_subdirectory.connectable_to_subpath (Compilation_mode.workspace cmod)) 
    and ending=Compilation_mode.ending_for_nonlast_module cmod 
    and last_ending=Compilation_mode.ending_for_last_module cmod 
    and product_ending=Compilation_mode.ending_for_final_product cmod  in
    let cm_elements_but_the_last = Image.image (
      fun (subdir,nm)->(Dfa_module.to_line nm)^ending
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
     (fun (_,nm) -> Set_of_polys.sort(Fw_with_dependencies.needed_libs_for_module fw nm)) nm_deps_with_subdirs in
    let pre_libs2=Set_of_polys.forget_order (Set_of_polys.fold_merge (libs_for_prow::pre_libs1)) in 
    let extension=(if cmod=Compilation_mode_t.Executable then ".cmxa" else ".cma") in
    let libs=String.concat(" ")
      (Image.image(fun z->Ocaml_library.file_for_library(z)^extension) pre_libs2) in 
    Option.add_element_on_the_right  
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




