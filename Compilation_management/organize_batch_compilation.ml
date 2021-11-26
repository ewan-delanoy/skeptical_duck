(* 

#use"Compilation_management/organize_batch_compilation.ml";;

*)


module Private = struct 

  
let ocamldebug_printersfile_path root= 
           (Dfa_root.connectable_to_subpath root)^
           (Dfa_subdirectory.connectable_to_subpath(Coma_constant.utility_files_subdir)) ^
             "cmos_for_ocamldebug.txt";;
let frontier = Coma_state.Automatic.frontier_with_unix_world ;;

let modules_with_their_ancestors cs l=
  Fw_with_dependencies.modules_with_their_ancestors
   (frontier cs) l ;;

module Command = struct 

    let module_separate_compilation cmod cs eless =
     Commands_for_batch_compilation.module_separate_compilation 
       cmod (frontier cs) eless;;
    
    let predebuggable cs short_path =
      Commands_for_batch_compilation.predebuggable 
        (frontier cs) short_path ;; 
    
    let debuggable_or_executable cmod cs rootless_path =
      Commands_for_batch_compilation.debuggable_or_executable
        cmod (frontier cs) rootless_path ;; 
end;;


module Ocaml_target_making=struct

exception Failed_during_compilation of (Dfa_module_t.t*Dfn_endingless_t.t*string);;

let rec helper_for_feydeau  (cmod:Compilation_mode_t.t) cs (rejected,treated,to_be_treated)=
     match to_be_treated with 
     []->(cs,rejected,List.rev treated)
     |triple::other_triples->
       let (nm,eless,cmd)=triple in
       if (Unix_command.uc cmd)=0
       then 
            let cs2=Coma_state.set_last_compilation_result_for_module cs nm true in 
            helper_for_feydeau cmod cs2 (rejected,(nm,eless)::treated,other_triples)
       else if (cmod<>Compilation_mode_t.Usual)
            then raise(Failed_during_compilation(triple))
            else 
            let triples_after=snd(Prepared.partition_in_two_parts (fun (nm2,_,_)->nm2<>nm) other_triples) in 
            let (rejected_siblings_as_triples,survivors)=List.partition
           (
              fun (nm2,_,_)->
                List.mem nm (Coma_state.ancestors_for_module cs nm2)
           ) triples_after in 
           let rejected_siblings_with_redundancies =  
              Image.image (fun (nm2,eless2,_)->(nm2,eless2) ) rejected_siblings_as_triples in 
           let rejected_siblings = Listennou.nonredundant_version rejected_siblings_with_redundancies in    
           let newly_rejected = (nm,eless)::rejected_siblings in 
           let cs_walker=ref(cs) in 
           let _=List.iter(
              fun (nm3,hm3)->
                cs_walker:= Coma_state.set_last_compilation_result_for_module (!cs_walker) nm3 false
           ) newly_rejected in 
           helper_for_feydeau cmod (!cs_walker) (rejected@newly_rejected,treated,survivors) ;;
         

let prepare_pretty_printers_for_ocamldebug cs deps = 
  let temp1 = "load_printer str.cma"::(Image.image (fun mname->
    let s= Dfa_module.to_line mname in 
    "load_printer "^s^".cmo"
  ) deps) 
  and printer_equipped_types = Coma_state.preq_types_with_extra_info cs  in 
  let printable_deps = List.filter (
    fun mn -> let eless = Coma_state.endingless_at_module cs mn in 
    List.mem (eless,true) printer_equipped_types
  ) deps in 
  let temp2 = Image.image (fun mname->
    let s= Dfa_module.to_line mname in 
    "install_printer "^(String.capitalize_ascii s)^".print_out"
  ) printable_deps in 
  let full_text = String.concat "\n" (temp1@temp2) in 
  let ppodbg_path = ocamldebug_printersfile_path (Coma_state.root cs) in 
  Io.overwrite_with (Absolute_path.of_string ppodbg_path) full_text;;

let dependencies_inside_shaft cmod cs (opt_modnames,opt_rootless_path)=
   match cmod with 
   Compilation_mode_t.Usual->Option.unpack opt_modnames
   |_->let rootless_path=Option.unpack opt_rootless_path in 
       let full_path=Absolute_path.of_string(
        (Dfa_root.connectable_to_subpath (Coma_state.root cs))^rootless_path) in 
       let nm_direct_deps = Look_for_module_names.names_in_mlx_file full_path in 
       let nm_deps=modules_with_their_ancestors cs nm_direct_deps in 
       let deps =List.filter (fun mn->List.mem mn nm_deps) (Coma_state.dep_ordered_modules cs) in 
       let _=(if cmod = Compilation_mode_t.Debug 
              then prepare_pretty_printers_for_ocamldebug cs deps) in 
       deps;;



let list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_modulenames,opt_rootless_path)=
   let l=dependencies_inside_shaft cmod cs (opt_modulenames,opt_rootless_path) in 
   let temp1=Image.image (fun mn->
     let eless=Coma_state.endingless_at_module cs mn in 
     let cmds=Command.module_separate_compilation cmod cs eless in 
    Image.image (fun cmd->(mn,Coma_state.endingless_at_module cs mn,cmd) ) cmds ) l in 
    List.flatten temp1;;



let list_of_commands_for_connecting_part_of_feydeau cmod cs (_,opt_rootless_path)=
   let cmds=(
   match cmod with 
    Compilation_mode_t.Usual
   |Compilation_mode_t.Executable ->[] 
   |_->
      let rootless_path=Option.unpack opt_rootless_path in 
      Command.predebuggable cs rootless_path) in 
   cmds;;


let list_of_commands_for_end_part_of_feydeau cmod cs (_,opt_rootless_path)= 
   let cmds=(
   match cmod with 
   Compilation_mode_t.Usual->[] 
   |_->
      let rootless_path=Option.unpack opt_rootless_path in 
      Command.debuggable_or_executable cmod cs rootless_path) in 
   cmds;;   

let list_of_commands_for_ternary_feydeau cmod cs short_path=
   let pair = (None,Some(short_path)) in 
   let pre_cmds1=list_of_commands_for_shaft_part_of_feydeau cmod cs pair in 
   let cmds1=Image.image (fun (_,_,cmd)->cmd) pre_cmds1
   and cmds2=list_of_commands_for_connecting_part_of_feydeau cmod cs pair
   and cmds3=list_of_commands_for_end_part_of_feydeau cmod cs pair in 
   cmds1@cmds2@cmds3;;



let shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)=
  let cmds=list_of_commands_for_shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in  
  helper_for_feydeau cmod cs ([],[],cmds);; 


  
let end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)=
  match cmod with 
   Compilation_mode_t.Usual->()
   |_->
     let all_cmds=
       (list_of_commands_for_connecting_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path))@
       (list_of_commands_for_end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path)) in 
     let _=Image.image  Unix_command.hardcore_uc all_cmds in 
     ()



let feydeau cmod cs (opt_modnames,opt_rootless_path)=
  let answer=shaft_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in 
  let _=end_part_of_feydeau cmod cs (opt_modnames,opt_rootless_path) in 
  answer;; 


let usual_feydeau cs modnames = feydeau Compilation_mode_t.Usual cs (Some(modnames),None);;

end;;  




let clean_debug_dir cs=
  let s_root=Dfa_root.connectable_to_subpath(Coma_state.root cs) in
  let s_debug_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.debug_build_subdir)) in 
  Unix_command.uc("rm -f "^s_debug_dir^"*.cm*"^" "^s_debug_dir^"*.ocaml_debuggable");;
   
let name_element_for_debugged_file = "debugged" ;;
let debugged_file_path = (Dfa_subdirectory.connectable_to_subpath(Coma_constant.utility_files_subdir))
             ^ name_element_for_debugged_file ^ ".ml" ;;  

let start_debugging cs=
  let  _=clean_debug_dir cs in
  let ppodbg_path = ocamldebug_printersfile_path (Coma_state.root cs) in 
  let _= Io.overwrite_with (Absolute_path.of_string ppodbg_path) "" in   
  let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau Compilation_mode_t.Debug cs debugged_file_path in 
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
   
let clean_exec_dir cs=
  let s_root=Dfa_root.connectable_to_subpath(Coma_state.root cs) in
  let s_exec_dir=s_root^(Dfa_subdirectory.connectable_to_subpath(Coma_constant.exec_build_subdir)) in 
  Unix_command.uc("rm -f "^s_exec_dir^"*.cm*"^" "^s_exec_dir^"*.ocaml_executable"^" "^s_exec_dir^"*.o");;
   

let start_executing cs short_path=
  let  _=clean_exec_dir cs in
  let cmds=Ocaml_target_making.list_of_commands_for_ternary_feydeau 
    Compilation_mode_t.Executable cs short_path in 
  Unix_command.conditional_multiple_uc cmds;;   

end ;;

let clean_debug_dir = Private.clean_debug_dir;;
let clean_exec_dir = Private.clean_exec_dir;;
let start_debugging = Private.start_debugging;;
let start_executing = Private.start_executing ;;

let usual_batch = Private.Ocaml_target_making.usual_feydeau ;;




