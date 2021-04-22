
(* 

#use"Compilation_management/needed_data_summary.ml";;

*)

(*
module Private = struct 

let modules_coming_from_modules_or_subdirs cs needed_modules needed_subdirs =
    let modules_above=Image.image (fun nm->
       Coma_state.above cs 
       (Coma_state.endingless_at_module cs nm)
    ) needed_modules  in 
    let all_elesses = Coma_state.all_modules cs in 
    Option.filter_and_unpack 
        (fun eless->
          if (List.exists(
            fun l->List.mem (Dfn_endingless.to_module eless) l
        )(needed_modules::modules_above)) ||
        (List.exists (Dfn_endingless.begins_with eless ) needed_subdirs) 
        then Some(Dfn_endingless.to_module eless)
        else None)
    all_elesses ;;
    
let rootlesses_to_be_copied cs summary =
        let fw = cs.Coma_state_t.frontier_with_unix_world in 
        let all_needed_modules =
        (match summary with 
        Needed_data_summary_t.Everything -> Coma_state.ordered_list_of_modules cs
       |Selection(needed_modules,needed_subdirs)-> 
        modules_coming_from_modules_or_subdirs cs needed_modules needed_subdirs
             ) in   
             let compilables= selector (fw.Fw_wrapper_t.compilable_files)
             and noncompilables= selector (fw.Fw_wrapper_t.noncompilable_files) in 
             let all_needed_modules= (Image.image Dfn_rootless.to_module compilables) 
                              @ needed_modules in 
             let (modules_in_good_order,all_nonspecials)=rootlesses_coming_from_modules cs all_needed_modules in 
             (modules_in_good_order,all_nonspecials,noncompilables);;


end ;;

let expand cs summary=
  let modules = (
    match summary with 
    Needed_data_summary_t.Everything -> cs.Coma_state_t.modules 
    |Selection(needed_modules,needed_subdirs) ->
        Private.rootlesses_coming_from_modules_or_subdirs 
            cs needed_modules needed_subdirs   
  ) in 
  let collected_acolytes=List.flatten  (Image.image (Coma_state.acolytes_at_module cs) modules) in 
  (modules,Image.image Dfn_full.to_rootless collected_acolytes);;

let is_everything = function Needed_data_summary_t.Everything -> true | _ -> false ;;
*)
