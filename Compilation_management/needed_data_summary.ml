
(* 

#use"Compilation_management/needed_data_summary.ml";;

*)


module Private = struct 

let compute_all_needed_modules cs needed_modules needed_subdirs =
    let all_elesses = Coma_state.all_endinglesses cs in 
    let step1_modules = Option.filter_and_unpack 
    (fun eless->
      if List.mem (Dfn_endingless.to_subdirectory eless) needed_subdirs
    then Some(Dfn_endingless.to_module eless)
    else None) all_elesses in
    let step2_modules = needed_modules@step1_modules in 
    let modules_above=List.flatten (Image.image (fun nm->
       Coma_state.above cs 
       (Coma_state.endingless_at_module cs nm)
    ) step2_modules)  in 
    let list_of_modules_with_nonstandard_ordering = 
          Ordered.sort Total_ordering.standard (modules_above@step2_modules) in 
    let all_elesses = Coma_state.all_endinglesses cs in 
    Option.filter_and_unpack 
        (fun eless->
          if List.mem (Dfn_endingless.to_module eless) list_of_modules_with_nonstandard_ordering
        then Some(Dfn_endingless.to_module eless)
        else None)
    all_elesses ;;
    
let expand cs summary =
        let fw = cs.Coma_state_t.frontier_with_unix_world in 
        let all_needed_modules =
        (match summary with 
        Needed_data_summary_t.Everything -> Coma_state.ordered_list_of_modules cs
       |Selection(needed_modules,needed_subdirs)-> 
        compute_all_needed_modules cs needed_modules needed_subdirs
             ) in 
        (*     
        let all_needed_subdirs = 
                Ordered.sort Total_ordering.standard (Image.image Dfa_ all_needed_module) in  
        *)            
        let original_noncompilables = fw.Fw_wrapper_t.noncompilable_files in      
        let noncompilables =
            (match summary with 
             Needed_data_summary_t.Everything -> Image.image fst original_noncompilables
            |Selection(needed_modules,needed_subdirs)-> 
              Option.filter_and_unpack (
                fun (rless,_)->
                    if List.mem (Dfn_rootless.to_subdirectory rless) needed_subdirs 
                    then Some rless
                    else None    
            ) original_noncompilables) in        
        let compilables= Option.filter_and_unpack (
            fun (rless,_)->
                if List.mem (Dfn_rootless.to_module rless) all_needed_modules 
                then Some rless
                else None    
        ) (fw.Fw_wrapper_t.compilable_files) in
        (all_needed_modules,compilables,noncompilables);;


end ;;

let expand = Private.expand ;;

let is_everything = function Needed_data_summary_t.Everything -> true | _ -> false ;;

