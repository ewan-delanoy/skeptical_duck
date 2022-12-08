(* 

#use"lib/Compilation_management/needed_data_summary.ml";;

*)


module Private = struct 

let compute_all_needed_elesses cs needed_modules needed_subdirs =
    let all_elesses = Fw_with_dependencies.all_endinglesses cs in 
    let step1_modules = More_option.filter_and_unpack 
    (fun eless->
      if List.mem (Dfn_endingless.to_subdirectory eless) needed_subdirs
    then Some(Dfn_endingless.to_module eless)
    else None) all_elesses in
    let step2_modules = needed_modules@step1_modules in 
    let modules_above=List.flatten (Image.image (fun nm->
       Fw_with_dependencies.ancestors_for_module cs nm
    ) step2_modules)  in 
    let list_of_modules_with_nonstandard_ordering = 
          Ordered.sort Total_ordering.standard (modules_above@step2_modules) in 
    let all_elesses = Fw_with_dependencies.all_endinglesses cs in 
    List.filter 
        (fun eless-> List.mem (Dfn_endingless.to_module eless) list_of_modules_with_nonstandard_ordering)
    all_elesses ;;

    

let expand fw summary =
        let all_needed_elesses =
        (match summary with 
        Needed_data_summary_t.Everything -> Fw_with_dependencies.all_endinglesses fw
       |Needed_data_summary_t.Selection(needed_modules,needed_subdirs)-> 
        compute_all_needed_elesses fw needed_modules needed_subdirs
             ) in 
        let all_needed_subdirs = 
                Ordered.sort Total_ordering.standard 
                  (Image.image Dfn_endingless.to_subdirectory all_needed_elesses) 
        and all_needed_modules = 
         Image.image Dfn_endingless.to_module all_needed_elesses in      
        let original_noncompilables = Fw_with_archives.noncompilable_files fw in
        (*
           we do not know a priori if the noncompilables in other subdirectories
           are needed, so we include them all by default 
        *)      
        let noncompilables =
            (match summary with 
             Needed_data_summary_t.Everything -> original_noncompilables
            |Needed_data_summary_t.Selection(_needed_modules,_needed_subdirs)-> 
              List.filter (
                fun rless-> List.mem (Dfn_rootless.to_subdirectory rless) all_needed_subdirs 
            ) original_noncompilables) in        
        let compilables= List.filter (
            fun rless->List.mem (Dfn_rootless.to_module rless) all_needed_modules 
        ) (Fw_with_archives.usual_compilable_files fw) in
        (all_needed_modules,compilables,noncompilables);;


end ;;


let expand = Private.expand ;;

let is_everything = function 
   Needed_data_summary_t.Everything -> true 
 | Needed_data_summary_t.Selection _ -> false ;;

