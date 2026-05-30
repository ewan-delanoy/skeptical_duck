(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_local_analizer.ml";;

*)

open Jvng_types ;;

module Private = struct 

let no_first_trial cbc is_complete = {
  first_approach = (fun (_tl:Jvsp_types.token_type_list) -> None);
  case_by_case = cbc ; 
  analysis_is_complete = is_complete ;
};;

end ;;
   
let direct l= Private.no_first_trial (Image.image (fun x->(x,Jvng_duplicated_name.of_string "true")) l) true ;;

let first_trial_only f = {
  first_approach = f;
  case_by_case = [] ; 
  analysis_is_complete = false ;
};;

let no_first_trial = Private.no_first_trial ;;

let one_level_above_molecular l= direct [l] ;; 
   
let use anlizr remaining_list = 
    match anlizr.first_approach remaining_list with 
     Some answer1 -> Some answer1 
     | None -> 
    match List.find_opt (fun (prefix,_)->Jvsp_token_types_list.starts_with remaining_list prefix) anlizr.case_by_case with 
     Some(_,answer2) -> Some answer2
     |None ->
       if anlizr.analysis_is_complete 
       then Some (Jvng_duplicated_name.of_string "false") 
       else None ;;


       