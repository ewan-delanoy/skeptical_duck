(*

#use"lib/Cee_language/cee_conditional_directives.ml";;

*)


exception Lonely_else of int * string ;;
exception Lonely_endif of int * string ;;
exception Two_elses of int * int * int ;;
exception Unfinished of ( int * (int option) ) ;; 

let rec helper_for_conditional_directive_linedexing (treated,partially_treated,to_be_treated) = 
  match to_be_treated with 
  [] -> (
          if partially_treated = [] 
          then List.rev treated 
          else raise(Unfinished(List.hd partially_treated)) 
        )
  |(line_idx,line) :: other_lines ->
     if String.starts_with line ~prefix:"#if"
     then helper_for_conditional_directive_linedexing (treated,(line_idx,None)::partially_treated,other_lines)   
     else  
     if String.starts_with line ~prefix:"#else"
     then 
        (
          match partially_treated with 
          [] -> raise(Lonely_else(line_idx,line))
          |(start_idx,middle_idx_opt) :: other_partially_treated ->
            (
              match middle_idx_opt with 
              (Some (idx1)) -> raise(Two_elses(start_idx,idx1,line_idx))
              |None ->
                helper_for_conditional_directive_linedexing 
                (treated,(start_idx,Some line_idx)::other_partially_treated,other_lines)   
            )
         ) 
     else   
     if String.starts_with line ~prefix:"#endif"
     then 
         (
           match partially_treated with 
           [] -> raise(Lonely_endif(line_idx,line))
           |(start_idx,middle_idx_opt) :: other_partially_treated ->
             (
               helper_for_conditional_directive_linedexing 
                 ((start_idx,middle_idx_opt,line_idx)::treated,other_partially_treated,other_lines)   
             )
          ) 
     else      
    helper_for_conditional_directive_linedexing (treated,partially_treated,other_lines) ;;

let compute_line_indices_for_cds_in_text text = 
  let lines = Lines_in_string.indexed_lines text in 
  helper_for_conditional_directive_linedexing ([],[],lines) ;;