(*

#use"lib/Cee_language/cee_conditional_directives.ml";;

*)

exception Else_with_elif of int * int * int ;;
exception Lonely_elif of int * string ;;
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
     if String.starts_with line ~prefix:"#elif"
        then 
           (
             match partially_treated with 
             [] -> raise(Lonely_elif(line_idx,line))
             |(start_idx,middle_idx_opt) :: other_partially_treated ->
               (
                 match middle_idx_opt with 
                 (Some (idx1)) -> raise(Else_with_elif(start_idx,idx1,line_idx))
                 |None ->
                   helper_for_conditional_directive_linedexing 
                   (treated,(line_idx,None)::(start_idx,Some line_idx)::other_partially_treated,other_lines)   
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

let compute_line_indices_for_cds_in_file ap = 
  compute_line_indices_for_cds_in_text (Io.read_whole_file ap)  ;;  

let adapt_ii_element root_dir i_elt = 
  if i_elt="-I." then "-I"^root_dir else 
  if (String.get i_elt 2)='/' then i_elt else 
  "-I"^root_dir^(Cull_string.cobeginning 2 i_elt) ;;    

let adapt_element root_dir (preceding_elt_opt,elt) = 
  if String.starts_with elt ~prefix:"-I"
  then adapt_ii_element root_dir elt
  else 
  match preceding_elt_opt with 
  None -> elt 
  |Some preceding_elt ->
    if List.mem preceding_elt ["-MF";"-MT"]
    then root_dir^elt
    else elt;;

let adapt_command root_dir cmd =
  let temp1 = Str.split (Str.regexp "[ \t\r]+") cmd in 
  let temp2 = List_again.universal_delta_list temp1 in 
  let temp3 = (None,List.hd temp1):: (Image.image (fun (e1,e2)->(Some e1,e2) ) temp2) in 
  let temp4 = Image.image (adapt_element root_dir) temp3 in 
  String.concat " " temp4 ;;

let random_marker = "cgmvgtkcxvvxckt" ;;  

let parametrized_marker k =
  let sk = string_of_int k in 
  random_marker^sk^random_marker ;;

let compute_intervals_to_be_removed_in_cut_range (start_idx,middle_idx,end_idx) block_is_accepted = 
  if block_is_accepted
  then [(start_idx,start_idx);(middle_idx,end_idx)]
  else [(start_idx,middle_idx);(end_idx,end_idx)] ;;  
  

let compute_intervals_to_be_removed_in_uncut_range (start_idx,end_idx) block_is_accepted = 
  if block_is_accepted
  then [(start_idx,start_idx);(end_idx,end_idx)]
  else [(start_idx,end_idx)] ;;    

let compute_intervals_to_be_removed  (start_idx,middle_idx_opt,end_idx) block_is_accepted = 
  match  middle_idx_opt with 
  (Some middle_idx) -> compute_intervals_to_be_removed_in_cut_range (start_idx,middle_idx,end_idx) block_is_accepted
  | None -> compute_intervals_to_be_removed_in_uncut_range (start_idx,end_idx) block_is_accepted ;;

let is_in_interval x (a,b) = (a<=x) && (x<=b) ;; 

let is_in_interval_union x intervals =
   List.exists (is_in_interval x) intervals ;;

let rewrite_using_watermarks old_text watermarked_text =   
  let lines = Lines_in_string.indexed_lines old_text 
  and triples = compute_line_indices_for_cds_in_text old_text  in 
  let indexed_triples = Int_range.index_everything triples in 
  let intervals_to_be_removed = List.flatten(Image.image (
     fun (cd_idx,(start_idx,middle_idx_opt,end_idx)) ->
      let block_is_accepted = 
        Substring.is_a_substring_of (parametrized_marker cd_idx) watermarked_text in 
      compute_intervals_to_be_removed  (start_idx,middle_idx_opt,end_idx) block_is_accepted 
  ) indexed_triples) in 
  let accepted_lines = List.filter_map (
     fun (line_idx,line) ->
       if is_in_interval_union line_idx intervals_to_be_removed 
       then None 
      else Some line 
  ) lines in 
  String.concat "\n" accepted_lines ;;




let watermark_text text = 
   let lines = Lines_in_string.indexed_lines text 
   and triples = compute_line_indices_for_cds_in_text text in 
   let cd_idx_from_line_idx =(fun line_idx ->
     Option.get(List.find_index (fun (line_idx2,_,_)->line_idx2=line_idx) triples)+1 
   ) in 
   let temp1 = Image.image (
     fun (line_idx,line) ->
       if  String.starts_with line ~prefix:"#if" 
       then let cd_idx = cd_idx_from_line_idx line_idx in
            [line;"char* unused_string"^(string_of_int cd_idx)^"=\""^
            random_marker^(parametrized_marker cd_idx)^random_marker^"\";"] 
       else [line]
   ) lines in 
   (String.concat "\n" (List.flatten temp1)) ;;

let main_pp_command (root_dir,raw_core_of_command,short_path) = 
    let core_of_command = adapt_command root_dir raw_core_of_command in 
    let s_ap = root_dir ^ short_path in 
    let short_s_ap = Cull_string.coending 2 s_ap 
    and ending = Cull_string.ending 2 s_ap in
    let second_filename = 
       (short_s_ap^"_"^random_marker^"_second"^ending) in
    let third_filename = 
      (short_s_ap^"_"^random_marker^"_third"^ending) in 
    core_of_command^" -E "^second_filename^" -o "^third_filename  ;;

let watermark_file (root_dir,raw_core_of_command,short_path) = 
    let s_ap = root_dir ^ short_path in 
    let short_s_ap = Cull_string.coending 2 s_ap 
    and ending = Cull_string.ending 2 short_path in
    let ap = Absolute_path.of_string s_ap in  
    let old_text = Io.read_whole_file ap in 
    let second_text = watermark_text old_text 
    and second_filename = 
       (short_s_ap^"_"^random_marker^"_second"^ending) in
    let second_file = Absolute_path.create_file_if_absent second_filename in
    let _ = Io.overwrite_with second_file second_text in 
    let current_dir = Sys.getcwd () in 
    let third_filename = 
      (short_s_ap^"_"^random_marker^"_third"^ending) in 
    let cmd1 = main_pp_command (root_dir,raw_core_of_command,short_path) in 
    let _ = (print_string("Executing "^cmd1^" ...");flush stdout) in 
    let cmds = [
      " cd "^root_dir;
      cmd1;
      " cd "^current_dir;      
    ] in 
    let _ = Unix_command.conditional_multiple_uc cmds in 
    let third_file = Absolute_path.of_string third_filename in 
    let third_text =Io.read_whole_file third_file in 
    let new_text = rewrite_using_watermarks old_text third_text in 
    let fourth_filename = 
      (short_s_ap^"_"^random_marker^"_fourth"^ending) in 
    let fourth_file = Absolute_path.create_file_if_absent fourth_filename in  
    Io.overwrite_with fourth_file new_text ;;



