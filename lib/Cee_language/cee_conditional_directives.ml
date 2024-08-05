(*

#use"lib/Cee_language/cee_conditional_directives.ml";;

*)

exception Else_with_elif of int * int * int ;;
exception Endif_without_matching_non_elif of int ;;
exception Lonely_elif of int * string ;;
exception Lonely_else of int ;;
exception Lonely_endif of int ;;
exception Two_elses of int * int * int ;;

exception Elif_in_ether of int ;;
exception Elif_after_else of int ;;
exception Else_in_ether of int ;;
exception Endif_in_ether of int ;;



type initial_command_t = {
   short_path : string ;
   ending : string ;
   core_of_command : string ;
} ;;

type config_t = {
   source : Directory_name_t.t ;
   destination : Directory_name_t.t ;
   commands : initial_command_t list;
} ;;

type line_beginning = 
    Lb_If
   |Lb_Elif
   |Lb_Else 
   |Lb_Endif 
   |Lb_Usual ;; 

module Line_beginning = struct 

  let data = [
    "if", Lb_If;
    "elif", Lb_Elif;  
    "else", Lb_Else;
    "endif", Lb_Endif
]  ;;  

let compute line =
  if not(String.starts_with line ~prefix:"#")
  then Lb_Usual  
  else  
  match Strung.char_finder_from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> Lb_Usual
  |Some idx1 -> 
    (match List.find_opt (fun (lb_name,_lb)->
      Substring.is_a_substring_located_at lb_name line idx1
      ) data with 
    None -> Lb_Usual
    |Some (_,lb) -> lb 
    );;

end ;;  



type small_space_t = {
   namespace : int ;
   start_idx : int ;
   end_idx  : int ;
} ;;


type walker_t = {
    lines : (int *string) list ;
    current_namespace : int ;
    preceding_namespaces : int list ;
    smallest_unused_namespace : int ;
    start_index_opt : int option ;
    unfinished_condition : bool;
    last_directive_was_an_else : bool;
    treated : small_space_t list;
} ;; 

module Walker = struct 

  let make text = {
    lines = Lines_in_string.indexed_lines text ;
    current_namespace = 0 ;
    preceding_namespaces = [] ;
    smallest_unused_namespace = 1;
    start_index_opt = None ;
    unfinished_condition = false;
    last_directive_was_an_else = false;
    treated = [];
  } ;; 

  let register_new_small_space_if_needed old_w w line_idx= 
    match old_w.start_index_opt with 
    None -> w
    |Some start_index ->
      let small_space = {
       namespace = old_w.current_namespace ;
       start_idx = start_index ;
       end_idx  = line_idx -1 ;
      } in 
     {
       w with 
       treated = small_space :: (w.treated)   
    } ;; 

  let deal_with_namespace_data old_w w line_beginning= 
     match line_beginning with 
     Lb_If ->
      {
        w with 
        current_namespace = old_w.smallest_unused_namespace ;
        smallest_unused_namespace = old_w.smallest_unused_namespace + 1;
        preceding_namespaces = old_w.current_namespace :: old_w.preceding_namespaces
      }
    | Lb_Endif ->
      {
        w with 
        current_namespace = List.hd(old_w.preceding_namespaces) ;
        smallest_unused_namespace = old_w.smallest_unused_namespace;
        preceding_namespaces = List.tl(old_w.preceding_namespaces)
      }
   | Lb_Elif | Lb_Else | Lb_Usual -> w ;;

  let directive_step old_w line_idx line line_beginning= 
    let cond_is_unfinished = (
      if List.mem line_beginning [Lb_If;Lb_Elif]
      then String.ends_with line ~suffix:"\\"
      else false 
    ) in 
    let w1 ={
    old_w with
    start_index_opt = None ;
    unfinished_condition = cond_is_unfinished;
    last_directive_was_an_else = (line_beginning = Lb_Else) ;
   } in 
   let w2 = register_new_small_space_if_needed old_w w1 line_idx in 
   deal_with_namespace_data old_w w2 line_beginning;; 
  
  let inner_usual_step w line_idx line = 
    match w.start_index_opt with 
    (Some _) -> w 
    |None -> 
    if not(w.unfinished_condition)
    then 
          {
            w with 
            start_index_opt = Some line_idx 
          } 
    else
    if String.ends_with line ~suffix:"\\"
    then w
    else 
      {
        w with 
        start_index_opt = Some line_idx ;
        unfinished_condition = false
      }     ;; 

  let usual_step w line_idx line =
    let w2 = inner_usual_step w line_idx line in 
    if w2.lines = []
    then register_new_small_space_if_needed w2 w2 (line_idx+1)
    else w2 ;;    
    

  let step old_w = 
    let (line_idx,line) = List.hd old_w.lines 
    and w = {old_w with lines = List.tl(old_w.lines)} in 
    let line_beginning = Line_beginning.compute line in 
    match line_beginning with 
    Lb_Usual -> usual_step w line_idx line 
   |Lb_If |Lb_Elif |Lb_Else |Lb_Endif -> directive_step w line_idx line line_beginning ;;
  

  let rec iterate w =
    if w.lines = [] 
    then List.rev w.treated 
    else iterate (step w) ;;   

end ;;  

let compute_small_spaces text =
   Walker.iterate(Walker.make text) ;; 

type multiline_t = {
   is_finished : bool ;
   last_line_idx : int ;
} ;;

module Multiline = struct 
  
let opt line_idx line = 
  if String.ends_with line ~suffix:"\\"
  then Some (
      {
       is_finished = false ;
       last_line_idx = line_idx ;
      }) 
  else None ;;

let finish_here ml = {
   ml with 
   is_finished = true;
} ;;

end ;;   

type incomplete_conditional_directive_t = {
   start_idx : int ;
   multiline : multiline_t option ;
   is_an_elif : bool ;
   middle_idx : int option ;
} ;;

module Incomplete_conditional_directive = struct 

let make line_idx line ~is_elif= {
  start_idx = line_idx ;
  multiline = Multiline.opt line_idx line ;
  is_an_elif  = is_elif ;
  middle_idx = None ;
} ;;

let add_elsie line_idx icd = 
  match icd.middle_idx with 
  (Some middle_idx) -> raise (Two_elses(icd.start_idx,middle_idx,line_idx))
  | None ->
  {
  icd with
  multiline = Option.map Multiline.finish_here icd.multiline  ;
  middle_idx = Some line_idx ;
} ;;

let add_usual (line_idx,line) icd = 
  match icd.multiline with 
   None -> icd
  |(Some multiline) -> 
    if multiline.is_finished 
    then icd 
    else  
    let new_multiline = {
      last_line_idx = line_idx;
      is_finished = (not(String.ends_with line ~suffix:"\\"))
      } in 
    {
      icd with 
      multiline = Some new_multiline ;
    } ;;

let range_for_master_ivy icd = 
  let end_idx = (
    match icd.multiline with 
    None -> icd.start_idx
    |(Some multiline) -> multiline.last_line_idx
  ) in 
  (icd.start_idx,end_idx) ;;  


end ;;  

exception Unfinished of incomplete_conditional_directive_t ;; 

type complete_conditional_directive_t = {
  before_the_end : incomplete_conditional_directive_t ;
  end_idx : int;
} ;;

module Complete_conditional_directive = struct 
  
let make line_idx icd = {
  before_the_end = icd ;
  end_idx = line_idx;
} ;;

let range_for_master_ivy ccd = 
   Incomplete_conditional_directive.range_for_master_ivy ccd.before_the_end ;; 

end ;;

type one_ivy_several_elives_t = {
    the_ivy : incomplete_conditional_directive_t;
    elives : incomplete_conditional_directive_t list
} ;;

module One_ivy_several_elives = struct 

let head oise = 
  match oise.elives with 
  [] -> oise.the_ivy 
  |elif :: _ -> elif ;;

let make ivy = {
  the_ivy = ivy;
  elives = [] 
} ;;

let add_elif new_elif oise = {
  oise with 
  elives = new_elif :: oise.elives ;
} ;;

let add_elsie line_idx oise = 
  match oise.elives with 
  [] -> {
    oise with 
    the_ivy = Incomplete_conditional_directive.add_elsie line_idx oise.the_ivy
  }
  | last_elif :: other_elives ->

  {
  oise with 
  elives = (Incomplete_conditional_directive.add_elsie line_idx last_elif) :: other_elives ;
} ;; 

let add_usual (line_idx,line) oise = 
  match oise.elives with 
  [] -> {
    oise with 
    the_ivy = Incomplete_conditional_directive.add_usual (line_idx,line) oise.the_ivy
  }
  | last_elif :: other_elives ->

  {
  oise with 
  elives = (Incomplete_conditional_directive.add_usual (line_idx,line) last_elif) :: other_elives ;
} ;; 

end ;;  

type partially_treated_t = PT of one_ivy_several_elives_t list ;;

module Partially_treated = struct 

let empty_one = PT [] ;;

let head (PT l) = One_ivy_several_elives.head(List.hd l) ;;  


let add_ivy (line_idx,line) (PT l) = 
  let new_ivy = Incomplete_conditional_directive.make line_idx line ~is_elif:false in 
  PT((One_ivy_several_elives.make new_ivy)::l) ;;

let add_elif (line_idx,line) (PT l) =  
  match l with 
  [] -> raise(Lonely_elif(line_idx,line)) 
  |oise :: other_blocks ->
  let new_elif = Incomplete_conditional_directive.make line_idx line ~is_elif:true in 
  PT((One_ivy_several_elives.add_elif new_elif oise)::other_blocks) ;;
  

let add_endif line_idx treated (PT l) =  
  match l with 
  [] -> raise(Lonely_endif(line_idx))
  |oise :: other_blocks ->
    let new_treated_items =
      List.rev_map (Complete_conditional_directive.make line_idx)
        (oise.elives@[oise.the_ivy]) in
     (List.rev_append new_treated_items treated,PT other_blocks) ;;
 
let add_elsie line_idx (PT l) =  
    match l with 
    [] -> raise(Lonely_else(line_idx))
  |oise :: other_blocks -> 
   PT((One_ivy_several_elives.add_elsie line_idx oise) :: other_blocks) ;;

let add_usual (line_idx,line) (PT l) =  
    match l with 
    [] -> PT l 
    |oise :: other_blocks -> 
    PT((One_ivy_several_elives.add_usual (line_idx,line) oise)::other_blocks) ;;
    


end ;;  


let test_for_directive_at_beginning_of_line directive_name line =
  if not(String.starts_with line ~prefix:"#")
  then false  
  else  
  match Strung.char_finder_from_inclusive_opt (fun c->
          not(List.mem c [' ';'\t';'\r'])
        ) line 2 with 
  None -> false 
  |Some idx1 -> Substring.is_a_substring_located_at directive_name line idx1 ;;   

let rec helper_for_conditional_directive_linedexing (treated,partially_treated,to_be_treated) = 
  match to_be_treated with 
  [] -> (
          if partially_treated = Partially_treated.empty_one 
          then List.rev treated 
          else raise(Unfinished(Partially_treated.head partially_treated)) 
        )
  |(line_idx,line) :: other_lines ->
     if test_for_directive_at_beginning_of_line "if" line
     then helper_for_conditional_directive_linedexing 
         (treated,Partially_treated.add_ivy (line_idx,line) partially_treated,other_lines)   
     else  
     if test_for_directive_at_beginning_of_line "else" line
     then helper_for_conditional_directive_linedexing
          (treated,Partially_treated.add_elsie line_idx partially_treated,other_lines)
     else   
     if test_for_directive_at_beginning_of_line "elif" line
     then helper_for_conditional_directive_linedexing
          (treated,Partially_treated.add_elif (line_idx,line) partially_treated,other_lines)     
     else    
     if test_for_directive_at_beginning_of_line "endif" line
     then let (treated2,partially_treated2) = Partially_treated.add_endif line_idx treated partially_treated in 
          helper_for_conditional_directive_linedexing 
                 (treated2,partially_treated2,other_lines)   
     else helper_for_conditional_directive_linedexing 
          (treated,Partially_treated.add_usual (line_idx,line)  partially_treated,other_lines) ;;

let compute_line_indices_for_cds_in_text text = 
  let lines = Lines_in_string.indexed_lines text in 
  helper_for_conditional_directive_linedexing ([],Partially_treated.empty_one,lines) ;;
(*

let text1 = String.concat "\n" 
[
   "#if 1\\";
   "2\\";
   "3";
   "#elif 4\\";
   "5";
   "#if 6";
   "7";
   "#endif 8";
   "#endif 9";
] ;;

let see1 = Cee_conditional_directives.compute_line_indices_for_cds_in_text text1 ;;

*)

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

let parametrized_line cd_idx =  
  "char* unused_string"^(string_of_int cd_idx)^"=\""^
  random_marker^(parametrized_marker cd_idx)^random_marker^"\";" ;;

let compute_intervals_to_be_removed_in_cut_range middle_idx ccd block_is_accepted = 
  let end_idx = ccd.end_idx in 
  if block_is_accepted
  then [Complete_conditional_directive.range_for_master_ivy ccd;(middle_idx,end_idx)]
  else [(ccd.before_the_end.start_idx,middle_idx);(end_idx,end_idx)] ;;  
  

let compute_intervals_to_be_removed_in_uncut_range ccd block_is_accepted = 
  let end_idx = ccd.end_idx in 
  if block_is_accepted
  then [Complete_conditional_directive.range_for_master_ivy ccd;(end_idx,end_idx)]
  else [(ccd.before_the_end.start_idx,end_idx)] ;;    

let compute_intervals_to_be_removed ccd block_is_accepted = 
  match  ccd.before_the_end.middle_idx  with 
  (Some middle_idx) -> compute_intervals_to_be_removed_in_cut_range middle_idx ccd block_is_accepted
  | None -> compute_intervals_to_be_removed_in_uncut_range ccd block_is_accepted ;;

let is_in_interval x (a,b) = (a<=x) && (x<=b) ;; 

let is_in_interval_union x intervals =
   List.exists (is_in_interval x) intervals ;;

let rewrite_using_watermarks old_text watermarked_text =   
  let lines = Lines_in_string.indexed_lines old_text 
  and ccds = compute_line_indices_for_cds_in_text old_text  in 
  let indexed_ccds = Int_range.index_everything ccds in 
  let intervals_to_be_removed = List.flatten(Image.image (
     fun (ccd_idx,ccd) ->
      let block_is_accepted = 
        Substring.is_a_substring_of (parametrized_marker ccd_idx) watermarked_text in 
      compute_intervals_to_be_removed  ccd block_is_accepted 
  ) indexed_ccds) in 
  let accepted_lines = List.filter_map (
     fun (line_idx,line) ->
       if is_in_interval_union line_idx intervals_to_be_removed 
       then None 
      else Some line 
  ) lines in 
  String.concat "\n" accepted_lines ;;

let rec helper_for_text_watermarking indexed_ccds
   (treated,ccd_idx_in_progress_opt,to_be_treated) =
   match to_be_treated with 
   [] -> List.rev treated 
   |(line_idx,line) :: others ->
      if (test_for_directive_at_beginning_of_line "if" line) ||
        (test_for_directive_at_beginning_of_line "elif" line) 
      then let ccd_idx =  
             fst(List.find (fun (_idx,ccd)->ccd.before_the_end.start_idx=line_idx) indexed_ccds) in 
           let treated2 = (
             match ccd_idx_in_progress_opt with 
             None -> treated 
             |Some ccd_idx_in_progress -> (parametrized_line ccd_idx_in_progress)::treated
           )  in 
           (
            if String.ends_with line ~suffix:"\\"
            then  helper_for_text_watermarking indexed_ccds 
                 (line::treated2,Some ccd_idx,others)
            else  helper_for_text_watermarking indexed_ccds 
                 ((parametrized_line ccd_idx)::line::treated2,None,others)  
           )
      else  
      match ccd_idx_in_progress_opt with 
      None -> helper_for_text_watermarking indexed_ccds 
             (line::treated,None,others)
      |Some ccd_idx_in_progress ->
        if String.ends_with line ~suffix:"\\"
          then  helper_for_text_watermarking indexed_ccds 
               (line::treated,ccd_idx_in_progress_opt,others)
          else  helper_for_text_watermarking indexed_ccds 
               ((parametrized_line ccd_idx_in_progress)::line::treated,None,others)        

let watermark_text text = 
   let lines = Lines_in_string.indexed_lines text 
   and ccds = compute_line_indices_for_cds_in_text text in 
   let indexed_ccds = Int_range.index_everything ccds in 
   let temp1 = helper_for_text_watermarking indexed_ccds 
   ([],None,lines) in 
   (String.concat "\n" temp1) ;;

(*

let text1 = String.concat "\n" 
[
   "#if 1";
   "2";
   "#elif 3";
   "4";
   "#if 5";
   "6";
   "#endif 7";
   "#endif 8";
   "9";
   "#if 10\\";
   "11\\";
   "12\\";
   "13";
   "14";
   "#endif 15"
] ;;

let text2 = Cee_conditional_directives.watermark_text text1 ;;

print_string(text2);;

print_string(Cee_conditional_directives.rewrite_using_watermarks text1 text2);;

let text3 = Cee_conditional_directives.parametrized_line 3;;

print_string(Cee_conditional_directives.rewrite_using_watermarks text1 text3);;

*)


let make_initial_command raw_command = 
  let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " raw_command 1) in 
  let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " raw_command (i1+4)) in
  let short_filename = Cull_string.interval raw_command (i1+4) (i2-1) in 
  {
    short_path = Cull_string.coending 2 short_filename ;
    ending = Cull_string.ending 2 short_filename ;
    core_of_command =Cull_string.beginning (i1-1) raw_command ;
  }  ;;
 

let initialize_configuration 
  ~source:src ~destination:dest raw_commands = 
  {
    source = src ;
    destination = dest ;
    commands = Image.image make_initial_command  raw_commands;
 } ;;


let main_preprocessing_command config init_cmd = 
  let src_dir = Directory_name.connectable_to_subpath config.source in  
  let core_of_command = adapt_command src_dir init_cmd.core_of_command in 
  let s_ap = src_dir ^ init_cmd.short_path ^ init_cmd.ending in 
  let short_s_ap = Cull_string.coending 2 s_ap 
  and ending = Cull_string.ending 2 s_ap in
  let second_filename = 
       (short_s_ap^"_"^random_marker^"_second"^ending) in
  let third_filename = 
      (short_s_ap^"_"^random_marker^"_third"^ending) in 
  core_of_command^" -E "^second_filename^" -o "^third_filename  ;;

let announce cmd = 
  (print_string("Executing "^cmd^" ...\n\n");
  flush stdout) ;;

let remove_cds_in_file config init_cmd  = 
  let src_dir = Directory_name.connectable_to_subpath config.source 
  and dest_dir = Directory_name.connectable_to_subpath config.destination in  
  let src_last = (Cull_string.after_rightmost (Cull_string.coending 1 src_dir) '/' ) ^ "/"
  and dest_last = (Cull_string.after_rightmost (Cull_string.coending 1 dest_dir) '/' ) ^ "/" in 
  let s_ap = src_dir ^ init_cmd.short_path ^ init_cmd.ending in 
  let short_s_ap = Cull_string.coending 2 s_ap 
  and ending = init_cmd.ending in
  let ap = Absolute_path.of_string s_ap in  
  let old_text = Io.read_whole_file ap in 
  let second_text = watermark_text old_text 
  and second_filename = 
       (short_s_ap^"_"^random_marker^"_second"^ending) in
  let second_file = Absolute_path.create_file_if_absent second_filename in
  let _ = announce("(watermark  "^
     (init_cmd.short_path ^ init_cmd.ending)^") > "^
     (src_last ^ init_cmd.short_path ^"_"^random_marker^"_second"^ending)^")") in 
  let _ = Io.overwrite_with second_file second_text in 
  let current_dir = Sys.getcwd () in 
    let third_filename = 
      (short_s_ap^"_"^random_marker^"_third"^ending) in 
  let cmd1 = main_preprocessing_command config init_cmd in 
  let _ = announce(cmd1) in 
  let cmds = [
      " cd "^src_dir; 
      cmd1;
      " cd "^current_dir;      
  ] in 
  let _ = Unix_command.conditional_multiple_uc cmds in 
  let third_file = Absolute_path.of_string third_filename in 
  let third_text =Io.read_whole_file third_file in 
  let new_text = rewrite_using_watermarks old_text third_text in 
  let fourth_filename = 
      (dest_dir ^ init_cmd.short_path ^ending) in 
  let fourth_file = Absolute_path.create_file_if_absent fourth_filename in  
  let _ = announce("(unifdefed  "^
     (init_cmd.short_path ^ init_cmd.ending)^") > "^
     (dest_last ^ init_cmd.short_path ^ending)^")") in 
  Io.overwrite_with fourth_file new_text ;;

let remove_cds_in_files config init_cmds = 
  let temp1 = Int_range.index_everything init_cmds 
  and sn = string_of_int(List.length init_cmds) in 
  List.iter (fun (idx,init_cmd) ->
    let msg1 = " Step "^(string_of_int idx)^" of "^sn^" : "^
    "removing cds in "^init_cmd.short_path^init_cmd.ending^"\n\n"
    and msg2 = " Finished step "^(string_of_int idx)^" of "^sn^".\n" in 
    print_string msg1;
    flush stdout;
    remove_cds_in_file config init_cmd;
    print_string msg2;
    flush stdout;
    ) temp1 ;;

let remove_cds config = remove_cds_in_files config config.commands ;; 

let cleanup_temporary_data_for_file config init_cmd  = 
  let src_dir = Directory_name.connectable_to_subpath config.source  in  
  let s_ap = src_dir ^ init_cmd.short_path ^ init_cmd.ending in 
  let short_s_ap = Cull_string.coending 2 s_ap 
  and ending = init_cmd.ending in
  let second_filename = 
       (short_s_ap^"_"^random_marker^"_second"^ending) in
  let third_filename = 
      (short_s_ap^"_"^random_marker^"_third"^ending) in 
  Unix_command.conditional_multiple_uc [
       "rm -f "^second_filename;
       "rm -f "^third_filename
  ]  ;;
  
let cleanup_temporary_data config =   
  Image.image (cleanup_temporary_data_for_file config) config.commands ;;