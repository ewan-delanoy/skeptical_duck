(*

#use"lib/Cee_language/cee_conditional_directives.ml";;

*)

exception Elif_in_ether of int ;;
exception Elif_after_else of int ;;
exception Else_in_ether of int ;;
exception Endif_in_ether of int ;;

module Private = struct 

  type initial_command = {
   short_path : string ;
   ending : string ;
   core_of_command : string ;
} ;;

 type t = {
    source : Directory_name_t.t ;
    destination : Directory_name_t.t ;
    commands : initial_command list;
 } ;;

 let make_initial_command raw_command = 
  let i1 = Option.get(Substring.leftmost_index_of_in_from_opt " -c " raw_command 1) in 
  let i2 = Option.get(Substring.leftmost_index_of_in_from_opt " " raw_command (i1+4)) in
  let short_filename = Cull_string.interval raw_command (i1+4) (i2-1) in 
  {
    short_path = Cull_string.coending 2 short_filename ;
    ending = Cull_string.ending 2 short_filename ;
    core_of_command =Cull_string.beginning (i1-1) raw_command ;
  }  ;;

 let make 
 ~source:src ~destination:dest raw_commands = 
 {
   source = src ;
   destination = dest ;
   commands = Image.image make_initial_command raw_commands;
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

let compute_small_spaces_in_text text =
   Walker.iterate(Walker.make text) ;; 

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

let check1 = compute_small_spaces_in_text text1 ;;

let text2 = String.concat "\n" 
[
   "1";
   "2";
   "#if 3\\";
   "4\\";
   "5";
   "#elif 6\\";
   "7";
   "#if 8";
   "9";
   "#endif 10";
   "11";
   "12";
   "#endif 13";
   "14";
] ;;

let check2 = compute_small_spaces_in_text text2 ;;

*)   

let compute_small_spaces_in_file ap = 
  compute_small_spaces_in_text (Io.read_whole_file ap)  ;;  

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

let is_in_interval x (a,b) = (a<=x) && (x<=b) ;; 

let is_in_interval_union x intervals =
   List.exists (is_in_interval x) intervals ;;

let rewrite_using_watermarks old_text watermarked_text =   
  let lines = Lines_in_string.indexed_lines old_text 
  and ssps = compute_small_spaces_in_text old_text  in 
  let indexed_ssps = Int_range.index_everything ssps in 
  let accepted_ssps = List.filter(
     fun (ssp_idx,ssp) ->
      if ssp.namespace = 0 then true else 
      Substring.is_a_substring_of (parametrized_marker ssp_idx) watermarked_text 
  ) indexed_ssps in 
  let accepted_intervals = Image.image (
    fun (_,ssp) -> (ssp.start_idx,ssp.end_idx)
  ) accepted_ssps in 
  let accepted_lines = List.filter_map (
     fun (line_idx,line) ->
       if is_in_interval_union line_idx accepted_intervals 
       then Some line 
      else None
  ) lines in 
  String.concat "\n" accepted_lines ;;

let pairs_of_indices_for_watermarking indexed_ssps = 
  List.filter_map (
    fun (ssp_idx,ssp) ->
      if ssp.namespace = 0 then None else 
      Some(ssp.start_idx,ssp_idx) 
  ) indexed_ssps ;;

let watermark_text text = 
   let lines = Lines_in_string.indexed_lines text 
   and ssps = compute_small_spaces_in_text text in 
   let indexed_ssps = Int_range.index_everything ssps in
   let pairs = pairs_of_indices_for_watermarking indexed_ssps in 
   let temp1 = Image.image (
      fun (line_idx,line) ->
        match List.assoc_opt line_idx pairs with 
        None -> [line]
        | (Some ssp_idx) ->
           [parametrized_line ssp_idx;line]
   ) lines in  
   (String.concat "\n" (List.flatten temp1)) ;;

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

let text2 = watermark_text text1 ;;

print_string(text2);;

print_string(rewrite_using_watermarks text1 text2);;

let text3 = parametrized_line 3;;

print_string(rewrite_using_watermarks text1 text3);;

*)



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
  let third_filename = 
      (short_s_ap^"_"^random_marker^"_third"^ending) in 
  let cmd1 = main_preprocessing_command config init_cmd in 
  let _ = announce(cmd1) in 
  let _ = Unix_command.uc cmd1 in 
  let third_file = Absolute_path.of_string third_filename in 
  let third_text =Io.read_whole_file third_file in 
  let new_text = rewrite_using_watermarks old_text third_text in 
  let fourth_filename = 
      (dest_dir ^ init_cmd.short_path ^ending) in 
  let fourth_file = Absolute_path.create_file_if_absent fourth_filename in  
  let _ = announce("(unifdeffed  "^
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
  
let cleanup_temporary_files_after_removing_conditional_directives_in_directly_compiled_files config =   
  Image.image (cleanup_temporary_data_for_file config) config.commands ;;

end ;;

let make = Private.make ;;

let remove_conditional_directives_in_directly_compiled_files = Private.remove_cds ;; 

let cleanup_temporary_files_after_removing_conditional_directives_in_directly_compiled_files = Private.cleanup_temporary_files_after_removing_conditional_directives_in_directly_compiled_files ;;
