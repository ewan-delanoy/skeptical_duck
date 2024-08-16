(*

#use"lib/Cee_language/cee_project_transfer.ml";;

*)

exception Elif_in_ether of int ;;
exception Elif_after_else of int ;;
exception Else_in_ether of int ;;
exception Endif_in_ether of int ;;

module type CAPSULE_TYPE = sig
   
  type initial_command = {
    short_path : string;
    ending : string;
    core_of_command : string;
  } ;;

   type t 
   val source : t -> Directory_name_t.t
   val destination : t -> Directory_name_t.t
   val commands : t -> initial_command list
   val make :
     source:Directory_name_t.t ->
     destination:Directory_name_t.t -> string list -> t
   val all_h_or_c_files : t -> string list   
   val directly_compiled_files : t -> string list
 
  end ;;



 
module Capsule = (struct 

  type initial_command = {
  short_path : string;
  ending : string;
  core_of_command : string;
  } ;;

 type immutable_t = {
    source : Directory_name_t.t ;
    destination : Directory_name_t.t ;
    commands : initial_command list;
    all_h_or_c_files_opt : (string list) option ;
    directly_compiled_files_opt : (string list) option ;
 } ;;

 type t = immutable_t ref ;;
 let str_order = Total_ordering.lex_for_strings ;;
 let str_sort = Ordered.sort str_order ;;  
 let source cpsl = (!cpsl).source ;;

 let destination cpsl = (!cpsl).destination ;; 

 let commands cpsl = (!cpsl).commands ;;

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
 ref({
   source = src ;
   destination = dest ;
   commands = Image.image make_initial_command raw_commands;
   all_h_or_c_files_opt = None ;
   directly_compiled_files_opt = None ;
}) ;;

let compute_all_h_or_c_files cpt = 
  let src = Directory_name.connectable_to_subpath cpt.source in 
  let temp1 = Unix_again.quick_beheaded_complete_ls src in
  str_sort(List.filter (
    fun fn -> List.exists (fun edg ->
       String.ends_with fn ~suffix:edg) [".h";".c";".macros"]
 ) temp1 );;  

 let all_h_or_c_files cpsl_ref = 
   let old_cpsl = (!cpsl_ref) in
   match old_cpsl.all_h_or_c_files_opt with 
  (Some old_answer) -> old_answer 
  |None ->
    let answer = compute_all_h_or_c_files old_cpsl in 
    let new_cpsl = {old_cpsl with 
      all_h_or_c_files_opt = Some answer 
    } in 
    let _ = (cpsl_ref:=new_cpsl) in 
    answer ;;

  let compute_directly_compiled_files cpsl = 
    str_sort(Image.image (
      fun cmd ->
       (cmd.short_path) ^ 
       (cmd.ending)
    ) cpsl.commands) ;;    
    
  let directly_compiled_files cpsl_ref = 
       let old_cpsl = (!cpsl_ref) in
       match old_cpsl.directly_compiled_files_opt with 
      (Some old_answer) -> old_answer 
      |None ->
        let answer = compute_directly_compiled_files old_cpsl in 
        let new_cpsl = {old_cpsl with 
          directly_compiled_files_opt = Some answer 
        } in 
        let _ = (cpsl_ref:=new_cpsl) in 
        answer ;;
   

end :CAPSULE_TYPE);; 

module Private = struct 

  type initial_command = Capsule.initial_command = {
    short_path : string;
    ending : string;
    core_of_command : string;
    } ;;

    let str_order = Total_ordering.lex_for_strings ;;
    let str_mem = Ordered.mem str_order ;;  

    let i_order = Total_ordering.for_integers ;;
     let i_merge = Ordered.merge i_order ;; 

     let sil_merge ox oy=
     let rec tempf=(function (u,v,accu)->
     if u=[] then (List.rev_append(accu)(v)) else
     if v=[] then (List.rev_append(accu)(u)) else
     let (xu,lxu)=List.hd(u) and yu=List.tl(u) 
     and (xv,lxv)=List.hd(v) and yv=List.tl(v) in
     match str_order(xu)(xv) with
       Total_ordering_result_t.Lower->tempf(yu,v,(xu,lxu)::accu)
     |Total_ordering_result_t.Equal->tempf(yu,yv,(xu,i_merge lxu lxv)::accu)
     |Total_ordering_result_t.Greater->tempf(u,yv,(xv,lxv)::accu)
     ) in
     tempf(ox,oy,[]);;
   
   let rec sil_sort  x=
     if List.length(x)<2
     then x
     else 
     let temp1=Partition_list.split_in_half(x) in
     let y1=sil_sort(fst temp1)
     and y2=sil_sort(snd temp1) in
     sil_merge y1 y2;;      
   

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



let main_preprocessing_command cpsl init_cmd = 
  let src_dir = Directory_name.connectable_to_subpath (Capsule.source cpsl) in  
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

let remove_cds_in_file cpsl init_cmd  = 
  let src_dir = Directory_name.connectable_to_subpath (Capsule.source cpsl) 
  and dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in  
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
  let cmd1 = main_preprocessing_command cpsl init_cmd in 
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

let remove_cds_in_files cpsl init_cmds = 
  let temp1 = Int_range.index_everything init_cmds 
  and sn = string_of_int(List.length init_cmds) in 
  List.iter (fun (idx,init_cmd) ->
    let msg1 = " Step "^(string_of_int idx)^" of "^sn^" : "^
    "removing cds in "^init_cmd.short_path^init_cmd.ending^"\n\n"
    and msg2 = " Finished step "^(string_of_int idx)^" of "^sn^".\n" in 
    print_string msg1;
    flush stdout;
    remove_cds_in_file cpsl init_cmd;
    print_string msg2;
    flush stdout;
    ) temp1 ;;

let remove_cds cpsl = remove_cds_in_files cpsl (Capsule.commands cpsl) ;; 

let cleanup_temporary_data_for_file cpsl init_cmd  = 
  let src_dir = Directory_name.connectable_to_subpath (Capsule.source cpsl)  in  
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
  
let cleanup_temporary_files_after_cds_removal cpsl =   
  Image.image (cleanup_temporary_data_for_file cpsl) (Capsule.commands cpsl) ;;

let remove_cds_and_cleanup cpsl =
   let _ = (remove_cds cpsl;
   cleanup_temporary_files_after_cds_removal cpsl) in 
  ();;
   
  exception Check_presence_in_project_exn of string ;;

  let check_presence_in_project cpsl fn =
     if str_mem fn (Capsule.all_h_or_c_files cpsl) 
     then fn 
     else raise(Check_presence_in_project_exn(fn));;  
   

  
exception Normalize_included_filename_exn of string * string ;;
let normalize_nonpointed_included_filename cpsl includer_fn included_fn =
   match List.find_opt (fun nfn->
     String.ends_with nfn ~suffix:included_fn   
   ) (Capsule.all_h_or_c_files cpsl) with 
  None -> raise (Normalize_included_filename_exn(includer_fn,included_fn))
  |(Some nfn) -> nfn;;   

  let rec normalize_pointed_included_filename cpsl includer_fn included_fn = 
    if not(String.starts_with included_fn ~prefix:"../") 
    then check_presence_in_project cpsl (includer_fn ^ "/" ^ included_fn) 
    else 
      let includer_fn2 = Cull_string.before_rightmost includer_fn '/'
      and included_fn2 = Cull_string.cobeginning 3 included_fn in 
      normalize_pointed_included_filename cpsl includer_fn2 included_fn2 ;;  
       
 let normalize_included_filename cpsl includer_dir included_fn = 
    if String.starts_with included_fn ~prefix:"../" 
    then normalize_pointed_included_filename cpsl includer_dir included_fn 
    else normalize_nonpointed_included_filename cpsl includer_dir included_fn ;;   


    let included_local_file_opt line =
      if not(String.starts_with line ~prefix:"#")
      then None  
      else  
      match Strung.char_finder_from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) line 2 with 
      None -> None
      |Some idx1 -> 
      if not(Substring.is_a_substring_located_at "include" line idx1)
      then None
      else   
      match Strung.char_finder_from_inclusive_opt (fun c->
            not(List.mem c [' ';'\t';'\r'])
      ) line (idx1+7) with 
      None -> None
      |Some idx2 -> 
      if (Strung.get line idx2)<>'"'
      then None
      else    
      match Strung.char_finder_from_inclusive_opt (fun c->
               c = '"'
      ) line (idx2+1) with 
      None -> None
      |Some idx3 ->      
        Some (Cull_string.interval line (idx2+1) (idx3-1))
        ;;

    let included_local_files_in_text text = 
      let temp1 = Lines_in_string.indexed_lines text in 
      let temp2 = List.filter_map (
        fun (line_idx,line) ->
          Option.map (fun included_fn ->
              (included_fn,[line_idx])
          ) (included_local_file_opt line)
      ) temp1 in 
      sil_sort temp2 ;;
    
    let included_local_files_in_file ap =
      included_local_files_in_text (Io.read_whole_file ap) ;; 
    
    exception Included_files_exn of string * string * string ;;

    let included_files cpsl includer_fn=
      let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in 
      let ap = Absolute_path.of_string (dest_dir ^ includer_fn) in 
      let temp1 = included_local_files_in_file ap 
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      Image.image (fun (included_fn,indices)->
            try (normalize_included_filename cpsl includer_dir included_fn,indices) with
            Normalize_included_filename_exn(x,y) -> raise(Included_files_exn(includer_fn,x,y))
            ) temp1;;
      
    let nonstandard_inclusion_formats_in_individual_includer cpsl includer_fn = 
      let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in 
      let ap = Absolute_path.of_string (dest_dir ^ includer_fn) in 
      let temp1 = included_local_files_in_file ap 
      and includer_dir = Cull_string.before_rightmost includer_fn '/' in 
      let text = Io.read_whole_file ap in 
      let lines = Lines_in_string.indexed_lines text in 
       List.flatten( List.filter_map (fun (included_fn,indices)->
          try (fun _->None)(normalize_included_filename cpsl includer_dir included_fn) with
               Normalize_included_filename_exn(_,_) -> 
                  Some(Image.image(fun idx->(includer_fn,List.assoc idx lines)) indices)
               ) temp1) ;;
      
    let nonstandard_inclusion_formats_in_includers cpsl includers =
      List.flatten(
        Image.image (nonstandard_inclusion_formats_in_individual_includer cpsl) includers
      ) ;;

   exception Standardize_inclusion_line_exn of string ;;   
   let standardize_inclusion_line line = 
      let occs = Substring.occurrences_of_in "\"" line in 
      if List.length(occs)<>2
      then raise(Standardize_inclusion_line_exn(line))
      else 
      let i1 = List.nth occs 0 
      and i2 = List.nth occs 1 in 
      let b = Bytes.of_string line in 
      let _ = (Bytes.set b (i1-1) '<';Bytes.set b (i2-1) '>') in 
      Bytes.to_string b ;;

   let standardize_inclusion_in_files cpsl includers ~dry_run= 
     let temp1 = nonstandard_inclusion_formats_in_includers cpsl includers in 
     let replacements_to_be_made = Image.image (
       fun (includer,line)->(includer,line,standardize_inclusion_line line)
     ) temp1 in 
    let _ =(
      if not(dry_run)
      then  let dest_dir = Directory_name.connectable_to_subpath (Capsule.destination cpsl) in 
            List.iter (fun (fn,ab,ba)->
                let ap =  Absolute_path.of_string (dest_dir ^ fn) in 
                Replace_inside.replace_inside_file (ab,ba) ap) replacements_to_be_made
    ) in 
    replacements_to_be_made;; 



end ;;

let make = Capsule.make ;;

let remove_conditional_directives_in_directly_compiled_files = Private.remove_cds_and_cleanup ;; 

let standardize_inclusion_in_files = Private.standardize_inclusion_in_files ;;
