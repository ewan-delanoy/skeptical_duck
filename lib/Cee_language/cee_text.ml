(*

#use"lib/Cee_language/cee_text.ml";;

*)

exception Lonely_Elif_exn of int ;;
exception Lonely_Else_exn of int ;;
exception Lonely_Endif_exn of int ;;
exception Double_Else_exn of int ;;
exception Standardize_inclusion_line_exn of string ;;  
exception Add_extra_inclusion_line_exn of string ;;
exception Unfinished_fiamengo_inclusion of string ;; 
  
module Private = struct 

  let str_order = Total_ordering.lex_for_strings ;;
  let str_mem = Ordered.mem str_order ;;  

  let i_order = Total_ordering.for_integers ;;
   let i_merge = Ordered.merge i_order ;; 

  let intstr_order = Total_ordering.product i_order str_order ;;

  let intstr_sort = Ordered.sort intstr_order;;

  let lines_inside_or_outside_cee_comments_or_dq_strings = 
    Lines_in_text.lines_inside_or_outside_cee_comments_or_dq_strings ;;

  let indexed_lines_inside_or_outside_cee_comments_or_dq_strings text = 
     Int_range.index_everything (lines_inside_or_outside_cee_comments_or_dq_strings text) ;; 


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
    "ifdef", Lb_If; 
    "ifndef", Lb_If;    
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

type guard_pattern_indicators_t = {
    first_ivy_index_opt : int option ;
    elsie_or_elif_for_first_index_opt : int option;
    endif_index_for_first_ivy_opt : int option ;
    ivy_index_after_first_ivy_opt : int option;
} ;; 




module Guard_Pattern = struct
let origin = {
  first_ivy_index_opt = None;
  elsie_or_elif_for_first_index_opt = None;
  endif_index_for_first_ivy_opt = None ;
  ivy_index_after_first_ivy_opt = None;
} ;; 

let test gpi=
  (gpi.endif_index_for_first_ivy_opt <> None) &&
  (gpi.elsie_or_elif_for_first_index_opt = None) &&
  (gpi.ivy_index_after_first_ivy_opt = None)  ;;   
  
end ;;

module Walker = struct 

   module Walker_Object = struct 

    type t = {
      lines : (int *(string * Situation_of_linebreak_inside_text_t.t)) list ;
      current_namespace : int ;
      preceding_namespaces : int list ;
      smallest_unused_namespace_index : int ;
      small_space_start_index_opt : int option ;
      unfinished_condition : bool;
      last_directive_was_an_else : bool;
      treated : small_space_t list;
      gpi : guard_pattern_indicators_t ;
    } ;;  
  let make text = {
    lines = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text ;
    current_namespace = 0 ;
    preceding_namespaces = [] ;
    smallest_unused_namespace_index = 1;
    small_space_start_index_opt = None ;
    unfinished_condition = false;
    last_directive_was_an_else = false;
    treated = [];
    gpi = Guard_Pattern.origin ;
  } ;; 

  let add_to_treated small_space w = {
       w with 
       treated = small_space :: (w.treated)   
    } ;; 

  let condition_is_unfinished w = w.unfinished_condition ;; 
  let get_current_namespace w = w.current_namespace ;; 
  let get_endif_index_for_first_ivy_opt w = w.gpi.endif_index_for_first_ivy_opt ;;
  let get_elsie_or_elif_for_first_index_opt w = w.gpi.elsie_or_elif_for_first_index_opt ;;
  let get_first_ivy_index_opt w = w.gpi.first_ivy_index_opt ;;
  let get_guard_pattern_indicators w = w.gpi ;;
  let get_ivy_index_after_first_ivy_opt w = w.gpi.ivy_index_after_first_ivy_opt ;;
  let get_lines w = w.lines ;;
  let get_preceding_namespaces w = w.preceding_namespaces ;;
  let get_small_space_start_index_opt w = w.small_space_start_index_opt ;;
  let get_smallest_unused_namespace_index w = w.smallest_unused_namespace_index ;;
  let get_treated w = w.treated ;;

  let last_directive_was_an_else w = w.last_directive_was_an_else ;;

  let register_endif_index_for_first_ivy w line_idx =
      let new_gpi = {w.gpi with endif_index_for_first_ivy_opt = Some line_idx} in 
      {w with gpi = new_gpi} ;;
  let register_elsie_or_elif_for_first_index w line_idx =
      let new_gpi = {w.gpi with elsie_or_elif_for_first_index_opt = Some line_idx} in 
      {w with gpi = new_gpi} ;;

  let register_first_ivy_index w line_idx =
    let new_gpi = {w.gpi with first_ivy_index_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;
  
  let register_ivy_index_after_first_ivy w line_idx =  
    let new_gpi = {w.gpi with ivy_index_after_first_ivy_opt = Some line_idx} in 
    {w with gpi = new_gpi} ;;

  let register_small_space_start_index w line_idx =    
      {
        w with 
        small_space_start_index_opt = Some line_idx 
      } ;; 

  let set_condition_to_finished w =    
      {
        w with 
        unfinished_condition = false
      }   ;;
 
  let set_lines w new_set_of_lines=    
      {
        w with 
        lines = new_set_of_lines
      }   ;;    

  let set_namespace_data w 
     ~v_current_namespace
     ~v_smallest_unused_namespace_index
     ~v_preceding_namespaces
      =  {
        w with 
        current_namespace = v_current_namespace ;
        smallest_unused_namespace_index = v_smallest_unused_namespace_index;
        preceding_namespaces = v_preceding_namespaces
      } ;;

  let set_unfinished_condition_data w 
      ~current_cond_is_unfinished
      ~current_line_is_an_elsie
    ={
      w with
        small_space_start_index_opt = None ;
        unfinished_condition = current_cond_is_unfinished;
        last_directive_was_an_else = current_line_is_an_elsie ;
     } ;;



   end ;;

  let register_new_small_space_if_needed old_w w line_idx= 
    match Walker_Object.get_small_space_start_index_opt old_w with 
    None -> w
    |Some start_index ->
      let small_space = {
       namespace = Walker_Object.get_current_namespace w ;
       start_idx = start_index ;
       end_idx  = line_idx -1 ;
      } in 
      Walker_Object.add_to_treated small_space w ;; 

  let deal_with_namespace_data old_w w line_beginning= 
     match line_beginning with 
     Lb_If ->
      let old_idx = Walker_Object.get_smallest_unused_namespace_index old_w in 
      let v_current_namespace=old_idx
      and v_smallest_unused_namespace_index=old_idx + 1
      and v_preceding_namespaces=(Walker_Object.get_current_namespace old_w) :: (Walker_Object.get_preceding_namespaces old_w) in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
     
    | Lb_Endif ->
      let preceding = Walker_Object.get_preceding_namespaces old_w in 
      let v_current_namespace=List.hd preceding
      and v_smallest_unused_namespace_index=Walker_Object.get_smallest_unused_namespace_index old_w
      and v_preceding_namespaces= List.tl preceding in 
      Walker_Object.set_namespace_data w 
        ~v_current_namespace
        ~v_smallest_unused_namespace_index
        ~v_preceding_namespaces
   | Lb_Elif | Lb_Else | Lb_Usual -> w ;;

   let deal_with_guard_pattern_detection old_w w line_beginning line_idx = 
    match line_beginning with 
    Lb_If ->
      (if ((Walker_Object.get_first_ivy_index_opt old_w) = None)
       then Walker_Object.register_first_ivy_index w line_idx
       else 
       if ((Walker_Object.get_endif_index_for_first_ivy_opt old_w) <> None) &&
          ((Walker_Object.get_ivy_index_after_first_ivy_opt old_w) = None)
       then Walker_Object.register_ivy_index_after_first_ivy w line_idx  
       else  w
       )
   | Lb_Elif | Lb_Else ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
         ((Walker_Object.get_current_namespace old_w)=1) &&
         ((Walker_Object.get_elsie_or_elif_for_first_index_opt old_w) = None)
      then Walker_Object.register_elsie_or_elif_for_first_index w line_idx 
      else w  
      )
     
   | Lb_Endif ->
    (if ((Walker_Object.get_first_ivy_index_opt old_w) <> None) &&
        ((Walker_Object.get_current_namespace old_w)=1) 
     then Walker_Object.register_endif_index_for_first_ivy w line_idx
     else w  
   )
   | Lb_Usual -> w ;;


  let directive_step old_w line_idx line line_beginning= 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Elif) then raise (Lonely_Elif_exn(line_idx)) else 
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Else) then raise (Lonely_Else_exn(line_idx)) else
    if ((Walker_Object.get_current_namespace old_w) = 0) && (line_beginning = Lb_Endif) then raise (Lonely_Endif_exn(line_idx)) else
    if (Walker_Object.last_directive_was_an_else old_w) && (line_beginning = Lb_Else) then raise (Double_Else_exn(line_idx)) else
    let current_cond_is_unfinished = (
      if List.mem line_beginning [Lb_If;Lb_Elif]
      then String.ends_with line ~suffix:"\\"
      else false 
    ) in 
    let w1 =
      Walker_Object.set_unfinished_condition_data old_w 
       ~current_cond_is_unfinished
       ~current_line_is_an_elsie:(line_beginning = Lb_Else) in 
   let w2 = register_new_small_space_if_needed old_w w1 line_idx in 
   let w3 = deal_with_namespace_data old_w w2 line_beginning in 
   let w4 = deal_with_guard_pattern_detection old_w w3 line_beginning line_idx in
   w4 ;;
  let inner_usual_step w line_idx line = 
    match Walker_Object.get_small_space_start_index_opt w  with 
    (Some _) -> w 
    |None -> 
    if not(Walker_Object.condition_is_unfinished w)
    then Walker_Object.register_small_space_start_index w line_idx 
    else
    if String.ends_with line ~suffix:"\\"
    then w
    else Walker_Object.set_condition_to_finished w ;; 

  let usual_step w line_idx line =
    let w2 = inner_usual_step w line_idx line in 
    if (Walker_Object.get_lines w2) = []
    then register_new_small_space_if_needed w2 w2 (line_idx+1)
    else w2 ;;    
    

  let step old_w = 
    let (line_idx,(line,linebreak_state)) = List.hd (Walker_Object.get_lines old_w) 
    and w = Walker_Object.set_lines old_w  
          (List.tl(Walker_Object.get_lines old_w)) in 
    let line_beginning = Line_beginning.compute line in 
    if linebreak_state <> Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings 
    then usual_step w line_idx line
    else     
    match line_beginning with 
    Lb_Usual -> usual_step w line_idx line 
   |Lb_If |Lb_Elif |Lb_Else |Lb_Endif -> 
     directive_step w line_idx line line_beginning ;;
  

  let rec iterate w =
    if (Walker_Object.get_lines w) = [] 
    then List.rev (Walker_Object.get_treated w) 
    else iterate (step w) ;;   

  
  let rec iterate_for_guard_pattern_detection w =
    if (Walker_Object.get_lines w) = [] 
    then Walker_Object.get_guard_pattern_indicators w 
    else iterate_for_guard_pattern_detection (step w) ;;   
    
let make = Walker_Object.make ;;

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

let test_text_for_guard_pattern text =
   Guard_Pattern.test 
   (Walker.iterate_for_guard_pattern_detection (Walker.make text)) ;; 

(*

let text1 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#endif 4";
  "#if 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check1 = Walker.iterate_for_guard_pattern_detection (Walker.make text1) ;;
let chuck1 = test_text_for_guard_pattern text1 ;;

let text2 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "4";
  "#elif 5";
  "6";
  "#endif 7";
  "8"
] ;;

let check2 = Walker.iterate_for_guard_pattern_detection (Walker.make text2) ;;
let chuck2 = test_text_for_guard_pattern text2 ;;

let text3 = String.concat "\n" 
[
  "1";
  "#if 2";
  "3";
  "#if 4";
  "5";
  "#elif 6";
  "7";
  "#endif 8";
  "9";
  "#if 10";
  "11";
  "#endif 12";
  "#endif 13";
  "14"
] ;;

let check3 = Walker.iterate_for_guard_pattern_detection (Walker.make text3) ;;
let chuck3 = test_text_for_guard_pattern text3 ;;


*)

let find_directive_from_list_opt line directives=
      if not(String.starts_with line ~prefix:"#")
      then None
      else  
      match Strung.char_finder_from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) line 2 with 
      None -> None
      |Some idx1 -> 
        List.find_opt (fun drctv ->
          Substring.is_a_substring_located_at drctv line idx1
          ) directives ;;

let text_has_ivy text =
   let lines = lines_inside_or_outside_cee_comments_or_dq_strings text in 
   List.exists (
    fun (line,linebreak_state) -> 
      (linebreak_state = Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings) &&
      (find_directive_from_list_opt line ["if"])<>None
   ) lines ;;          

exception First_ivy_in_text_exn ;;   
let first_ivy_in_text text =
   let lines = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
   match List.find_opt (fun (_line_idx,(line,linebreak_state))->
      (linebreak_state=Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings) && 
      (find_directive_from_list_opt line ["if"])<>None
   ) lines with 
   None -> raise First_ivy_in_text_exn 
   |Some(line_idx,_) -> line_idx ;;
   
exception Last_endif_in_text_exn ;;   
let last_endif_in_text text =
  let lines = List.rev(indexed_lines_inside_or_outside_cee_comments_or_dq_strings text) in 
  match List.find_opt (fun (_line_idx,(line,linebreak_state))->
          (linebreak_state=Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings) && 
         (find_directive_from_list_opt line ["endif"])<>None
  ) lines with 
   None -> raise Last_endif_in_text_exn 
  |Some(line_idx,_) -> line_idx ;;   
 
let put_first_ivy_on_first_line text = 
  let line_idx = first_ivy_in_text text in 
  Lines_in_text.put_line_first_in_text line_idx text ;;

let put_last_endif_on_last_line text = 
  let line_idx = last_endif_in_text text in 
  let temp_text = Lines_in_text.put_line_last_in_text line_idx text in 
  let temp_lines = List.rev(lines_inside_or_outside_cee_comments_or_dq_strings temp_text) in 
  let ((temp_last_line,_),temp_tl) = List_again.head_with_tail temp_lines in 
  (* Any comments after the #endif must be moved before it *)
  let after= Cull_string.two_sided_cutting  ("#endif","") temp_last_line in 
  let all_lines_except_last = Image.image fst temp_tl in 
  let almost_all_lines=(
     match Strung.char_finder_from_inclusive_opt (fun c->
              not(List.mem c [' ';'\t';'\r'])
            ) after 1 with 
      None -> all_lines_except_last
      |Some idx -> (Cull_string.cobeginning (idx-1) after) :: all_lines_except_last
  )in
  let lines = List.rev("#endif" :: almost_all_lines) in 
  String.concat "\n" lines;;  

let emphatize_first_ivy_and_last_endif text = 
  let text2 = put_first_ivy_on_first_line text in 
  put_last_endif_on_last_line text2 ;;

let standardize_guard_in_text_opt text = 
  if test_text_for_guard_pattern text 
  then let new_text = emphatize_first_ivy_and_last_endif text in 
       if new_text = text 
       then None 
       else Some(new_text)
  else None ;;   

 let marker_for_cd_defined_region = "cgmvgtkcxvvxckt" ;;  
 let parametrized_marker_for_cd_defined_region name_for_watermarkable_file k =
   let sk = string_of_int k in 
   marker_for_cd_defined_region^name_for_watermarkable_file^sk^marker_for_cd_defined_region ;;
 
 let parametrized_line_for_cd_defined_region name_for_watermarkable_file cd_idx =  
   "char* unused_string"^(string_of_int cd_idx)^"=\""^
   (parametrized_marker_for_cd_defined_region name_for_watermarkable_file cd_idx)^"\";" ;;
 
 let is_in_interval x (a,b) = (a<=x) && (x<=b) ;; 
 
 let is_in_interval_union x intervals =
    List.exists (is_in_interval x) intervals ;;
 
let marker_for_inclusion_highlighting = "cgmvgtkcxvvxckt" ;;  
let parametrized_marker_for_inclusion_highlighting inclusion_idx verb =
   let s_idx = string_of_int inclusion_idx in 
   "char* unused_string_for_inclusion_highlighting"^s_idx^"_"^verb^
   "=\""^marker_for_inclusion_highlighting^" Inclusion number "^s_idx^
   " "^verb^"s here \";";;
    ;;
 
 let markers_for_inclusion_highlighting inclusion_idx =  
   (
     parametrized_marker_for_inclusion_highlighting inclusion_idx "start",
     parametrized_marker_for_inclusion_highlighting inclusion_idx "end"
   );;


let compute_shadow old_text ~inclusion_index_opt ~name_for_included_file 
  ~preprocessed_includer_text =   
   let ssps = compute_small_spaces_in_text old_text  in 
   let indexed_ssps = Int_range.index_everything ssps in 
   let subtext = (
      match inclusion_index_opt with 
      None -> preprocessed_includer_text 
      |Some inclusion_idx -> 
        let markers = 
          markers_for_inclusion_highlighting inclusion_idx in 
        Cull_string.between_markers 
         markers preprocessed_includer_text
   ) in 
   let accepted_ssps = List.filter(
         fun (ssp_idx,ssp) ->
          if ssp.namespace = 0 then true else 
          Substring.is_a_substring_of 
          (parametrized_marker_for_cd_defined_region name_for_included_file ssp_idx) subtext 
   ) indexed_ssps in 
   Cee_shadow_t.Sh(List.length indexed_ssps,
   Cee_prawn_t.P(Image.image fst accepted_ssps)) ;;

 let crop_using_prawn old_text (Cee_prawn_t.P(accepted_indices)) =
   let lines = Lines_in_text.indexed_lines old_text 
   and ssps = compute_small_spaces_in_text old_text  in 
   let accepted_ssps = Image.image(
      fun ssp_idx ->
       (ssp_idx,List.nth ssps (ssp_idx-1)) 
   ) accepted_indices in 
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
  
  let tattoo_regions_between_conditional_directives ~name_for_included_file text = 
     let lines = Lines_in_text.indexed_lines text 
     and ssps = compute_small_spaces_in_text text in 
     let indexed_ssps = Int_range.index_everything ssps in
     let pairs = pairs_of_indices_for_watermarking indexed_ssps in 
     let temp1 = Image.image (
        fun (line_idx,line) ->
          match List.assoc_opt line_idx pairs with 
          None -> [line]
          | (Some ssp_idx) ->
             [parametrized_line_for_cd_defined_region name_for_included_file ssp_idx;line]
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

let text3 = parametrized_line_for_cd_defined_region 3;;

print_string(rewrite_using_watermarks text1 text3);;

*)

let generic_included_file_opt (opening_char,closing_char) line =
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
  if (Strung.get line idx2)<>opening_char
  then None
  else    
  match Strung.char_finder_from_inclusive_opt (fun c->
           c = closing_char
  ) line (idx2+1) with 
  None -> None
  |Some idx3 ->      
    Some (Cull_string.interval line (idx2+1) (idx3-1))
    ;;

let included_local_file_opt = generic_included_file_opt ('"','"');;
let included_nonlocal_file_opt = generic_included_file_opt ('<','>');;


let included_local_files_in_text text = 
  let temp1 = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
  let temp2 = List.filter_map (
    fun (line_idx,(line,linebreak_state)) ->
      if linebreak_state<>Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings then None else
      Option.map (fun included_fn ->
          (line_idx,included_fn)
      ) (included_local_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;

let included_nonlocal_files_in_text text = 
  let temp1 = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
  let temp2 = List.filter_map (
      fun (line_idx,(line,linebreak_state)) ->
        if linebreak_state<>Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings then None else
        Option.map (fun included_fn ->
            (line_idx,included_fn)
        ) (included_nonlocal_file_opt line)
  ) temp1 in 
  intstr_sort temp2 ;;


let add_extra_ending_in_inclusion_line ~extra line = 
  let occs = Substring.occurrences_of_in "\"" line in 
  if List.length(occs)<>2
  then raise(Add_extra_inclusion_line_exn(line))
  else 
  let i1 = List.nth occs 0 
  and i2 = List.nth occs 1 in 
  let included_file = Cull_string.interval line (i1+1) (i2-1) in 
  (Cull_string.beginning i1 line)^
  (Cee_common.add_extra_ending_in_filename ~extra included_file)^
  (Cull_string.interval line i2 (String.length line)) ;;

(*  
  add_extra_ending_in_inclusion_line 
  ~extra:"fat" "# include \"The_brown.cat\"";;
*)

let add_extra_ending_in_inclusions_inside_text ~extra text =
  let lines_before = lines_inside_or_outside_cee_comments_or_dq_strings text 
  and counter=ref 0 in 
  let lines_after = Image.image(
    fun (line,linebreak_state) -> 
      if (linebreak_state=Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings)
        &&
         ((included_local_file_opt line)<>None)
      then let _ =(counter:=(!counter)+1) in 
      add_extra_ending_in_inclusion_line ~extra line 
      else line    
  ) lines_before in 
  (String.concat "\n" lines_after,!counter);;
 let highlight_inclusions_in_text text = 
  let temp1 = indexed_lines_inside_or_outside_cee_comments_or_dq_strings text in 
  let all_lines = Image.image (
    fun (line_idx,(line,linebreak_state)) ->
      ((line_idx,line),
      (linebreak_state=Situation_of_linebreak_inside_text_t.Lbit_outside_comments_or_strings)
      &&
      ((included_local_file_opt line)<>None))
  ) temp1 in 
  let inclusion_lines = List.filter snd all_lines in 
  let indexed_inclusion_lines = Int_range.index_everything inclusion_lines in 
  let line_idx_to_incl_idx = Image.image (
    fun (incl_idx,((line_idx,_),_)) -> (line_idx,incl_idx)
  ) indexed_inclusion_lines in 
  let modified_lines = Image.image (
    fun ((line_idx,line),line_is_an_inclusion) ->
      if not(line_is_an_inclusion) then line else 
      let incl_idx = List.assoc line_idx line_idx_to_incl_idx in 
      let (marker_before,marker_after) = 
       markers_for_inclusion_highlighting incl_idx in 
      "\n"^marker_before ^"\n"^line^"\n"^marker_after 
  ) all_lines in 
  String.concat "\n" modified_lines ;;

(*


let text1 = String.concat "\n" 
[
   "1";
   "2";
   "#include \"abc\"";
   "4";
   "#include \"def\"";
   "#include \"ghi\"";
   "7";
] ;;

let text2 = highlight_inclusions_in_text text1 ;;

print_string text2 ;;


*)

let highlight_and_add_extra_ending_in_inclusions_inside_text 
   ~extra text =
    add_extra_ending_in_inclusions_inside_text ~extra 
     (highlight_inclusions_in_text text)
    ;;

let marker_for_beginning_fiamengo_inclusions = "xvxBLjSGtxSfWtCVHNDS" ;;
let marker_for_ending_fiamengo_inclusions = "vQBknJWWtwvqDgSkVJms" ;;

let fiamengo_ends_here = " ends here *)" ;;
let fiamengo_beginner_prefix ~fiamengo_depth =
  "(* "^marker_for_beginning_fiamengo_inclusions^" "^
    "Depth-"^(string_of_int fiamengo_depth)^" inclusion of ";;

let fiamengo_ender_prefix ~fiamengo_depth =
  "(* "^marker_for_ending_fiamengo_inclusions^" "^
    "Depth-"^(string_of_int fiamengo_depth)^" inclusion of ";;

let fiamengize_individual_inclusion reader ~fiamengo_depth 
   name_of_included_file =
    (fiamengo_beginner_prefix ~fiamengo_depth)^
    name_of_included_file^" starts here *)\n"^
    (reader name_of_included_file)^
    "\n"^
    (fiamengo_ender_prefix ~fiamengo_depth)^
    name_of_included_file^fiamengo_ends_here;;

let rec helper_for_fiamengization reader ~fiamengo_depth= function
(treated,remaining_lines) -> 
  match remaining_lines with 
  [] -> treated
  |line :: other_lines ->
   (
     match included_local_file_opt  line  with 
      None -> helper_for_fiamengization reader ~fiamengo_depth 
            (treated^"\n"^line,other_lines)
    |Some name_of_included_file ->
      let fiamengized_line =  
      fiamengize_individual_inclusion reader ~fiamengo_depth 
   name_of_included_file in 
      helper_for_fiamengization reader ~fiamengo_depth 
            (treated^"\n"^fiamengized_line,other_lines)
   ) ;;

let fiamengize_whole_text  reader ~fiamengo_depth text= 
  helper_for_fiamengization reader ~fiamengo_depth
  ("",Lines_in_text.lines text) ;;

let extract_fiamengo_included_filename end_prefix line =
   Cull_string.two_sided_cutting (end_prefix,fiamengo_ends_here) line ;;
  

let pusher_for_fiamengo_parsing
  (opt_answer,walker) =
  match opt_answer with 
  (Some _) -> failwith("This should never happen in pusher_for_fiamengo_parsing")
  |None ->
  let (beg_prefix,end_prefix,treated,inclusion_pending,subtext,remaining_lines) = walker in 
    match remaining_lines with 
    [] -> (Some(List.rev ((false,(subtext,""))::treated)),walker) 
    |line :: other_lines ->
    (*
     simple two-state alternative : if an inclusion is currently
     pending we look for the end prefix, otherwise for the beginning prefix
    *)
    if inclusion_pending
   then (if String.starts_with line ~prefix:end_prefix 
          then  let filename = extract_fiamengo_included_filename end_prefix line in 
                 (None,(beg_prefix,end_prefix,(true,(filename,subtext))::treated,
                false,"",other_lines))
          else  (None,(beg_prefix,end_prefix,treated,
                true,subtext^"\n"^line,other_lines))
        )
   else if String.starts_with line ~prefix:beg_prefix 
          then  (*
                extra care must be taken in case the included subtext is 
                empty
               *)
               let treated2 = (false,(subtext,""))::treated in 
               (
                match other_lines with 
                [] -> raise(Unfinished_fiamengo_inclusion line)
                |line2 :: other_lines2 ->
                  if String.starts_with line2 ~prefix:end_prefix 
                  then let filename = extract_fiamengo_included_filename end_prefix line in 
                      (None,(beg_prefix,end_prefix,(true,(filename,""))::treated2,
                       false,"",other_lines2))
                  else (None,(beg_prefix,end_prefix,treated2,
                       true,line2,other_lines2))

               )
          else    
                (None,(beg_prefix,end_prefix,treated,
                false,subtext^"\n"^line,other_lines))  ;;    


let rec iterator_for_fiamengo_parsing  pair =
  match fst pair with 
  (Some answer) -> answer 
  | None -> 
    iterator_for_fiamengo_parsing 
    (pusher_for_fiamengo_parsing pair) ;;
  

let parse_fiamengized_text ~fiamengo_depth text = 
   iterator_for_fiamengo_parsing  
  (None,(fiamengo_beginner_prefix ~fiamengo_depth,
   fiamengo_ender_prefix ~fiamengo_depth,
   [],false,"",Lines_in_text.lines text)) ;;




end ;;  

let compute_shadow = Private.compute_shadow ;;
let crop_using_prawn = Private.crop_using_prawn ;;

let fiamengize_text ~fiamengo_depth reader text = Private.fiamengize_whole_text ~fiamengo_depth reader text;;
let highlight_and_add_extra_ending_in_inclusions_inside_text = Private.highlight_and_add_extra_ending_in_inclusions_inside_text ;;
let included_local_files_in_text = Private.included_local_files_in_text ;;
let included_nonlocal_files_in_text = Private.included_nonlocal_files_in_text ;;
let parse_fiamengized_text ~fiamengo_depth text = Private.parse_fiamengized_text ~fiamengo_depth text;;
let standardize_guard_in_text_opt = Private.standardize_guard_in_text_opt ;;
let tattoo_regions_between_conditional_directives= Private.tattoo_regions_between_conditional_directives;;


