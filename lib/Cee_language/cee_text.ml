(*

#use"lib/Cee_language/cee_text.ml";;

*)

exception Lonely_Elif_exn of int ;;
exception Lonely_Else_exn of int ;;
exception Lonely_Endif_exn of int ;;
exception Double_Else_exn of int ;;
  
module Private = struct 

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
    smallest_unused_namespace_index : int ;
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
    smallest_unused_namespace_index = 1;
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
        current_namespace = old_w.smallest_unused_namespace_index ;
        smallest_unused_namespace_index = old_w.smallest_unused_namespace_index + 1;
        preceding_namespaces = old_w.current_namespace :: old_w.preceding_namespaces
      }
    | Lb_Endif ->
      {
        w with 
        current_namespace = List.hd(old_w.preceding_namespaces) ;
        smallest_unused_namespace_index = old_w.smallest_unused_namespace_index;
        preceding_namespaces = List.tl(old_w.preceding_namespaces)
      }
   | Lb_Elif | Lb_Else | Lb_Usual -> w ;;

  


  let directive_step old_w line_idx line line_beginning= 
    if (old_w.start_index_opt = None) && (line_beginning = Lb_Elif) then raise (Lonely_Elif_exn(line_idx)) else 
    if (old_w.start_index_opt = None) && (line_beginning = Lb_Else) then raise (Lonely_Else_exn(line_idx)) else
    if (old_w.start_index_opt = None) && (line_beginning = Lb_Endif) then raise (Lonely_Endif_exn(line_idx)) else
    if (old_w.last_directive_was_an_else) && (line_beginning = Lb_Else) then raise (Double_Else_exn(line_idx)) else
    let new_cond_is_unfinished = (
      if List.mem line_beginning [Lb_If;Lb_Elif]
      then String.ends_with line ~suffix:"\\"
      else false 
    ) in 
    let w1 ={
    old_w with
    start_index_opt = None ;
    unfinished_condition = new_cond_is_unfinished;
    last_directive_was_an_else = (line_beginning = Lb_Else) ;
   } in 
   let w2 = register_new_small_space_if_needed old_w w1 line_idx in 
   let w3 = deal_with_namespace_data old_w w2 line_beginning in 
   w3 ;;
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

end ;;  

let included_local_files_in_file = Private.included_local_files_in_file ;;
let random_marker = Private.random_marker ;;
let rewrite_using_watermarks = Private.rewrite_using_watermarks ;;
let standardize_inclusion_line = Private.standardize_inclusion_line ;;
let watermark_text= Private.watermark_text;;


