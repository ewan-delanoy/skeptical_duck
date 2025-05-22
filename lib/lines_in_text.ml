(*

#use"lib/lines_in_text.ml";;

*)

exception Shift_indentation_in_line_exn of int * string ;;

exception Put_line_first_bad_line_index_exn  of int * int ;;
exception Put_line_last_bad_line_index_exn  of int * int ;;

exception Unfinished_single_quoted_string ;;
exception Unfinished_double_quoted_string ;;

module Private = struct 

  let lines old_text=
     let left_offset=(if String.starts_with ~prefix:"\n" old_text  then "\n" else "")
     and right_offset=(if String.ends_with ~suffix:"\n" old_text  then "\n" else "") in
     let s=left_offset^old_text^right_offset in
     Str.split (Str.regexp_string "\n") s ;;

  let indexed_lines text=
     Int_range.index_everything (lines text);;
  
  let rec iterator_for_enchancement (num_of_treated_chars,treated_lines,lines) =
       match lines with 
       [] -> List.rev treated_lines 
       |(line_idx,line) :: other_lines ->
        iterator_for_enchancement 
        (num_of_treated_chars+(String.length line)+1,
         (num_of_treated_chars+1,line_idx,line)::treated_lines,other_lines)   ;;
        
  let enhance indexed_lines =  iterator_for_enchancement (0,[],indexed_lines );;      
  
  let adjust_num_of_lines_upwards_in_text ~required_size text =
      let temp1 = lines text in  
      let d = required_size - (List.length temp1) in 
      if d<=0 
      then text 
      else text ^ (String.make d '\n') ;;   

  let adjust_num_of_lines_upwards_in_file ~required_size file =
      let old_text = Io.read_whole_file file in
      let new_text = adjust_num_of_lines_upwards_in_text ~required_size old_text  in
      Io.overwrite_with file new_text ;;   

  let tripartition_associated_to_interval s i j=
      let temp2=lines s in 
      let (temp3,temp4)=List_again.long_head_with_tail (i-1) temp2 in 
      let part1=String.concat "\n" (List.rev temp3) in 
      let (temp5,temp6)=List_again.long_head_with_tail (j-i+1) temp4 in 
      let part2=String.concat "\n" (List.rev temp5) in 
      let part3=String.concat "\n" temp6 in 
      (part1^"\n",part2,"\n"^part3);;
   
   (* tripartition_associated_to_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
       
  let interval text i j=
    let temp1=indexed_lines text in
    let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
    let temp3=Image.image snd temp2 in
    String.concat "\n" temp3;;  
      
  (* interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
    
  let copy_interval_from_text_to_text (i,j)  src dest =
     let src_linelength = List.length (lines src) in 
     let temp1 = adjust_num_of_lines_upwards_in_text ~required_size:src_linelength dest in
     let (before,_in_between,after) = tripartition_associated_to_interval temp1 i j in 
     before^(interval src i j)^after;;

      
  (* copy_interval_from_text_to_text (2,5) "1\n2\n3\n4\n5\n6\n7\n" "a\nb\nc";; *)

  let copy_interval_from_file_to_file (i,j) src_file  dest_file =
     let src = Io.read_whole_file src_file 
     and old_text = Io.read_whole_file dest_file  in 
     let new_text = copy_interval_from_text_to_text (i,j) src old_text in 
     Io.overwrite_with dest_file new_text ;; 
     
   exception Lines_in_char_range_exn of int*int;;

   let number_of_lines_in_char_interval text  i j=
     try (List.length(List.filter (fun k->
         String.get text (k-1)='\n'
     ) (Int_range.range i j))) with
     _->raise(Lines_in_char_range_exn(i,j));;    

   let duplicate_interval_in_text (i,j) text = 
     let (before,itv,after) = tripartition_associated_to_interval text i j in 
     before^itv^"\n"^itv^after ;;

  (* duplicate_interval_in_text (2,4) "1\n2\n3\n4\n5\n";; *)

   let duplicate_interval_in_file (i,j) src_file  =
     let old_text = Io.read_whole_file src_file  in 
     let new_text = duplicate_interval_in_text (i,j) old_text in 
     Io.overwrite_with src_file new_text ;; 

   let naive_closeup_around_index text j=
     let n=String.length text in
     let temp1=List.filter(fun j->(String.get text (j-1))='\n')(Int_range.range 1 n) in
     let (temp2,temp3)=Hurried.partition_in_two_parts(fun k->k<j) temp1 in
     let a=(if List.length(temp2)<6 then 1 else List.nth(List.rev temp2)(5))
     and b=(if List.length(temp3)<6 then n else List.nth(temp3)(5)) in
     (a,String.sub text a (b-a));;

  let closeup_around_index text idx =
     let (char_idx,subtext) = naive_closeup_around_index text idx in 
     let startline_idx = (Strung.number_of_lines_before text char_idx) in 
     let lines = indexed_lines subtext in 
     let decorated_lines = Image.image (
       fun (idx2,line)->
          let prefix = 
            Strung.insert_repetitive_offset_on_the_left ' ' 6 (string_of_int (idx2+startline_idx)) in 
          prefix^": "^line  
     ) lines in
     String.concat "\n" (""::decorated_lines) ;; 

     let change_indentation_in_interval_in_text ~indent (i,j) ~text  =
     let old_lines = indexed_lines text  in 
     let new_lines = Image.image (
         fun (k,line) -> 
           if (k<i)||(k>j)
           then line
          else (String.make indent ' ')^(Cull_string.trim_spaces_on_the_left line)
     ) old_lines in 
     String.concat "\n" new_lines ;;
   
   let impose_fixed_indentation_in_interval_in_file ~indent (i,j) fn =
     let old_text=Io.read_whole_file fn in
     let new_text=change_indentation_in_interval_in_text ~indent (i,j) ~text:old_text   in
     Io.overwrite_with fn new_text;;     
 
    let indentation_decomposition line =
        let n = String.length line in 
        match List.find_opt(fun j->
            not(List.mem (String.get line (j-1)) [' ';'\t']) 
          )(Int_range.range 1 n) with 
        None -> (line,"")
        |Some (j0) -> (Cull_string.beginning (j0-1) line,Cull_string.cobeginning (j0-1) line) ;;

    (*
    
    indentation_decomposition "abc";;
    indentation_decomposition "\t \tabc";;

    *)


    let shift_indentation_in_line line ~shift_amplitude ~forced =
        if shift_amplitude>=0 
        then (String.make shift_amplitude ' ')^line 
        else 
        let positive_amplitude = (-shift_amplitude) 
        and (indent,bare_text) = indentation_decomposition line in 
        let m = String.length indent in 
        if m>=positive_amplitude
        then (Cull_string.cobeginning positive_amplitude indent)^bare_text
        else     
        if forced 
        then bare_text 
        else raise(Shift_indentation_in_line_exn(shift_amplitude,line)) ;;  




    let shift_indentation_in_interval_in_text_with (i,j) ~text ~shift_amplitude ~forced =
      let old_lines = indexed_lines text  in 
      let new_lines = Image.image (
          fun (k,line) -> 
            if (k<i)||(k>j)
            then line
           else shift_indentation_in_line line ~shift_amplitude ~forced
      ) old_lines in 
      String.concat "\n" new_lines ;;
  
  (* ident_interval_in_text_with (2,5) ~text:"1\n2\n3\n4\n5\n6\n7\n" ~tab_width:3;; *)
  
  let shift_indentation_in_interval_in_file_with (i,j) fn ~shift_amplitude ~forced=
     let old_text=Io.read_whole_file fn in
     let new_text=shift_indentation_in_interval_in_text_with (i,j) ~text:old_text ~shift_amplitude ~forced  in
     Io.overwrite_with fn new_text;;     

  let occurrences_of_in_at_beginnings_of_lines patt text = 
    let temp1 = enhance (indexed_lines text) in 
    List.filter_map (
       fun (c_idx,l_idx,line) ->
          if String.starts_with ~prefix:patt line  
          then Some(c_idx,l_idx)
          else None
    ) temp1 ;;  

  (*

  let patt1 = "Bart" ;;

  let text1 = "Uv\nBart45\nJBart6\nBartAgain" ;;
     
  let z1 = occurrences_of_in_at_beginnings_of_lines patt1 text1 ;;

  let check_z1 = List.for_all (fun (c_idx,l_idx) -> 
        Substring.is_a_substring_located_at patt1 text1 c_idx
  ) z1 ;;


  *)  

  let put_line_first_in_text line_idx text = 
    if line_idx=1 then text else
    let lines = indexed_lines text in 
    match List.assoc_opt line_idx lines with 
    None -> raise (Put_line_first_bad_line_index_exn (line_idx,List.length lines))
    |Some(the_line) ->
    let lines2 = List.filter_map
       (fun (idx,old_line)->
        if idx=line_idx 
        then None 
        else Some old_line) lines in 
    String.concat "\n" (the_line::lines2) ;;

  (* put_line_first_in_text 4 "1\n2\n3\n4\n5" ;; *)

  let put_line_last_in_text line_idx text = 
    let lines = indexed_lines text in 
    let n = List.length lines in 
    if line_idx=n then text else
    match List.assoc_opt line_idx lines with 
    None -> raise (Put_line_last_bad_line_index_exn (line_idx,n))
    |Some(the_line) ->
    let lines2 = List.filter_map
      (fun (idx,old_line)->
       if idx=line_idx 
       then None 
       else Some old_line) lines in 
    String.concat "\n" (lines2@[the_line]) ;;
  
  (*  put_line_last_in_text 4 "1\n2\n3\n4\n5" ;; *)
    
  let put_line_first_in_file line_idx src_file  =
    let old_text = Io.read_whole_file src_file  in 
    let new_text = put_line_first_in_text line_idx  old_text in 
    Io.overwrite_with src_file new_text ;; 

  let put_line_last_in_file line_idx src_file  =
    let old_text = Io.read_whole_file src_file  in 
    let new_text = put_line_last_in_text line_idx  old_text in 
    Io.overwrite_with src_file new_text ;; 

  type situation = 
    Inside_a_single_quoted_string 
   |Inside_a_double_quoted_string 
   |Inside_a_starry_comment
   |Inside_a_double_slash_comment 
   |Outside_comments_or_strings ;;  
  

  (* Data type to compute whether 
  the next  linebreak is in a comment or not. 
  The computation returns a pair made of
  the next linebreak's index, with a boolean
      indicating if the linebreak is in a comment.
  *)

  type walker = {
      answer_opt : (int * bool) option ;
      next_idx : int ;
      current_state : situation ;
      text : string ;
      text_length : int;
  } ;;

  let result_from_walker_opt w = 
     match w.answer_opt with 
     (Some answer) -> Some(Some answer) 
     | None -> 
     if w.next_idx > w.text_length
     then Some None 
     else None ;;

  let step w =
   let old_idx = w.next_idx in  
   let c = Strung.get w.text old_idx in 
   match w.current_state with 
    Inside_a_single_quoted_string -> 
       if c='\n' then raise(Unfinished_single_quoted_string) else
       if c = '\'' 
       then { w with 
              next_idx = old_idx +1;
              current_state = Outside_comments_or_strings;
            }
       else 
        let coming_idx = (
          if (Substring.is_a_substring_located_at "\\\\" w.text old_idx)
             || (Substring.is_a_substring_located_at "\\'" w.text old_idx) 
          then old_idx+2
          else old_idx+1  ) in  
           { w with 
              next_idx = coming_idx
            }       
   |Inside_a_double_quoted_string -> 
       if c='\n' then raise(Unfinished_double_quoted_string) else
       if c = '"' 
       then { w with 
              next_idx = old_idx +1 ;
              current_state = Outside_comments_or_strings;
            }
       else 
       let coming_idx = (
          if (Substring.is_a_substring_located_at "\\\\" w.text old_idx)
            || (Substring.is_a_substring_located_at "\\\n" w.text old_idx)
            || (Substring.is_a_substring_located_at "\\\"" w.text old_idx) 
          then old_idx+2
          else old_idx+1  ) in  
          { w with 
              next_idx = coming_idx
            } 
          
   |Inside_a_starry_comment -> 
      if c = '\n' then {w with answer_opt = Some (old_idx,true)} else    
      (* here we use the fact that /* */-comments cannot be nested in C *)
      let (next_situation,coming_idx)=
       (if Substring.is_a_substring_located_at "*/" w.text old_idx 
      then (Outside_comments_or_strings,old_idx+2) 
      else (Inside_a_starry_comment,old_idx+1)) in   
          { w with 
              next_idx = coming_idx;
              current_state = next_situation;
            }  
   |Inside_a_double_slash_comment -> 
      if c = '\n' then {w with answer_opt = Some (old_idx,false)} else 
      { w with next_idx = old_idx+1;}           
   |Outside_comments_or_strings ->
       if c = '\n' then {w with answer_opt = Some (old_idx,false)} else 
       if c = '\'' 
       then { w with 
              next_idx = old_idx+1;
              current_state = Inside_a_single_quoted_string;
            }  
       else      
       if c = '"' 
       then { w with 
              next_idx = old_idx+1;
              current_state = Inside_a_double_quoted_string;
            }  
       else    
       if Substring.is_a_substring_located_at "/*" w.text old_idx
       then { w with 
              next_idx = old_idx+2;
              current_state = Inside_a_starry_comment;
            } 
       else     
       if Substring.is_a_substring_located_at "//" w.text old_idx
       then { w with 
              next_idx = old_idx+2;
              current_state = Inside_a_double_slash_comment;
            }  
       else { w with 
              next_idx = old_idx+1
            }        
      ;;      

let rec iterate w = 
   match result_from_walker_opt w with 
    Some res -> res 
    | None -> iterate (step w) ;;

let initial_walker txt idx unfinished_comment=
   {
      answer_opt = None ;
      next_idx = idx ;
      current_state = (
        if unfinished_comment 
        then Inside_a_starry_comment 
        else Outside_comments_or_strings);
      text = txt ;
      text_length = String.length txt;
  } ;;

let next_newline_inside_or_outside_cee_comments_opt 
  txt idx unfinished_comment=
  let w = initial_walker txt idx unfinished_comment in 
  iterate w;;



let rec helper_for_lines_inside_or_outside_cee_comments 
  (whole_text,total_length,treated_lines,next_idx_to_be_treated,unfinished_comment)= 
  if next_idx_to_be_treated > total_length 
  then List.rev treated_lines 
  else 
  match next_newline_inside_or_outside_cee_comments_opt  
        whole_text next_idx_to_be_treated unfinished_comment with
  None -> 
      let rest_of_text = 
        Cull_string.cobeginning (next_idx_to_be_treated-1) whole_text  in 
      List.rev ((rest_of_text,unfinished_comment) :: treated_lines) 
  |Some(newline_idx,unfinished_comment2) ->
     let line= Cull_string.interval whole_text next_idx_to_be_treated (newline_idx-1) in 
     helper_for_lines_inside_or_outside_cee_comments 
  (whole_text,total_length,(line,unfinished_comment)::treated_lines,newline_idx+1,unfinished_comment2);; 

let lines_inside_or_outside_cee_comments text = 
  helper_for_lines_inside_or_outside_cee_comments 
  (text,String.length text,[],1,false) ;;

(*  


 let txt1 = String.concat "\n" [
   "1 When";"2 The "; "3 /* Saints"; "4 Go" ; "5 Marching */ In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments txt1 ;; 

let txt2 = String.concat "\n" [
   "1 When";"2 The "; "3 '\\' /* Saints */"; "4 Go" ; "5 Marching In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments txt2 ;;

let txt3 = String.concat "\n" [
   "1 When";"2 The "; "3 \"/*\" Saints"; "4 Go" ; "5 Marching \"*/\" In"; "6 Oh"
   ]  ;; 

lines_inside_or_outside_cee_comments txt3 ;; 

*)
  
  let modify_interval_inside_text f text i j =
     let (before,itv,after) = tripartition_associated_to_interval text i j in 
     let new_itv = f itv in 
     before ^ new_itv ^ after ;;

  let modify_interval_inside_file f src_file i j =
    let old_text = Io.read_whole_file src_file  in 
    let new_text = modify_interval_inside_text f old_text i j in 
    Io.overwrite_with src_file new_text ;;    

  end ;;   


  let closeup_around_index = Private.closeup_around_index ;;
  let copy_interval_from_file_to_file = Private.copy_interval_from_file_to_file ;;
  let copy_interval_from_text_to_text = Private.copy_interval_from_text_to_text ;; 

  let duplicate_interval_in_file = Private.duplicate_interval_in_file ;;
  let duplicate_interval_in_text = Private.duplicate_interval_in_text ;;

  let enhanced_indexed_lines s= Private.enhance (Private.indexed_lines s);;
  
  (*
  
  enhanced_indexed_lines "a\nb";;
  enhanced_indexed_lines "\na\nb";;
  enhanced_indexed_lines "a\nb\n";;
  
  *)

  let impose_fixed_indentation_in_interval_in_file = Private.impose_fixed_indentation_in_interval_in_file ;;   


  let indexed_lines = Private.indexed_lines ;;
  
  (*
  
  indexed_lines "a\nb";;
  indexed_lines "\na\nb";;
  indexed_lines "a\nb\n";;
  
  *)

  

let interval = Private.interval ;;

   let line_index_from_char_index text char_idx=
      1+(Private.number_of_lines_in_char_interval text 1 char_idx);;

  let lines text= Image.image snd (indexed_lines text);;

  let lines_inside_or_outside_cee_comments = Private.lines_inside_or_outside_cee_comments ;; 

  let modify_interval_inside_file = Private.modify_interval_inside_file ;;

  let modify_interval_inside_text = Private.modify_interval_inside_text ;;
  let occurrences_of_in_at_beginnings_of_lines = Private.occurrences_of_in_at_beginnings_of_lines ;; 

  let put_line_first_in_file = Private.put_line_first_in_file ;; 

  let put_line_first_in_text = Private.put_line_first_in_text ;; 

  let put_line_last_in_file = Private.put_line_last_in_file ;; 
  
  let put_line_last_in_text = Private.put_line_last_in_text ;; 

  let remove_interval text i j=
    let old_indexed_lines=indexed_lines text in
    let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) old_indexed_lines  in
    let new_indexed_lines=Image.image snd temp2 in
    String.concat "\n" new_indexed_lines;; 
  
  let remove_interval_in_file fn i j=
      let old_text=Io.read_whole_file fn in
      let new_text=remove_interval old_text i j  in
     Io.overwrite_with fn new_text;;   
  
  let remove_lines_containing_substring_in_text pattern text =
     let old_indexed_lines=indexed_lines text in
     let temp2=List.filter (fun (_,line)->not(Substring.is_a_substring_of pattern line)) old_indexed_lines  in
     let new_indexed_lines=Image.image snd temp2 in
     String.concat "\n" new_indexed_lines;; 
   
   let remove_lines_containing_substring_in_file pattern fn=
       let old_text=Io.read_whole_file fn in
       let new_text=remove_lines_containing_substring_in_text pattern old_text  in
      Io.overwrite_with fn new_text;;   
  
let findreplace_in_interval (x,y) s i j=
      let (part1,old_part2,part3) = Private.tripartition_associated_to_interval s i j in 
      let new_part2 = Replace_inside.replace_inside_text (x,y) old_part2 in 
      part1^new_part2^part3 ;; 

let findreplace_in_interval_in_file (x,y) fn i j=
      let old_text=Io.read_whole_file fn in
      let new_text=findreplace_in_interval (x,y) old_text i j  in
      Io.overwrite_with fn new_text;;     
  

(* replace_in_interval ("\n"," ") "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)

let shift_indentation_in_interval_in_file_with = Private.shift_indentation_in_interval_in_file_with ;;
let shift_indentation_in_interval_in_text_with = Private.shift_indentation_in_interval_in_text_with ;;

let suppress_linebreaks_in_interval text i j=
    let (part1,old_part2,part3) = Private.tripartition_associated_to_interval text i j in 
    let new_part2 = String.concat "" (lines old_part2) in 
    part1^new_part2^part3 ;; 
  
  (* suppress_linebreaks_in_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
  
let suppress_linebreaks_in_interval_in_file fn i j=
    let old_text=Io.read_whole_file fn in
    let new_text=suppress_linebreaks_in_interval old_text i j  in
    Io.overwrite_with fn new_text;;     

let tripartition_associated_to_interval = Private.tripartition_associated_to_interval ;;    

