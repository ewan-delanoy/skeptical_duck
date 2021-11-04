(*

#use"lines_in_string.ml";;

*)


module Private = struct 

  let lines old_s=
     let left_offset=(if Supstring.begins_with old_s "\n" then "\n" else "")
     and right_offset=(if Supstring.ends_with old_s "\n" then "\n" else "") in
     let s=left_offset^old_s^right_offset in
     Str.split (Str.regexp_string "\n") s ;;

  let core text=
     Ennig.index_everything (lines text);;
  
  let rec iterator_for_enchancement (num_of_treated_chars,treated_lines,lines) =
       match lines with 
       [] -> List.rev treated_lines 
       |(line_idx,line) :: other_lines ->
        iterator_for_enchancement 
        (num_of_treated_chars+(String.length line)+1,
         (num_of_treated_chars,line_idx,line)::treated_lines,other_lines)   ;;
        
  let enhance indexed_lines =  iterator_for_enchancement (0,[],indexed_lines );;      
  
  let adjust_num_of_lines_upwards_in_string ~required_size text =
      let temp1 = lines text in  
      let d = required_size - (List.length temp1) in 
      if d<=0 
      then text 
      else text ^ (String.make d '\n') ;;   

  let adjust_num_of_lines_upwards_in_file ~required_size file =
      let old_text = Io.read_whole_file file in
      let new_text = adjust_num_of_lines_upwards_in_string ~required_size old_text  in
      Io.overwrite_with file new_text ;;   

  let tripartition_associated_to_interval s i j=
      let temp2=lines s in 
      let (temp3,temp4)=Listennou.big_rht (i-1) temp2 in 
      let part1=String.concat "\n" (List.rev temp3) in 
      let (temp5,temp6)=Listennou.big_rht (j-i+1) temp4 in 
      let part2=String.concat "\n" (List.rev temp5) in 
      let part3=String.concat "\n" temp6 in 
      (part1^"\n",part2,"\n"^part3);;
   
   (* tripartition_associated_to_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
       
  let interval text i j=
    let temp1=core text in
    let temp2=List.filter (fun (k,_)->(i<=k)&&(k<=j)) temp1  in
    let temp3=Image.image snd temp2 in
    String.concat "\n" temp3;;  
      
  (* interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
    
  let copy_interval_from_string_to_string (i,j) src dest =
     let src_linelength = List.length (lines src) in 
     let temp1 = adjust_num_of_lines_upwards_in_string ~required_size:src_linelength dest in
     let (before,in_between,after) = tripartition_associated_to_interval temp1 i j in 
     before^(interval src i j)^after;;

      
  (* copy_interval_from_string_to_string (2,5) "1\n2\n3\n4\n5\n6\n7\n" "a\nb\nc";; *)

  let copy_interval_from_file_to_file (i,j) src_file dest_file =
     let src = Io.read_whole_file src_file 
     and old_text = Io.read_whole_file dest_file  in 
     let new_text = copy_interval_from_string_to_string (i,j) src old_text in 
     Io.overwrite_with dest_file new_text ;; 
     


  end ;;   
  
  let copy_interval_from_file_to_file = Private.copy_interval_from_file_to_file ;;
  let copy_interval_from_string_to_string = Private.copy_interval_from_string_to_string ;; 

  let enhanced_core s= Private.enhance (Private.core s);;
  
  (*
  
  enhanced_core "a\nb";;
  enhanced_core "\na\nb";;
  enhanced_core "a\nb\n";;
  
  *)
  
  let core = Private.core ;;
  
  (*
  
  core "a\nb";;
  core "\na\nb";;
  core "a\nb\n";;
  
  *)

  
  let indent_interval_in_string_with (i,j) ~text ~tab_width =
    let old_lines = core text 
    and tab = String.make tab_width ' ' in 
    let new_lines = Image.image (
        fun (k,line) -> 
          if (k<i)||(k>j)
          then line
         else tab^line
    ) old_lines in 
    String.concat "\n" new_lines ;;

(* ident_interval_in_string_with (2,5) ~text:"1\n2\n3\n4\n5\n6\n7\n" ~tab_width:3;; *)

let indent_interval_in_file_with (i,j) fn ~tab_width=
   let old_text=Io.read_whole_file fn in
   let new_text=indent_interval_in_string_with (i,j) ~text:old_text ~tab_width  in
   Io.overwrite_with fn new_text;;   

let interval = Private.interval ;;

  let lines s= Image.image snd (core s);;
  
  exception Lines_in_char_range_exn of int*int;;
  
  let number_of_lines_in_char_interval s  i j=
     try (List.length(List.filter (fun k->
         String.get s (k-1)='\n'
     ) (Ennig.ennig i j))) with
     _->raise(Lines_in_char_range_exn(i,j));; 
  
  let line_index_from_char_index s char_idx=
    1+(number_of_lines_in_char_interval s 1 char_idx);;
  
  let remove_interval s i j=
    let temp1=core s in
    let temp2=List.filter (fun (k,_)->(i>k)||(k>j)) temp1  in
    let temp3=Image.image snd temp2 in
    String.concat "\n" temp3;; 
  
  let remove_interval_in_file fn i j=
      let s1=Io.read_whole_file fn in
      let s2=remove_interval s1 i j  in
     Io.overwrite_with fn s2;;   
  
  let remove_lines_containing_substring_in_string pattern text =
     let temp1=core text in
     let temp2=List.filter (fun (_,line)->not(Substring.is_a_substring_of pattern line)) temp1  in
     let temp3=Image.image snd temp2 in
     String.concat "\n" temp3;; 
   
   let remove_lines_containing_substring_in_file pattern fn=
       let old_text=Io.read_whole_file fn in
       let new_text=remove_lines_containing_substring_in_string pattern old_text  in
      Io.overwrite_with fn new_text;;   
  
let suppress_linebreaks_in_interval s i j=
    let (part1,old_part2,part3) = Private.tripartition_associated_to_interval s i j in 
    let new_part2 = String.concat "" (lines old_part2) in 
    part1^new_part2^part3 ;; 
  
  (* suppress_linebreaks_in_interval "1\n2\n3\n4\n5\n6\n7\n" 2 5;; *)
  
let suppress_linebreaks_in_interval_in_file fn i j=
    let s1=Io.read_whole_file fn in
    let s2=suppress_linebreaks_in_interval s1 i j  in
    Io.overwrite_with fn s2;;     

let tripartition_associated_to_interval = Private.tripartition_associated_to_interval ;;    



