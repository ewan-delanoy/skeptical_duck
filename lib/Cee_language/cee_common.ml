(*

#use"lib/Cee_language/cee_common.ml";;

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

