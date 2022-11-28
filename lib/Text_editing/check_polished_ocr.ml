(*

#use"lib/Text_editing/check_polished_ocr.ml";;

Checks if footnote and page numbering is consistent.
A footenote number has format [b][color=blue](<Number>)[/color][/b]  
A footnote has format [size=90][b][color=blue](<Number>)[/color][/b]<Text>[/size]

*)


exception Footnote_inconsistency ;;


module Private = struct 

let is_a_digit c = List.mem c 
    ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

let is_not_a_digit c = not(is_a_digit c) ;;

let seek_positive_integer_at_index text idx =
    if (idx<0)||(idx > (String.length text)) then None else 
    let c = String.get text (idx-1) in 
    if is_not_a_digit c then None else  
    let j = Strung.char_finder_from is_not_a_digit text idx in 
    let next_idx = (if j=0 then (String.length text)+1 else j) in 
    let written_integer = int_of_string(Cull_string.interval text idx (next_idx-1)) in 
    Some(written_integer,next_idx) ;;   

(*    
seek_positive_integer_at_index "abc725def" 3;;    
seek_positive_integer_at_index "abc725def" 4;;
seek_positive_integer_at_index "abc725def" 5;;
*)



let seek_substring_at_index substr text idx = 
    if Substring.is_a_substring_located_at substr text idx 
    then Some(idx+(String.length substr))
    else None ;;  

let seek_phpbb_footnote_number_at_index text idx =
    match seek_substring_at_index "[b][color=blue](" text idx with 
    None -> None 
    | Some idx2 ->
        (
          match seek_positive_integer_at_index text idx2 with 
           None -> None 
           | Some(written_integer,idx3) -> 
              (
                match seek_substring_at_index ")[/color][/b]" text idx3 with 
                 None -> None  
                 | Some idx4 ->
                  Some(written_integer,idx4)
              )
        ) ;; 

(*    
seek_phpbb_footnote_number_at_index "abc[b][color=blue](40)[/color][/b]def" 3;;    
seek_phpbb_footnote_number_at_index "abc[b][color=blue](40)[/color][/b]def" 4;; 

*)

let size_opening_tag = "[size=90]" ;;
let size_closing_tag = "[/size]" ;; 

let seek_small_sized_paragraph_at_index text idx =
  match seek_substring_at_index size_opening_tag text idx with 
  None -> None 
  | Some idx2 -> 
     let idx3 = Substring.leftmost_index_of_in_from size_closing_tag text idx2 in 
     let snippet_in_between = Cull_string.interval text idx2 (idx3-1) in
     Some(snippet_in_between,idx3+(String.length size_closing_tag)) ;;  

(*    
seek_small_sized_paragraph_at_index "123[size=90]The Bounty[/size]0123" 3;;    
seek_small_sized_paragraph_at_index "123[size=90]The Bounty[/size]0123" 4;;  

*)

let seek_phpbb_footnote_at_index text idx =     
  match seek_substring_at_index size_opening_tag text idx with 
  None -> None 
  | Some idx2 -> 
     (
      match seek_phpbb_footnote_number_at_index text idx2 with 
      None -> None 
      | Some (footnote_number,idx3) -> 
        let idx4 = Substring.leftmost_index_of_in_from size_closing_tag text idx3 in 
        let snippet_in_between = Cull_string.interval text idx3 (idx4-1) in
        Some(footnote_number,snippet_in_between,idx4+(String.length size_closing_tag))
     );;
(*    
seek_phpbb_footnote_at_index "123[size=90][b][color=blue](27)[/color][/b]The Bounty[/size]123" 3;;    
seek_phpbb_footnote_at_index "123[size=90][b][color=blue](27)[/color][/b]The Bounty[/size]123" 4;;   

*)

let pusher_for_footnote_collecting text (references,footnotes,next_idx) =
   match  seek_phpbb_footnote_at_index text next_idx with 
   Some(footnote_nbr,_snippet_in_between,idx2) -> (references,footnote_nbr::footnotes,idx2) 
    | None ->
      (
        match  seek_phpbb_footnote_number_at_index text next_idx with 
        Some(footnoteref_nbr,idx3) -> (footnoteref_nbr::references,footnotes,idx3)
        | None -> (references,footnotes,next_idx+1)   
      ) ;; 

let helper_for_footnote_collecting (text,text_length) =
   let rec tempf = (fun triple -> 
   let (references,footnotes,next_idx) = triple in  
   if next_idx > text_length 
   then (List.rev references,List.rev footnotes) 
   else tempf(pusher_for_footnote_collecting text triple) ) in 
  tempf ;; 

let collect_footnotes text =
  helper_for_footnote_collecting (text,String.length text) ([],[],1) ;;   

(*

collect_footnotes 
("ABC[b][color=blue](27)[/color][/b]DEF[b][color=blue](31)[/color][/b]GHI"^
"[size=90][b][color=blue](31)[/color][/b]JKL[/size]"^
"[size=90][b][color=blue](40)[/color][/b]MNO[/size]") ;;
*)  


let message_for_int_range_completeness_check unordered_numbers =
    let numbers = Ordered.zort Total_ordering.for_integers unordered_numbers in 
    let vmin = List.hd numbers and vmax = List.hd (List.rev numbers) in 
    let full_range = Int_range.range vmin vmax in 
    let missing_numbers = Ordered.setminus Total_ordering.for_integers full_range numbers in 
    let range_descr = "Range "^(string_of_int vmin)^"-"^(string_of_int vmax) in 
    if missing_numbers = []
    then range_descr^" is complete.\n"
    else let missing_descr = String.concat "," (Image.image string_of_int missing_numbers) in 
         "The following pages are missing in "^range_descr^": "^missing_descr^"\n" ;;

let check_range_completeness unordered_numbers = 
   let msg = "\n\n\n"^(message_for_int_range_completeness_check unordered_numbers) in 
   print_string msg;flush stdout ;;         

let message_for_refs_without_notes refs_without_notes =
    if refs_without_notes = [] then "" else 
    let missing_descr = String.concat "," (Image.image string_of_int refs_without_notes) in 
    "The following numbers are referenced but do not correspond to any footnotes : \n"^missing_descr^"\n" ;;

let message_for_refs_notes_without_refs notes_without_refs = 
  if notes_without_refs = [] then "" else 
  let missing_descr = String.concat "," (Image.image string_of_int notes_without_refs) in 
  "The following footnotes are not referenced anywhere : \n"^missing_descr^"\n" ;;   


let print_inconsistency (page_number,refs_without_notes,notes_without_refs) =
    if (refs_without_notes,notes_without_refs)=([],[])
    then ()
    else
      let msg ="On page "^(string_of_int page_number)^" :\n"^
               (message_for_refs_without_notes refs_without_notes)^
               (message_for_refs_notes_without_refs notes_without_refs)^"\n" in  
      print_string msg;flush stdout ;;  

let footnote_inconsistencies numbered_pages = 
  Option.filter_and_unpack (
    fun (page_number,page_content) ->
       let (unordered_refs,unordered_notes) = collect_footnotes page_content in 
       let refs = Ordered.zort Total_ordering.for_integers  unordered_refs 
       and notes = Ordered.zort Total_ordering.for_integers  unordered_notes in 
       let refs_without_notes = Ordered.setminus Total_ordering.for_integers refs notes 
       and notes_without_refs = Ordered.setminus Total_ordering.for_integers notes refs in  
       if (refs_without_notes,notes_without_refs)=([],[])
       then None 
       else Some(page_number,refs_without_notes,notes_without_refs)   
 ) numbered_pages ;;

let check_all text =
    let numbered_pages = Percent_pagination.extract_all_pages text in 
    let _ = check_range_completeness (Image.image fst numbered_pages) in 
    let inconsistencies = footnote_inconsistencies numbered_pages in 
    List.iter print_inconsistency inconsistencies ;; 
    
let check_footnotes_on_page text = 
  let inconsistencies = footnote_inconsistencies [(Percent_pagination.read_number_of_first_page text),text] in 
  let _ = List.iter print_inconsistency inconsistencies in 
  if inconsistencies <> [] 
  then raise Footnote_inconsistency;;  

(*    
 
check_all 
("A\n%\n% Page 1 \n%\nB\nC\nD"^
"\n%\n% Page 2 \n%\n"^
"ABC[b][color=blue](27)[/color][/b]DEF[b][color=blue](31)[/color][/b]GHI"^
"[size=90][b][color=blue](31)[/color][/b]JKL[/size]"^
"[size=90][b][color=blue](40)[/color][/b]MNO[/size]"^
"\n%\n% Page 3 \n%\n"^
"ABC[b][color=blue](72)[/color][/b]DEF[b][color=blue](13)[/color][/b]GHI"^
"[size=90][b][color=blue](13)[/color][/b]JKL[/size]"^
"[size=90][b][color=blue](4)[/color][/b]MNO[/size]"^
"E\nF\nG\n%\n% Page 6 \n%\nH\nI\nJ");;

*)

end ;;

let check_footnotes_on_page = Private.check_footnotes_on_page ;;
let check_pages_and_footnotes = Private.check_all ;;

