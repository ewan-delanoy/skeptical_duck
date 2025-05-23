(*

#use"lib/Text_editing/check_ocr.ml";;

Checks if footnotes and page numbering are consistent.
Exists in two versions : Phpbb and Basic.

In phpbb mode :
A footenote number has format [b][color=blue](<Number>)[/color][/b]  
A footnote has format [size=90][b][color=blue](<Number>)[/color][/b]<Text>[/size]

In basic mode, all this reduces to the format (<Number>).

*)


exception Footnote_inconsistency ;;


module Private = struct

module Common = struct 

let is_a_digit c = List.mem c 
    ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'];;

let is_not_a_digit c = not(is_a_digit c) ;;

let seek_positive_integer_at_index text idx =
    if (idx<0)||(idx > (String.length text)) then None else 
    let c = String.get text (idx-1) in 
    if is_not_a_digit c then None else  
    let j_opt = Strung.char_finder_from_inclusive_opt is_not_a_digit text idx in 
    let next_idx = (match j_opt with None -> (String.length text)+1 | Some j -> j) in 
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


let message_for_int_range_completeness_check unordered_numbers =
    let numbers = Ordered.sort Total_ordering.for_integers unordered_numbers in 
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

end ;;

module Phpbb = struct

let seek_phpbb_footnote_number_at_index text idx =
    match Common.seek_substring_at_index "[b][color=blue](" text idx with 
    None -> None 
    | Some idx2 ->
        (
          match Common.seek_positive_integer_at_index text idx2 with 
           None -> None 
           | Some(written_integer,idx3) -> 
              (
                match Common.seek_substring_at_index ")[/color][/b]" text idx3 with 
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
  match Common.seek_substring_at_index size_opening_tag text idx with 
  None -> None 
  | Some idx2 -> 
     let idx3 = Option.get(Substring.leftmost_index_of_in_from_opt size_closing_tag text idx2) in 
     let snippet_in_between = Cull_string.interval text idx2 (idx3-1) in
     Some(snippet_in_between,idx3+(String.length size_closing_tag)) ;;  

(*    
seek_small_sized_paragraph_at_index "123[size=90]The Bounty[/size]0123" 3;;    
seek_small_sized_paragraph_at_index "123[size=90]The Bounty[/size]0123" 4;;  

*)

let seek_phpbb_footnote_at_index text idx =     
  match Common.seek_substring_at_index size_opening_tag text idx with 
  None -> None 
  | Some idx2 -> 
     (
      match seek_phpbb_footnote_number_at_index text idx2 with 
      None -> None 
      | Some (footnote_number,idx3) -> 
        let idx4 = Option.get(Substring.leftmost_index_of_in_from_opt size_closing_tag text idx3) in 
        let snippet_in_between = Cull_string.interval text idx3 (idx4-1) in
        Some(footnote_number,snippet_in_between,idx4+(String.length size_closing_tag))
     );;
(*    
seek_phpbb_footnote_at_index "123[size=90][b][color=blue](27)[/color][/b]The Bounty[/size]123" 3;;    
seek_phpbb_footnote_at_index "123[size=90][b][color=blue](27)[/color][/b]The Bounty[/size]123" 4;;   

*)



let pusher_for_phpbb_footnote_collecting text (references,footnotes,next_idx) =
   match  seek_phpbb_footnote_at_index text next_idx with 
   Some(footnote_nbr,_snippet_in_between,idx2) -> (references,footnote_nbr::footnotes,idx2) 
    | None ->
      (
        match  seek_phpbb_footnote_number_at_index text next_idx with 
        Some(footnoteref_nbr,idx3) -> (footnoteref_nbr::references,footnotes,idx3)
        | None -> (references,footnotes,next_idx+1)   
      ) ;; 

let helper_for_phpbb_footnote_collecting (text,text_length) =
   let rec tempf = (fun triple -> 
   let (references,footnotes,next_idx) = triple in  
   if next_idx > text_length 
   then (List.rev references,List.rev footnotes) 
   else tempf(pusher_for_phpbb_footnote_collecting text triple) ) in 
  tempf ;; 

  



let collect_phpbb_footnotes text =
  helper_for_phpbb_footnote_collecting (text,String.length text) ([],[],1) ;; 

(*

collect_phpbb_footnotes 
("ABC[b][color=blue](27)[/color][/b]DEF[b][color=blue](31)[/color][/b]GHI"^
"[size=90][b][color=blue](31)[/color][/b]JKL[/size]"^
"[size=90][b][color=blue](40)[/color][/b]MNO[/size]") ;;
*)  

let footnote_inconsistencies numbered_pages = 
  List.filter_map (
    fun (page_number,page_content) ->
       let (unordered_refs,unordered_notes) = collect_phpbb_footnotes page_content in 
       let refs = Ordered.sort Total_ordering.for_integers  unordered_refs 
       and notes = Ordered.sort Total_ordering.for_integers  unordered_notes in 
       let refs_without_notes = Ordered.setminus Total_ordering.for_integers refs notes 
       and notes_without_refs = Ordered.setminus Total_ordering.for_integers notes refs in  
       if (refs_without_notes,notes_without_refs)=([],[])
       then None 
       else Some(page_number,refs_without_notes,notes_without_refs)   
 ) numbered_pages ;;

let check_all_pages text =
    let numbered_pages = Percent_pagination.extract_all_pages text in 
    let _ = Common.check_range_completeness (Image.image fst numbered_pages) in 
    let inconsistencies = footnote_inconsistencies numbered_pages in 
    List.iter Common.print_inconsistency inconsistencies ;; 
    
let check_footnotes_on_page text = 
  let inconsistencies = footnote_inconsistencies [(Percent_pagination.read_number_of_first_page text),text] in 
  let _ = List.iter Common.print_inconsistency inconsistencies in 
  if inconsistencies <> [] 
  then raise Footnote_inconsistency;;  

(*    
 
check_all_pages 
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

module Basic = struct 

let separator_announcing_footnotes="% FOOTNOTES BEGIN HERE" ;;
let seek_naive_reference_at_index text idx =     
  match Common.seek_substring_at_index "(" text idx with 
  None -> None 
  | Some idx2 -> 
     (
      match Common.seek_positive_integer_at_index text idx2 with 
           None -> None 
           | Some(written_integer,idx3) -> 
      (match Common.seek_substring_at_index ")" text idx3 with 
      None -> None 
      | Some idx4 -> Some(written_integer,idx4)
     ));;

(*    
seek_naive_reference_at_index "1234(67) go marching in" 5;;    

*)

let pusher_for_naive_reference_collecting text (references,next_idx) =
   match  seek_naive_reference_at_index text next_idx with 
   Some(footnote_nbr,idx2) -> (footnote_nbr::references,idx2) 
    | None ->(references,next_idx+1)   ;; 

let helper_for_naive_reference_collecting (text,text_length) =
   let rec tempf = (fun pair -> 
   let (references,next_idx) = pair in  
   if next_idx > text_length 
   then List.rev references
   else tempf(pusher_for_naive_reference_collecting text pair) ) in 
  tempf ;; 

let collect_naive_references text =
  helper_for_naive_reference_collecting (text,String.length text) ([],1) ;;


let collect_footnotes text = 
  let idx_opt = Substring.leftmost_index_of_in_from_opt 
      separator_announcing_footnotes text 1 in 
  let (main_text,footnotes) = (match idx_opt with 
    None -> (text,[]) 
    |Some idx -> 
      let body = Cull_string.beginning idx text 
      and appendix = Cull_string.cobeginning idx text  in 
      (body,collect_naive_references appendix)
  ) in 
  (collect_naive_references main_text,footnotes) ;;

let footnote_inconsistencies numbered_pages = 
  List.filter_map (
    fun (page_number,page_content) ->
       let (unordered_refs,unordered_notes) = collect_footnotes page_content in 
       let refs = Ordered.sort Total_ordering.for_integers  unordered_refs 
       and notes = Ordered.sort Total_ordering.for_integers  unordered_notes in 
       let refs_without_notes = Ordered.setminus Total_ordering.for_integers refs notes 
       and notes_without_refs = Ordered.setminus Total_ordering.for_integers notes refs in  
       if (refs_without_notes,notes_without_refs)=([],[])
       then None 
       else Some(page_number,refs_without_notes,notes_without_refs)   
 ) numbered_pages ;;

let check_all_pages text =
    let numbered_pages = Percent_pagination.extract_all_pages text in 
    let _ = Common.check_range_completeness (Image.image fst numbered_pages) in 
    let inconsistencies = footnote_inconsistencies numbered_pages in 
    List.iter Common.print_inconsistency inconsistencies ;; 
    
let check_footnotes_on_page text = 
  let inconsistencies = footnote_inconsistencies [(Percent_pagination.read_number_of_first_page text),text] in 
  let _ = List.iter Common.print_inconsistency inconsistencies in 
  if inconsistencies <> [] 
  then raise Footnote_inconsistency;;  


end ;;  

end ;;

let check_basic_footnotes_on_page = Private.Basic.check_footnotes_on_page ;;
let check_basic_footnotes_on_all_pages = Private.Basic.check_all_pages ;;

let check_phpbb_footnotes_on_page = Private.Phpbb.check_footnotes_on_page ;;
let check_phpbb_footnotes_on_all_pages = Private.Phpbb.check_all_pages ;;

let separator_announcing_basic_footer = Private.Basic.separator_announcing_footnotes ;;
