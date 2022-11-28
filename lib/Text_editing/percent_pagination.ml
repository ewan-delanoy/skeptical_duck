(*

#use"lib/Text_editing/percent_pagination.ml";;

Page number i is preced by three lines, each starting with a percent. 
The middle line also contains the mention "Page i" after its percent. 

*)

exception No_pages_to_extract ;; 
exception No_percent_block_present ;;   

module Private = struct

let test_for_percent_block list_of_lines = 
    if List.length(list_of_lines)<3 then None else 
    let (rev_left,right) = Listennou.big_rht 3 list_of_lines in 
    if List.for_all (fun (_j,line)->Supstring.begins_with line "%") rev_left  
    then Some(List.rev rev_left,right)
    else None ;;  

let rec seek_next_percent_block (treated,to_be_treated) =
      match test_for_percent_block to_be_treated with 
      (Some(left,right))->
          Some(List.rev treated,left,right)
      |None -> 
        (
          match to_be_treated with 
            [] -> None 
           |already_explored :: others -> seek_next_percent_block (already_explored::treated,others)
        ) ;; 



let towards_first_page_extraction lines =
    match seek_next_percent_block ([],lines) with 
     None -> raise No_percent_block_present
     |Some(before_block1,block1,after_block1) ->
         let (lines_in_page1,lines_after_page1) = (
          match seek_next_percent_block ([],after_block1) with 
          None -> (after_block1,[])
          |Some(before_block2,block2,after_block2) ->  
              (before_block2,block2@after_block2)         
         )  in 
         (before_block1,block1,lines_in_page1,lines_after_page1) ;;        

let re_merge l = String.concat "\n" (Image.image snd l) ;;     

let extract_first_page_and_remerge text =
    let indexed_lines = Lines_in_string.indexed_lines text in 
    let (_before_block1,block1,lines_in_page1,lines_after_page1) = 
    towards_first_page_extraction indexed_lines in 
    (re_merge (block1@lines_in_page1), re_merge lines_after_page1) ;; 

(*    
extract_first_page_and_remerge "A\n%\n%1\n%\nB";;    
extract_first_page_and_remerge "A\n%\n%1\n%\nB\n%\n%2\n%\nC\n%\n%3\n%\nD";;

*)

let extract_page_number_from_percent_block three_lines =
  let (_,line_containing_pagenumber) = List.nth three_lines 1 in        
  int_of_string(Cull_string.two_sided_cutting ("% Page "," ") line_containing_pagenumber);;  


let read_number_of_first_page text =
  let indexed_lines = Lines_in_string.indexed_lines text in 
  let (_before_block1,block1,_lines_in_page1,_lines_after_page1) = 
    towards_first_page_extraction indexed_lines in 
    extract_page_number_from_percent_block block1 ;;  


(*    
 
read_page_number
"A\n%\n% Page 7 \n%\nB\nC\nD\n%\n% Page 8 \n%\nE\nF\nG\n%\n% Page 9 \n%\nH\nI\nJ";;

*)

let rec helper_for_page_extraction (treated,current_percent_block,to_be_treated) = 
  match seek_next_percent_block ([],to_be_treated) with 
  None -> List.rev((current_percent_block,to_be_treated)::treated)
  |Some(before_block1,block1,after_block1) -> 
    helper_for_page_extraction ((current_percent_block,before_block1)::treated,block1,after_block1) ;;


let extract_all_pages_in_lined_form text =
  let indexed_lines = Lines_in_string.indexed_lines text in 
  match seek_next_percent_block ([],indexed_lines) with 
      None -> raise No_pages_to_extract
     |Some(_before_block1,block1,after_block1) ->
      helper_for_page_extraction ([],block1,after_block1) ;;

(*    
 
extract_all_pages_in_lined_form 
"A\n%\n% Page 1\n%\nB\nC\nD\n%\n% Page 2\n%\nE\nF\nG\n%\n% Page 3\n%\nH\nI\nJ";;

*)

let extract_all_pages text =
    let temp1 = extract_all_pages_in_lined_form text in 
    Image.image (
      fun (percent_block,lines_in_page) ->
        (extract_page_number_from_percent_block percent_block,
         re_merge lines_in_page)
    ) temp1 ;;

(*    
 
extract_all_pages 
"A\n%\n% Page 5 \n%\nB\nC\nD\n%\n% Page 6 \n%\nE\nF\nG\n%\n% Page 7 \n%\nH\nI\nJ";;

*)

let modify_string_pagewise f old_text = 
  let indexed_pages =extract_all_pages old_text in 
  let indexed_new_pages = Image.image (fun 
  (page_nbr,old_text) ->
    (page_nbr,f old_text)  
  ) indexed_pages in 
  String.concat "\n" (Image.image (
  fun (page_nbr,old_text) ->
     "\n%\n% Page "^(string_of_int page_nbr)^" \n%\n" ^ (f old_text)
) indexed_new_pages );; 


(*    
 
modify_string_pagewise (Replace_inside.replace_inside_string ("\n","_"))
"A\n%\n% Page 5 \n%\nB\nC\nD\n%\n% Page 6 \n%\nE\nF\nG\n%\n% Page 7 \n%\nH\nI\nJ";;

*)

end ;; 

let extract_all_pages = Private.extract_all_pages ;;

let modify_file_pagewise f file = 
  let old_text = Io.read_whole_file file in
  let new_text = Private.modify_string_pagewise f old_text  in
  Io.overwrite_with file new_text ;;  

let modify_string_pagewise = Private.modify_string_pagewise ;;
let read_number_of_first_page = Private.read_number_of_first_page ;;

