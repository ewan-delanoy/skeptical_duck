(*

#use"lib/Text_editing/percent_pagination.ml";;

Page number i is preceded by three lines, each starting with a percent. 
The middle line also contains the mention "Page i" after its percent. 

*)

exception No_pages_to_extract ;; 
exception No_percent_block_present ;;   
exception Page_number_not_followed_by_a_space ;;

module Private = struct

let test_for_percent_block list_of_lines = 
    if List.length(list_of_lines)<3 then None else 
    let (rev_left,right) = List_again.long_head_with_tail 3 list_of_lines in 
    if List.for_all (fun (_j,line)->String.starts_with ~prefix:"%" line) rev_left  
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
    let indexed_lines = Lines_in_text.indexed_lines text in 
    let (_before_block1,block1,lines_in_page1,lines_after_page1) = 
    towards_first_page_extraction indexed_lines in 
    (re_merge (block1@lines_in_page1), re_merge lines_after_page1) ;; 

(*    
extract_first_page_and_remerge "A\n%\n%1\n%\nB";;    
extract_first_page_and_remerge "A\n%\n%1\n%\nB\n%\n%2\n%\nC\n%\n%3\n%\nD";;

*)

let extract_page_number_from_percent_block three_lines =
  let (_,line_containing_pagenumber) = List.nth three_lines 1 in  
  if String.get(line_containing_pagenumber)(String.length(line_containing_pagenumber)-1)<>' '
  then raise(Page_number_not_followed_by_a_space)
  else          
  int_of_string(Cull_string.two_sided_cutting ("% Page "," ") line_containing_pagenumber);;  


let read_number_of_first_page text =
  let indexed_lines = Lines_in_text.indexed_lines text in 
  let (_before_block1,block1,_lines_in_page1,_lines_after_page1) = 
    towards_first_page_extraction indexed_lines in 
    extract_page_number_from_percent_block block1 ;;  


(*    
 
read_page_number
"A\n%\n% Page 7 \n%\nB\nC\nD\n%\n% Page 8 \n%\nE\nF\nG\n%\n% Page 9 \n%\nH\nI\nJ";;

*)

let rec helper_for_page_extraction verbose (treated,current_percent_block,to_be_treated) = 
  match seek_next_percent_block ([],to_be_treated) with 
  None -> List.rev((current_percent_block,to_be_treated)::treated)
  |Some(before_block1,block1,after_block1) -> 
    let sk = string_of_int(extract_page_number_from_percent_block block1) in 
    let _ = (if verbose then print_string ("Page "^sk^" extracted.\n");flush stdout) in 
    helper_for_page_extraction verbose ((current_percent_block,before_block1)::treated,block1,after_block1) ;;


let extract_all_pages_in_lined_form verbose text =
  let indexed_lines = Lines_in_text.indexed_lines text in 
  match seek_next_percent_block ([],indexed_lines) with 
      None -> raise No_pages_to_extract
     |Some(_before_block1,block1,after_block1) -> 
      let _ = (if verbose then print_string "Starting to extract the pages ... \n\n\n";flush stdout) in 
      let answer = helper_for_page_extraction verbose ([],block1,after_block1) in 
      let _ = (if verbose then print_string "Page extraction ended. \n\n\n";flush stdout) in 
      answer;;

(*    
 
extract_all_pages_in_lined_form 
"A\n%\n% Page 1\n%\nB\nC\nD\n%\n% Page 2\n%\nE\nF\nG\n%\n% Page 3\n%\nH\nI\nJ";;

*)

let extract_all_pages verbose text =
    let temp1 = extract_all_pages_in_lined_form verbose text in 
    Image.image (
      fun (percent_block,lines_in_page) ->
        (extract_page_number_from_percent_block percent_block,
         re_merge lines_in_page)
    ) temp1 ;;

(*    
 
extract_all_pages 
"A\n%\n% Page 5 \n%\nB\nC\nD\n%\n% Page 6 \n%\nE\nF\nG\n%\n% Page 7 \n%\nH\nI\nJ";;

*)

let merge_all_pages pairs =
  let temp1 = Image.image(
    fun (page_nbr,page_content) -> 
      "\n%\n% Page "^(string_of_int page_nbr)^" \n%\n"^page_content
  ) pairs in 
  String.concat "\n" temp1 ;;

(*

let text1 = "A\n%\n% Page 5 \n%\nB\nC\nD\n%\n% Page 6 \n%\nE\nF\nG\n%\n% Page 7 \n%\nH\nI\nJ";;
let pairs1 = extract_all_pages true text1 ;;
let text2 = merge_all_pages pairs1 ;;

*)  


let modify_string_pagewise f old_text = 
  let indexed_pages =extract_all_pages true old_text in 
  String.concat "\n" (Image.image (
  fun (page_nbr,old_text) ->
     "\n%\n% Page "^(string_of_int page_nbr)^" \n%\n" ^ (f old_text)
) indexed_pages );; 


(*    
 
modify_string_pagewise (Replace_inside.replace_inside_text ("\n","_"))
"A\n%\n% Page 5 \n%\nB\nC\nD\n%\n% Page 6 \n%\nE\nF\nG\n%\n% Page 7 \n%\nH\nI\nJ";;

*)

end ;; 

let extract_all_pages ?(verbose=true) text = Private.extract_all_pages verbose text ;;
let extract_first_page_in_string = Private.extract_first_page_and_remerge ;; 
let extract_first_page_in_file file = Private.extract_first_page_and_remerge (Io.read_whole_file file) ;; 

let modify_file_pagewise f file = 
  let old_text = Io.read_whole_file file in
  let new_text = Private.modify_string_pagewise f old_text  in
  Io.overwrite_with file new_text ;;  

let merge_all_pages = Private.merge_all_pages ;;  
let modify_string_pagewise = Private.modify_string_pagewise ;;
let read_number_of_first_page = Private.read_number_of_first_page ;;

