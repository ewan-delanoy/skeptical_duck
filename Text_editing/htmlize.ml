(*

#use"Text_editing/htmlize.ml";;

*)

exception Missing_link_or_footnote of int * int * int ;;

let footnote_marker         = ref "nowfeetneto";;
let link_to_footnote_marker = ref "lonktifiitnite";;

module Private = struct 

  let paragraphs txt =
    let temp1 = Partition_string.wrt_paragraphs txt in 
    let temp2 = Image.image (
       fun (quoted_text,whitespace)->
        let new_text =(
            if (quoted_text = "")||
              (Substring.is_a_substring_of (!footnote_marker ) quoted_text)
            then quoted_text 
            else " \n<p>\n"^quoted_text^"\n</p>\n "  
        ) in 
        new_text^whitespace
    ) temp1 in 
    String.concat "" temp2;;

  let deal_with_link_to_footnotes_in_page preceding_count page ranges=
  let temp1 = Ennig.index_everything ranges in 
  let temp2 = Image.image (fun (k,(i,j))->
    let sk = string_of_int(k+preceding_count) in 
    let msg = "<span id=\""^"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>" in 
      ((i,j),msg) ) temp1 in 
  Strung.replace_ranges_in temp2 page ;;      

  let deal_with_footnotes_in_page preceding_count page ranges=
    let temp1 = Ennig.index_everything ranges in 
    let temp2 = Image.image (fun (k,(i,j))->
      let sk = string_of_int(k+preceding_count) in 
      let left = (if k=1 then "" else "\n</div>\n")
      and right = "<div id=\""^"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a> </div>" in
      let msg = left^right in
        ((i,j),msg) ) temp1 in 
    (Strung.replace_ranges_in temp2 page)^"\n</div>\n";;     

let error_handling_ref = ref None ;;

  let treat_all_in_page footnote_count page_number page=
    let page1 = paragraphs page in 
    let lfm = (!link_to_footnote_marker) 
    and fm = (!footnote_marker) in 
    let ranges_for_lfm = Substring.ranges_for_occurrences_of_in lfm page1 in 
    let page2 = deal_with_link_to_footnotes_in_page footnote_count page1 ranges_for_lfm in 
    let ranges_for_fm = Substring.ranges_for_occurrences_of_in fm page2 in
    let n_lfm = List.length ranges_for_lfm 
    and n_fm = List.length ranges_for_fm in 
    if n_lfm <> n_fm 
    then let _=(error_handling_ref:=Some(page1,page2,ranges_for_lfm,ranges_for_fm)) in 
         raise (Missing_link_or_footnote(page_number,n_lfm,n_fm) ) 
    else  
    let page3 = deal_with_footnotes_in_page footnote_count page2 ranges_for_fm in   
    let sp = string_of_int page_number in 
    let page4 = "<div id=p"^sp^">\n"^page3^"\n</div>\n"  in 
    (footnote_count+n_fm,page4) ;;  


  let rec iterator_for_pages 
    (already_treated,footnote_count,page_count,to_be_treated) = 
    match to_be_treated with 
    [] -> List.rev already_treated 
    |page :: other_pages ->
      let (new_footnote_count,treated_page) 
        = treat_all_in_page footnote_count (page_count+1) page in 
      iterator_for_pages 
        (treated_page::already_treated,new_footnote_count,page_count+1,other_pages)  ;;

  let pages page_contents =  
   String.concat "\n" (iterator_for_pages ([],0,0,page_contents)) ;;
   
  (*

    pages [
    "abc\n\n a lonktifiitnite b def\n \n zy lonktifiitnite f"^
    "\n\n nowfeetneto gh nowfeetneto ijk   ";
    "abc\n\n c lonktifiitnite d def\n \n zy lonktifiitnite g"^
    "\n\n nowfeetneto gh nowfeetneto ijk   "
    ] ;;
  *)




end ;;

let paragraphs = Private.paragraphs ;;
let pages = Private.pages ;;


