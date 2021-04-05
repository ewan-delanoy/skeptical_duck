(*

#use"Text_editing/html_to_phpbb.ml";;


*)


exception Nondisjoint_ranges of (int * int) * (int * int) ;;

module Private = struct

let translate_usual_tags = Replace_inside.replace_several_inside_string 
["<i>","[i]";"</i>","[/i]";"<b>","[b]";"</b>","[/b]"] ;;

let merge_two_lists_indexed_by_disjoint_ranges l1 l2 =
    let rec tempf = (fun (already_treated,part1,part2)->
      match part1 with 
      [] -> List.rev_append already_treated part2 
      |elt1 :: rest_of_part1 -> 
          (
            match part2 with 
            [] -> List.rev_append already_treated part1 
           |elt2 :: rest_of_part2 -> 
               let ((i1,j1),data1) = elt1 
               and ((i2,j2),data2) = elt2 in 
               if i1=i2 then raise(Nondisjoint_ranges((i1,j1),(i2,j2))) else 
               if i1<i2 then tempf(elt1::already_treated,rest_of_part1,part2)
                        else tempf(elt2::already_treated,part1,rest_of_part2)       
          )
    ) in 
    tempf([],l1,l2);;


let write_phpbb_link_to_footnote idx =
    let s_idx = string_of_int idx in 
    "[color=blue]("^s_idx^")[/color][/b]" ;;

let write_phpbb_footnote footnote_idx content =
  let s_idx = string_of_int footnote_idx in 
  "[size=90][b][color=blue]("^s_idx^")[/color][/b]"^content^"[/size]"  ;;

let main text =   
    let temp1 = Enumerate_html_links_to_footnotes.main text 
    and temp2 = Enumerate_html_footnotes.main text in 
    let temp3 = Image.image (fun (range,i)->(range,(i,None)) ) temp1 
    and temp4 = Image.image (fun (range,(i,content))->(range,(i,Some content)) ) temp2 in 
    let temp5 = merge_two_lists_indexed_by_disjoint_ranges temp3 temp4 in 
    let temp6 = Image.image (
      fun (range,(idx,opt))->let phpbbized_content =(match opt with
          None -> write_phpbb_link_to_footnote idx 
         |Some(content)-> write_phpbb_footnote idx content 
      ) in 
      (range,phpbbized_content)

    ) temp5 in 
    let temp7 = Strung.replace_ranges_in temp6 text in 
    translate_usual_tags temp7;; 


end ;;

let translate = Private.main ;;


(*

let text1=
 "123<div id=\"n1\"><a href=\"#ln1\">(1)</a>   <i>The Nature of the Early "^
 "Church</i> (New York: Charles Scribner’s Sons,\n1941), p. 208.\n\n </div>456"^
 "<div id=\"n2\"><a href=\"#ln2\">(2)</a>   Cf. <i>Orpheus. A History of "^
 "Religions.</i> Translated by Florence Simmonds (New York: The Liveright "^
 "Publishing Co., 1941), pp. 249 f.\n\n \n</div>789";;

let test1 = Private.seek_html_footnote_at_index text1 4 ;;

let (aggr,data) = main text1 ;;

print_string aggr;;

let footnote_indices = 
   Image.image (fun 
    ((footnote_idx,footnote_body),_)->footnote_idx) data ;;

let n1 = List.length footnote_indices ;;    

let test2 = (footnote_indices = Ennig.ennig 1 n1) ;;

*)