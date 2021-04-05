(*

#use"Text_editing/convert_html_footnotes_to_phpbb_footnotes.ml";;

The main function takes an HTML source text and returns the list of all its footnotes,
in phpbb rather than html form.


*)


(*
module Private = struct


let merge_two_lists_indexed_by_disjoint_ranges l1 l2 =
    let rec tempf = (fun (already_treated,part1,part2)->
      
      
    ) in 
    tempf([],l1,l2);;


let write_phpbb_link_to_footnote idx =
    let s_idx = string_of_int idx in 
    "[color=blue]("^s_idx^")[/color][/b]" ;;

let write_phpbb_footnote footnote_idx html_content =
  let s_idx = string_of_int footnote_idx in 
  let phpbb_content = Replace_inside.replace_several_inside_string 
    ["<i>","[i]";"</i>","[/i]";"<b>","[b]";"</b>","[/b]"] html_content in 
  "[size=90][b][color=blue]("^s_idx^")[/color][/b]"^phpbb_content^"[/size]"  ;;

let main text =   
    let temp1 = Enumerate_html_footnotes.main text in 
    let temp2 = Image.image write_phpbb_footnote temp1 in 
    let aggregated_footnotes = "\n\n\n" ^ (String.concat "\n\n\n" temp2) ^ "\n\n\n" in
    (aggregated_footnotes,temp1) ;; 

end ;;

let main = Private.main ;;
*)

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