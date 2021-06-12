(*

#use"Text_editing/enumerate_html_links_to_footnotes.ml";;

*)

exception Unterminated_div of int ;;
exception Unterminated_big_div of int * int;;


module Private = struct

let blanks= [' ';'\n';'\r';'\t'];;

let digits = Ennig.doyle char_of_int 48 57;;


let seek_html_link_to_footnote_at_index text i0=
  let part1 = "<span" in 
  if not(Substring.is_a_substring_located_at part1 text i0)
  then None 
  else  
  let i1 = i0 + (String.length part1) in 
  let i2 = Strung.char_finder_from (fun c->not(List.mem c blanks)) text i1 in 
  if i2 < 1 
  then raise(Unterminated_div(i0))
  else
  let part2 = "id=\"ln" in 
  if not(Substring.is_a_substring_located_at part2 text i2)
  then None 
  else  
  let i3 = i2 + (String.length part2) in      
  let i4 = Strung.char_finder_from (fun c->not(List.mem c digits)) text i3 in 
  if i4 < 1 
  then None
  else
  let idx_in_str_form =  Cull_string.interval text i3 (i4-1) in 
  let link_idx = int_of_string(idx_in_str_form) in 
  let part3 = "\"><a href=\"#n"^idx_in_str_form^"\">("^idx_in_str_form^")</a></span>" in 
  if not(Substring.is_a_substring_located_at part3 text i4)
  then None 
  else  
  Some((i0,i4+(String.length part3)-1),link_idx) ;;
   
let enumerate_html_link_to_footnotes text =
   let n = String.length text in   
   let rec tempf =(fun (already_treated,cursor)->
      if cursor > n then List.rev already_treated else 
      match seek_html_link_to_footnote_at_index text cursor with 
       (Some((i_start,i_end),link_idx)) ->
         tempf (((i_start,i_end),link_idx)::already_treated,i_end+1) 
      |None ->  tempf(already_treated,cursor+1)  
      ) in 
   tempf([],1) ;;   

end ;;

let main = Private.enumerate_html_link_to_footnotes ;;

(*

let text1=
 "123<span id=\"ln3\"><a href=\"#n3\">(3)</a></span>   <i>The Nature of the Early "^
 "Church</i> (New York: Charles Scribner’s Sons,\n1941), p. 208.\n\n </div>456"^
 "<div id=\"n2\"><a href=\"#ln2\">(2)</a>   Cf. <i>Orpheus. A History of "^
 "Religions.</i> <span id=\"ln7\"><a href=\"#n7\">(7)</a></span> (New York: The Liveright "^
 "Publishing Co., 1941), pp. 249 f.\n\n \n</div>789";;

let test1 = Private.seek_html_footnote_at_index text1 4 ;;

let (aggr,data) = main text1 ;;

print_string aggr;;


*)