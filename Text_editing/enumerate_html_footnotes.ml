(*

#use"Text_editing/enumerate_html_footnotes.ml";;

The main function takes an HTML source text and returns the list of all its footnotes,
in phpbb rather than html form.


*)

exception Unterminated_div of int ;;
exception Unterminated_big_div of int * int;;


module Private = struct

let blanks= [' ';'\n';'\r';'\t'];;

let digits = Ennig.doyle char_of_int 48 57;;


let seek_html_footnote_at_index text i0=
  let part1 = "<div" in 
  if not(Substring.is_a_substring_located_at part1 text i0)
  then None 
  else  
  let i1 = i0 + (String.length part1) in 
  let i2 = Strung.char_finder (fun c->not(List.mem c blanks)) text i1 in 
  if i2 < 1 
  then raise(Unterminated_div(i0))
  else
  let part2 = "id=\"n" in 
  if not(Substring.is_a_substring_located_at part2 text i2)
  then None 
  else  
  let i3 = i2 + (String.length part2) in      
  let i4 = Strung.char_finder (fun c->not(List.mem c digits)) text i3 in 
  if i4 < 1 
  then None
  else
  let idx_in_str_form =  Cull_string.interval text i3 (i4-1) in 
  let footnote_idx = int_of_string(idx_in_str_form) in 
  let part3 = "\"><a href=\"#ln"^idx_in_str_form^"\">("^idx_in_str_form^")</a>" in 
  if not(Substring.is_a_substring_located_at part3 text i4)
  then None 
  else  
  let i5 = i4 + (String.length part3) in  
  let part5="</div>" in 
  let i6 = Substring.leftmost_index_of_in_from part5 text i5 in 
  if i6 < 0 
  then raise(Unterminated_big_div(i0,i5))  
  else 
  Some((i0,i6+(String.length part5)-1),(footnote_idx,Cull_string.interval text i5 (i6-1))) ;;
   
let main text =
   let n = String.length text in   
   let rec tempf =(fun (already_treated,cursor)->
      if cursor > n then List.rev already_treated else 
      match seek_html_footnote_at_index text cursor with 
       (Some((i_start,i_end),(footnote_idx,footnote_body))) ->
         tempf (((i_start,i_end),(footnote_idx,footnote_body))::already_treated,i_end+1) 
      |None ->  tempf(already_treated,cursor+1)  
      ) in 
   tempf([],1) ;;   

end ;;

let main = Private.main ;;

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
    (txt_idx,footnote_idx,footnote_body,next_idx)->footnote_idx) data ;;

let n1 = List.length footnote_indices ;;    

let test2 = (footnote_indices = Ennig.ennig 1 n1) ;;

*)