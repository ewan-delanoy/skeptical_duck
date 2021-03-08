(*

#use"Text_editing/trim_text_between_tags.ml";;

The final "trim" function removes any trailing space on the left or right of any
text between the tags.

This module was first used on a phpbb snippet with italics tags ([i] and [/i])


*)

exception Bad_parenthesing of (string*string*int) list ;; 

module Private = struct

type parenthesis_pair=P of string*string;;

type data_for_decomposition={
   mutable partial_result : ((parenthesis_pair option)*string) list;
   mutable currently_open_pars : ((parenthesis_pair*int) list) list;
   mutable smallest_unprocessed_index : int;
   mutable cursor_location : int; 
};;

let initial_data={
  partial_result=[];
  currently_open_pars=[];
  smallest_unprocessed_index=1;
  cursor_location=1;
};;



let test_for_left_paren_at_index 
   s i (P(lparen,rparen))=Substring.is_a_substring_located_at lparen s i;;
 
let test_for_right_paren_at_index 
   s i (P(lparen,rparen))=Substring.is_a_substring_located_at rparen s i;;
 
let look_for_left_paren_at_index app s i=
   let rec finder=(fun
    possibilities->match possibilities with
    []->None
    |paren::other_parens->
      if test_for_left_paren_at_index s i paren
      then Some(paren)
      else finder other_parens
   ) in
   finder app;;
  
let process_without_open_pars app  s data=
   match look_for_left_paren_at_index app s data.cursor_location with
     None->(data.cursor_location<-data.cursor_location+1)
    |Some(paren)->
                let (P(lparen,rparen))=paren in
               let _=(
               if data.currently_open_pars=[]
               then let i_start=data.smallest_unprocessed_index
                    and i_end=data.cursor_location-1 in
                    let _=(data.smallest_unprocessed_index<-data.cursor_location) in
                    if i_start<=i_end 
                    then let enclosed_substring=Cull_string.interval s i_start i_end in
                    	 let new_result=(None,enclosed_substring) in
                    	 data.partial_result<-new_result::(data.partial_result)
               ) in
               let temp1=Option.filter_and_unpack (fun pair->
                  let (P(l_par,r_par)) = pair in 
                  if l_par=lparen
                  then Some(pair,data.cursor_location)
                  else None) app in
               (
                data.currently_open_pars<-(temp1::data.currently_open_pars);
                data.cursor_location<-data.cursor_location+String.length(lparen)
               );;
               
let process_with_open_pars app  s data=
  let temp1=List.hd(data.currently_open_pars) 
  and i=data.cursor_location in
  let opt1=Option.seek (fun (paren,idx)->test_for_right_paren_at_index s i paren) temp1 in
  if opt1=None
  then process_without_open_pars app  s data
  else 
       let (best_paren,_)=Option.unpack opt1 in
       let (P(lparen,rparen))=best_paren 
       and new_list=List.tl(data.currently_open_pars) in
       let _=(
          data.currently_open_pars<-new_list;
        (data.cursor_location<-data.cursor_location+String.length(rparen))
       ) in
       if new_list<>[]
       then ()
       else let i_start=data.smallest_unprocessed_index+String.length(lparen)
            and i_end=i-1 in
            let enclosed_substring=Cull_string.interval s i_start i_end in
            let new_result=(Some(best_paren),enclosed_substring) in
            (
                data.partial_result<-new_result::(data.partial_result);
                data.smallest_unprocessed_index<-data.cursor_location
             )
            ;;              

let process app s data=
  if data.currently_open_pars=[]
  then process_without_open_pars app s data
  else process_with_open_pars app s data;;

let final_touch s data=
  let a=data.smallest_unprocessed_index
  and b=String.length s in
  if (a<=b)
  then let new_result=(None,Cull_string.interval s a b) in
       data.partial_result<-new_result::(data.partial_result);;

let decompose app s=
  let data={
  		partial_result=[];
  		currently_open_pars=[];
  		smallest_unprocessed_index=1;
  		cursor_location=1;
  } in
  while data.cursor_location<=(String.length s)
  do
     process app s data
  done;
  (if data.currently_open_pars <> []
  then let simplified_msg = Image.image (fun l->
       let (P(l_par,r_par),idx)=List.hd l in  
        (l_par,r_par,idx)
       )data.currently_open_pars in 
       raise(Bad_parenthesing(simplified_msg))
  else   final_touch s data;
         List.rev(data.partial_result));;   


let in_string app txt=
   let formal_app = Image.image (fun (l_par,r_par)->P(l_par,r_par)) app in 
   let temp1 = decompose formal_app txt in  
   let temp2 = Image.image (fun (opt,snippet)->
      match opt with 
      None -> snippet 
      |Some(P(l_par,r_par))->l_par^(Cull_string.trim_spaces snippet)^r_par
  ) temp1 in 
  String.concat "\n" temp2 ;;
   
let in_file app ap =
    let old_text = Io.read_whole_file ap in 
    let new_text = in_string app old_text in 
    Io.overwrite_with ap new_text ;;


end ;; 

let in_file = Private.in_file ;;
let in_string = Private.in_string ;;


(*

Sample examples :

decompose [ P("(",")");P("{","}");P("BEGIN","END") ]
("How (much (research effort) is {expected} when) BEGIN posting a"^
"Code Review ENDquestion? A "^
"lot. {{(An absurd amount)}}. More BEGIN than  BEGIN you think END"^
"you ENDare capable of.");;

decompose [ P("[","]") ] "[ab]cd[efg]";;
decompose [ P("[","]") ] "[23][678";;



*)






