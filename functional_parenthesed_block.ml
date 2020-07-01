(*

#use"functional_parenthesed_block.ml";;

Decompose a string into parenthesed blocks.
More general version of Parenthesed_block module : now parentheses are not necessarily constant strings.
Also, parentheses detectors return the parentheses ranges among other things.

*)

type label = string * string ;;
type range = int * int ;;
type addendum = range * range ;;
type parenthesis_detector = (string -> int -> ((int* range) option)) ;;
type parenthesis_triple= label* parenthesis_detector * parenthesis_detector;;
type associator=string;;


type data_for_decomposition={
   mutable partial_result : (((string * addendum) option)*string) list;
   mutable currently_open_pars : (parenthesis_triple list) list;
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
   s i ((lbl,lparen_f,rparen_f):parenthesis_triple)=
       lparen_f s i;;
 
let test_for_right_paren_at_index 
   s i ((lbl,lparen_f,rparen_f):parenthesis_triple)=
      rparen_f s i;;
 
let look_for_left_paren_at_index app s i=
   let rec finder=(fun
    possibilities->match possibilities with
    []->None
    |paren::other_parens->
      (match test_for_left_paren_at_index s i paren with 
        Some(paren_length,addenda) -> Some(paren,paren_length,addenda)
       |None ->finder other_parens )
   ) in
   finder app;;


let process_without_open_pars app  s data=
   match look_for_left_paren_at_index app s data.cursor_location with
     None->(data.cursor_location<-data.cursor_location+1)
    |Some(paren,lparen_length,addenda)->
                let (lbl,lparen,rparen)=paren in
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
               let temp1=List.filter (fun (lbl2,_,_)->(fst lbl2)=(fst lbl)) app in
               (
                data.currently_open_pars<-(temp1::data.currently_open_pars);
                data.cursor_location<-data.cursor_location+lparen_length
               );;
               
let process_with_open_pars app  s data=
  let temp1=List.hd(data.currently_open_pars) 
  and i=data.cursor_location in
  let opt1=Option.find_and_stop (fun paren->
    match (test_for_right_paren_at_index s i paren) with 
    Some(rparen_length,rparen_range)->Some(paren,rparen_length,rparen_range)
    |None ->None ) temp1 in
  if opt1=None
  then process_without_open_pars app  s data
  else 
       let (best_paren,rparen_length,rparen_range)=Option.unpack opt1 in
       let new_list=List.tl(data.currently_open_pars) in
       let _=(
          data.currently_open_pars<-new_list;
        (data.cursor_location<-data.cursor_location+rparen_length)
       ) in
       if new_list<>[]
       then ()
       else let old_start = data.smallest_unprocessed_index in 
            let (old_lparen_length,lparen_range) = 
                Option.unpack(test_for_left_paren_at_index s old_start best_paren) in 
            let i_start=old_start + old_lparen_length
            and i_end=i-1 in
            let enclosed_substring=Cull_string.interval s i_start i_end in
            let ((left_label,right_label),_,_) = best_paren in 
            let new_result=(Some(left_label^right_label,(lparen_range,rparen_range)),enclosed_substring) in
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

let decompose_without_taking_blanks_into_account app s=
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
  final_touch s data;
  List.rev(data.partial_result);;   

module With_associator=struct

   let test_for_associator_at_index  (asc:associator) s i=
     Substring.is_a_substring_located_at asc s i;;

   let process_without_open_pars (asc:associator) app  s data=
   match look_for_left_paren_at_index app s data.cursor_location with
     None->(
           if not(test_for_associator_at_index asc s data.cursor_location)
           then data.cursor_location<-data.cursor_location+1
           else
                (
                  let i_start=data.smallest_unprocessed_index
                  and i_end=data.cursor_location-1 in
                (
                  if i_start<=i_end 
                    then let enclosed_substring=Cull_string.interval s i_start i_end in
                    	 let new_result=(None,enclosed_substring) in
                    	 data.partial_result<-new_result::(data.partial_result)  
                );
                data.cursor_location<-data.cursor_location+String.length(asc);
                data.smallest_unprocessed_index<-data.cursor_location
                )
           )
    |Some(paren,lparen_length,addenda)->
               let (lbl,lparen,rparen)=paren in
               let temp1=List.filter (fun (lbl2,_,_)->(fst lbl2)=(fst lbl)) app in
               data.currently_open_pars<-(temp1::data.currently_open_pars);
               data.cursor_location<-data.cursor_location+lparen_length
               ;;
               
let process_with_open_pars (asc:associator) app  s data=
  let temp1=List.hd(data.currently_open_pars) 
  and i=data.cursor_location in
  let opt1=Option.find_and_stop (fun paren->
    match (test_for_right_paren_at_index s i paren) with 
    Some(rparen_length)->Some(paren,rparen_length)
    |None ->None ) temp1 in
  if opt1=None
  then (
          match look_for_left_paren_at_index app s data.cursor_location with
     	  None->(data.cursor_location<-data.cursor_location+1)
        |Some(paren,lparen_length,addenda)->
               let (lbl,lparen,rparen)=paren in
               let temp1=List.filter (fun (lbl2,_,_)->(fst lbl2)=(fst lbl)) app in
               data.currently_open_pars<-(temp1::data.currently_open_pars);
               data.cursor_location<-data.cursor_location+lparen_length
              
        )
  else (
       let (best_paren,(rparen_length,addenda))=Option.unpack opt1 in
       let new_list=List.tl(data.currently_open_pars) in
       data.currently_open_pars<-new_list;
       data.cursor_location<-data.cursor_location+rparen_length
       );;              

let process asc app s data=
  if data.currently_open_pars=[]
  then process_without_open_pars asc app s data
  else process_with_open_pars asc app s data;;

let final_touch s data=
  let a=data.smallest_unprocessed_index
  and b=String.length s in
  if (a<=b)
  then let new_result=(None,Cull_string.interval s a b) in
       data.partial_result<-new_result::(data.partial_result);;

let decompose_without_taking_blanks_into_account asc app s=
  let data={
  		partial_result=[];
  		currently_open_pars=[];
  		smallest_unprocessed_index=1;
  		cursor_location=1;
  } in
  while data.cursor_location<=(String.length s)
  do
     process asc app s data
  done;
  final_touch s data;
  List.rev_map snd (data.partial_result);;   


end;;

let decompose_with_associator=
  ((With_associator.decompose_without_taking_blanks_into_account):
   associator -> parenthesis_triple list -> string -> string list
  );;

let decompose app s=
  let temp1=decompose_without_taking_blanks_into_account app s in
  let temp2=Option.filter_and_unpack (
     fun (lab,t)->
       let u=Cull_string.trim_spaces t in
       if lab<>None then Some[lab,u] else
       if u="" then None else
       let ttemp1=Str.split(Str.regexp"[ \n\t]+") u in
       Some(Image.image (fun v->(None,v)) ttemp1)
  ) temp1 in
  List.flatten temp2;;

(*

Sample examples :

let basic_detection pattern s  i=
  if Substring.is_a_substring_located_at pattern s i 
  then Some(String.length pattern,(i,i-1+String.length pattern))
  else None ;;

let lc l= Image.image (fun (x,y)->((x,y),basic_detection x,basic_detection y)) l;;

decompose (lc [ ("(",")");("{","}");("BEGIN","END") ])
("How (much (research effort) is {expected} when) BEGIN posting a"^
"Code Review ENDquestion? A "^
"lot. {{(An absurd amount)}}. More BEGIN than  BEGIN you think END"^
"you ENDare capable of.");;

decompose (lc[ ("[","]+");("[","]*");("BEGIN","END") ])
("ijk [abc [def]+ gh]* lm hhh [nop [qr]* stu]+ vw []+ ab  ");;

decompose [ ("[","]") ] "[ab]cd[efg]";;
decompose_without_taking_blanks_into_account [ ("[","]") ] "[ab]cd[efg]";;
decompose_without_taking_blanks_into_account [ ("[","]") ] "uv[ab][efg]";;
decompose_without_taking_blanks_into_account [ ("(",")") ]
"abc((de)|fgh)|ij|kl|||mno";;

decompose_with_associator "|" [ ("(",")") ]
"abc(de|fgh)|ij|kl|||mno";;

decompose_with_associator "|" [ ("(",")") ]
"|abc(de|fgh)|ij|kl|||mno";;

decompose_with_associator "asc" [ ("(",")") ]
"abc(deascfgh)ascijascklascascascmno";;

decompose_with_associator "|" [ ("(",")") ] "123((67)|012)|56|89|||345";;

*)






