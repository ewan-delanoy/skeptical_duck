(*

#use"phpbb_text_with_quotes.ml";;


*)

module Private = struct 

let quote_starter = "<QUOTE" ;;

let detect_phpbb_quote_start_at_index s idx =
   if not(Substring.is_a_substring_located_at quote_starter s idx)
   then None 
   else 
   let idx2=Substring.leftmost_index_of_in_from ">" s (idx+(String.length quote_starter)) in 
   Some(idx2-idx+1,(idx,idx2));;

let quote_ender = "</QUOTE>";;

let detect_phpbb_quote_end_at_index s idx =
   if Substring.is_a_substring_located_at quote_ender s idx
   then let e = String.length quote_ender in 
        Some(e,(idx,idx+e-1))
   else None ;;

let first_analysis text= Image.image (fun (opt,content)->
  (opt=None,content)
) (Functional_parenthesed_block.decompose_without_taking_blanks_into_account 
[("php","bb"),detect_phpbb_quote_start_at_index,detect_phpbb_quote_end_at_index] text);;   


let rewrite_first_result text (opt,content) = match opt with 
  None -> (None,content) 
  |Some (_,(p1,p2)) -> let (i,j) = p1 in 
                       if j=i+6 then (Some"",content) else 
                       (Some(Cull_string.interval text (i+15) (j-2)),content);;

let first_analysis text= Image.image (rewrite_first_result text)
(Functional_parenthesed_block.decompose_without_taking_blanks_into_account 
[("php","bb"),detect_phpbb_quote_start_at_index,detect_phpbb_quote_end_at_index] text);;   

let rewrite_element (opt,content) = match opt with 
  None -> content 
  |Some(author) ->
     if author="" 
     then "<QUOTE>"^content^"</QUOTE>"
     else "<QUOTE author=\""^author^"\">"^content^"</QUOTE>" ;;
 
let is_very_easy l =
   if List.length(l)<>1 then false else fst(List.hd l)=None ;; 


let seed_state = 1 ;;
let usual_state = 2 ;;

type walker =
  W of int * string * Phpbb_text_with_quotes_t.t list * (string option * string) list 
  * ( string * Phpbb_text_with_quotes_t.t list * (string option * string) list ) list *
  Phpbb_text_with_quotes_t.t list * (string option * string) list *
  Phpbb_text_with_quotes_t.t option ;; 

let the_end answer = 
   W(0,"",[],[],[],[],[],Some answer);;
   
let seed_item (done_in_concat,todo_in_concat) = 
    W(seed_state,"",[],[],[],done_in_concat,todo_in_concat,None);;  

let push_seed_item (done_in_concat,todo_in_concat) = 
  match todo_in_concat with 
    [] -> the_end(Phpbb_text_with_quotes_t.Concatenated (List.rev done_in_concat))
    |(opt,new_elt)::others ->
        match opt with 
        None -> (* if we get here the analysis has already been done *)
                let quick_result = Phpbb_text_with_quotes_t.Atom(new_elt) in 
                seed_item(quick_result::done_in_concat,others)
       |Some(author) ->
              let temp = first_analysis new_elt  in 
              if is_very_easy temp 
              then let quick_result = Phpbb_text_with_quotes_t.Quoted(author,
                          Phpbb_text_with_quotes_t.Atom(new_elt)) in 
                   seed_item(quick_result::done_in_concat,others) 
              else W(usual_state,author,[],temp,[],done_in_concat,others,None);;   

let push_usual_item_in_short_case 
(current_author,done_in_quote,todo_in_quote,pending_quotes,
      done_in_concat,todo_in_concat) = 
  let new_result = Phpbb_text_with_quotes_t.Quoted(current_author,
                          Phpbb_text_with_quotes_t.Concatenated(List.rev done_in_quote)) in
  match pending_quotes with
  [] -> seed_item(new_result::done_in_concat,todo_in_concat) 
  |(author2,done_in_quote2,todo_in_quote2) :: later_quotes ->
     W(usual_state,author2,new_result::done_in_quote2,todo_in_quote2,later_quotes,
      done_in_concat,todo_in_concat,None) ;;

let push_usual_item_in_long_case 
(current_author,done_in_quote,others,pending_quotes,
      done_in_concat,todo_in_concat) (opt,new_elt) =
 match opt with 
 None -> (* if we get here the analysis has already been done *)
     let quick_result = Phpbb_text_with_quotes_t.Atom(new_elt) in 
      W(usual_state,current_author,quick_result::done_in_quote,others,pending_quotes,
                  done_in_concat,todo_in_concat,None)
 |Some(author3) ->
     let temp = first_analysis new_elt  in 
      if is_very_easy temp 
      then let quick_result = Phpbb_text_with_quotes_t.Quoted(author3,
                          Phpbb_text_with_quotes_t.Atom(new_elt)) in 
                   W(usual_state,current_author,quick_result::done_in_quote,others,pending_quotes,
                  done_in_concat,todo_in_concat,None) 
      else W(usual_state,author3,[],temp,(current_author,done_in_quote,others)::pending_quotes,done_in_concat,todo_in_concat,None);;  



let push_usual_item (current_author,done_in_quote,todo_in_quote,pending_quotes,
      done_in_concat,todo_in_concat) = 
       match todo_in_quote with 
    [] -> push_usual_item_in_short_case 
      (current_author,done_in_quote,todo_in_quote,pending_quotes,
      done_in_concat,todo_in_concat)
    | (opt,new_elt)::others -> push_usual_item_in_long_case 
        (current_author,done_in_quote,others,pending_quotes,
      done_in_concat,todo_in_concat) (opt,new_elt) ;;
      
         

let push_item item =
 let 
  (W(state_idx,author,done_in_quote,todo_in_quote,pending_quotes,
      done_in_concat,todo_in_concat,opt_final_result)) = item in 
  if opt_final_result <> None then item else     
  match state_idx with 
   1 ->  push_seed_item (done_in_concat,todo_in_concat)
  |_ ->  push_usual_item (author,done_in_quote,todo_in_quote,pending_quotes,
      done_in_concat,todo_in_concat) ;;

let rec iterate_on_item item =
 let 
  (W(state_idx,author,done_in_quote,todo_in_quote,pending_quotes,
      done_in_concat,todo_in_concat,opt_final_result)) = item in 
  match opt_final_result with 
  Some(final_result) -> final_result 
  |None -> iterate_on_item (push_item item);;

end ;;

let parse text =
   let temp = Private.first_analysis text in 
   if Private.is_very_easy temp 
   then Phpbb_text_with_quotes_t.Atom(text)
   else 
   let seed = Private.seed_item ([],temp) in 
   Private.iterate_on_item seed ;;

let rec unparse = function 
    Phpbb_text_with_quotes_t.Atom(text) -> text 
   |Concatenated l -> String.concat "" (Image.image unparse l)
   |Quoted(author,compound) ->
      let content = unparse compound in 
      if author="" 
      then "<QUOTE>"^content^"</QUOTE>"
       else "<QUOTE author=\""^author^"\">"^content^"</QUOTE>" ;; 

(*

let example = "<r>First quote :<br/>

<QUOTE author=\"John\"><s>[quote=\"John\"]</s> Hello ! <e>[/quote]</e></QUOTE>

An anonymous quote : <QUOTE><s>[quote]</s> At the Savoy <e>[/quote]</e></QUOTE>

And some nested quotes : <QUOTE author=\"Jane\"><s>[quote=\"Jane\"]</s> One 

<QUOTE><s>[quote]</s> Two <e>[/quote]</e></QUOTE> Three <QUOTE author=\"Four\"><s>

[quote=\"Four\"]</s> Five <e>[/quote]</e></QUOTE> <e>[/quote]</e></QUOTE></r>";;


let g1 = first_analysis example ;;
let g2 = Image.image rewrite_element g1 ;;
let check1 = ((String.concat "" g2)=example);;

let g3 = seed_item ([],g1);;
let g4 = parse example ;;
let g5 = unparse g4 ;;
let check2 = (g5 = example) ;; 

let ff = Memoized.small push_item g3;;

*)