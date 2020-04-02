(*

#use"phpbb_text_with_quotes.ml";;


*)

exception Empty_input_in_parser ;;

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

let rec helper_for_parsing = function    
   [] -> raise(Empty_input_in_parser)
   |(treated,to_be_treated) ::others ->
      (
         match to_be_treated with 
         [] -> 
             let new_phpbbtext = Phpbb_text_with_quotes_t.Concatenated(List.rev treated) in 
             (
               match others with 
                [] -> new_phpbbtext
               |(treated2,to_be_treated2)::later ->
                 helper_for_parsing ((new_phpbbtext::treated2,to_be_treated2)::later)
             )
         |(opt,new_elt) :: other_elts ->
            (
              match opt with 
              None ->  let easy_phpbbtext = Phpbb_text_with_quotes_t.Atom(new_elt) in
                 helper_for_parsing ((easy_phpbbtext::treated,to_be_treated) ::others)
              |Some(author) ->   
            ) 
      ) ;;
      

let example = "<r>First quote :<br/>

<QUOTE author=\"John\"><s>[quote=\"John\"]</s> Hello ! <e>[/quote]</e></QUOTE>

An anonymous quote : <QUOTE><s>[quote]</s> At the Savoy <e>[/quote]</e></QUOTE>

And some nested quotes : <QUOTE author=\"John\"><s>[quote=\"John\"]</s> One 

<QUOTE><s>[quote]</s> Two <e>[/quote]</e></QUOTE> Three <QUOTE author=\"Four\"><s>

[quote=\"Four\"]</s> Five <e>[/quote]</e></QUOTE> <e>[/quote]</e></QUOTE></r>";;


let g1 = first_analysis example ;;
let g2 = Image.image rewrite_element g1 ;;
let check1 = ((String.concat "" g2)=example);;

(*
let h1= Substring.leftmost_index_of_in_from quote_starter example 1 ;;
let h2= Substring.leftmost_index_of_in_from ">" example h1 ;;
let see1 = Cull_string.interval example h1 h2 ;;
*)
