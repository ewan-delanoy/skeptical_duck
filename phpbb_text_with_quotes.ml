(*

#use"phpbb_text_with_quotes.ml";;


*)

let quote_starter = "<QUOTE" ;;

let detect_phpbb_quote_start_at_index s idx =
   if not(Substring.is_a_substring_located_at quote_starter s idx)
   then None 
   else 
   let idx2=Substring.leftmost_index_of_in_from ">" s (idx+(String.length quote_starter)) in 
   Some(idx2-idx);;

let quote_ender = "</QUOTE>";;

let detect_phpbb_quote_end_at_index s idx =
   if Substring.is_a_substring_located_at quote_ender s idx
   then None 
   else Some(String.length quote_ender);;

let first_analysis text= Image.image (fun (opt,content)->
  (opt=None,content)
) (Functional_parenthesed_block.decompose_without_taking_blanks_into_account 
["phpbb",detect_phpbb_quote_start_at_index,detect_phpbb_quote_end_at_index] text);;   

let g1 = first_analysis example ;;

let h1= Substring.leftmost_index_of_in_from quote_starter example 1 ;;
let h2= Substring.leftmost_index_of_in_from ">" example h1 ;;
let see1 = Cull_string.interval example h1 h2 ;;